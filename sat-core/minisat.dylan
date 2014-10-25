Module: sat-core
Synopsis: 
Author: 
Copyright: 

/* All by itself a clause doesnt make much sense, as it uses a collection of
indexes only valid for a certain <sat-solver> */
define class <clause-it> (<object>)
  slot lits :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define class <sat-solver-it> (<sat-solver>)
end class;


/*
 Extract the variable from a literal
 */
define function lit-to-var (lit :: <integer>) => (var :: <integer>);
  // this is akin to var / 2, gets the variable
  ash(lit, -1);
end;

// FIXME value is 0 or 1, or maybe it should be <boolean>
define function var-value-to-lit (var :: <integer>, value :: <integer>) => (lit :: <integer>);
  assert(value = 1 | value = 0);
  logior(ash(var, 1), value)
end;

/*
 Extract the signal from a literal
 Check if literal is negated (if it has a sign)
 */
define function negated? (lit :: <integer>) => (i :: <integer>)
  logand(lit, 1);
end;


/*
 * this expects variables to be 1-indexed 
 * and the negative of a variable to be -(var-index)
 * we'll turn this into a form where variables are 
 * 0-indexed and literals are given by (var-index * 2); to negate, add 1
 */
define method add-clause (s :: <sat-solver-it>, cnf :: <collection>) => ();
  let c = make(<clause-it>);
  for(cnf-var in cnf) 
    let i = cnf-var;
    // force to positive value
    if (i < 0)
      i := i * -1;
    end if;
    // 0-index
    i := i - 1;
    // transform into our notation
    i := i * 2;
    if (cnf-var < 0)
      i := i + 1;
    end if;
    add!(c.lits, i);
  end for;
  add!(s.clauses, c);
end method;

define method select-lit (c :: <clause-it>) => (i :: <integer>)
  c.lits[0]
end;

define function setup-watchlist(s :: <sat-solver-it>) => (watchlist :: <array>)
  let watchlist = make(<array>, dimensions: list(s.var-count * 2));
  let i = 0;
  while (i < size(watchlist))
    watchlist[i] := make(<deque>);
    i := i + 1;
  end while;
  
  for (clause in s.clauses)
    push-last(watchlist[select-lit(clause)], clause);
  end for;

  watchlist;
end;

define method update-watchlist(s :: <sat-solver-it>, watchlist :: <array>, false_literal :: <integer>, assignments :: <table>) => (r :: <boolean>);
  block (main-ret)

    while (size(watchlist[false_literal]) > 0)
      let clause :: <clause-it> = watchlist[false_literal][0];
      let found-alternative :: <boolean> = #f;

      block (get-out)	
	for (alternative in clause.lits)
	  let var = lit-to-var(alternative); // ash(alternative, -1); 
	  let is-negated = negated?(alternative); // 
	  let has-assignment = element(assignments, var, default: #"none");
	  if (has-assignment = #"none" | has-assignment = logxor(is-negated, 1))
	    found-alternative := #t;
	    
	    // delete first
	    pop(watchlist[false_literal]);
	    
	    // append to end
	    push-last(watchlist[alternative], clause);
	    
	    get-out();
	  end if;
	end for;
      end block;
      
      if (~found-alternative)
	main-ret(#f);
      end if;
    end while;

    main-ret(#t);
  end block
end method;

define class <var-stat> ()
  slot var :: <integer>, init-keyword: var:;
  slot usage :: <integer> = 0;
end;

define function sort-var-stat (a :: <var-stat>, b :: <var-stat>) => (r :: <boolean>)
  a.usage > b.usage
end;

define method solve (o :: <sat-solver-it>) => (r :: false-or(<table>));
  let var-count :: <integer> = o.var-count;

  // Sort variables by usage.
  let stats = make(<array>, dimensions: list(var-count), fill: #f);
  for(i from 0 below var-count)
    stats[i] := make(<var-stat>, var: i);
  end;
  for(clause in o.clauses)
    for(lit in clause.lits)
      let v = lit-to-var(lit);
      stats[v].usage := stats[v].usage + 1;
    end;
  end;
  
  sort!(stats, test: sort-var-stat, stable: #t);
  
  
  /*
  for(s in stats)
    format-out("%=: %=\n", s.var, s.usage);
  end;
  force-out(); */
  
  /*
    assignments: our assignments at the moment
    
    dh: current index we're working with
    d: current variable we're working with

    state: will track which values for which variables we already tried.
           0: nothing has been tried
           1: #f
           2: #t
           3: #f and #t
  */
  let watchlist = setup-watchlist(o);
  let assignments :: <table> = make(<table>);
  
  let dh :: <integer> = 0;
  let d :: <integer> = stats[dh].var;

  let state :: <array> = make(<array>, dimensions: list(var-count), fill: 0); 
  let tried-something :: <boolean> = #f;
  
  block (solve-ret)
    while (#t)

      // dh is a helper index
      assert(dh >= 0 & d <= var-count);
      
      // TODO unit propagation here ???


      if (dh = var-count)
	// All variables are assigned
	// so we will return our assignments
	// we could continue though ...
	solve-ret(assignments);

	dh := dh - 1;
	d := stats[dh].var;
	
	// FIXME undo assigment too, right? python doesnt do it
      else
	// select next var
	d := stats[dh].var;
      end if;

      // d is an actual variable
      assert(d >= 0 & d < var-count);
      
      /*
	try to assign a value; 
	TODO insert heuristics here
	*/
      
      tried-something := #f;
      block (exit-for-block)
	for (a :: <integer> in list(0, 1))
	  
	  // (state[d] >> a) & 1 == 0
	  if (logand(ash(state[d], -a), 1) = 0) 
	    tried-something := #t;

	    // SOLVER.ENQUEUE (?)
	    // state[d] |= (1 << a)
	    state[d] := logior(state[d], ash(1, a));
	    assignments[d] := a;
	    
	    // this is the value we'll try

	    let v = var-value-to-lit(d, a);
	    if (~update-watchlist(o, watchlist, v, assignments))
	      // undo assignment, lets try again
	      remove-key!(assignments, d);
	    else
	      dh := dh + 1;    
	      exit-for-block();
	    end if;		    
	  end if;
	end for;
      end block;

      if (~tried-something)
	if (dh = 0)
	  solve-ret(#f);
	  // no more solutions
	else
	  // backtrack, undo state, assignment and variable
	  state[d] := 0;
	  remove-key!(assignments, d);
	  dh := dh - 1;
	end if;
      end if;
    end while;
  end block;  
end;

// TODO watchlists are not adjusted on backtracking, minisat.pdf says that some constraint systems might need this. see undo lists (page 4, Propagation)

// TODO cleanup variables, sort by activity

// TODO stack of variable assignments (trail) - dont follow variable index -, decide variables

// TODO when conflicts arise, learn

// TODO simplify clauses using boolean algebra (local and globally (?))

// TODO activity

// TODO heuristics:
// - sort variables by usage in the input problem
// - proprocessing: limited applications of resolution steps (wot?)
// - aggressive backtracking, ie, dont jump only 1 var at a time
// - dynamic sort of variables (why?)


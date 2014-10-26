Module: sat-core
Synopsis: 
Author: 
Copyright: 

define class <minisat-stats> (<sat-stats>)
  slot backtracks :: <integer> = 0;
  slot iterations :: <integer> = 0;
  slot failed-assignments :: <integer> = 0;
  slot correct-assignments :: <integer> = 0;
  slot n-tried-something :: <integer> = 0;
  slot var-stats :: <collection>;
  slot clause-stats :: <collection>;
end;

define method print-object (o :: <minisat-stats>, s :: <stream>) => ()
  next-method();
  
  format(s, " Var Stats:\n");
  for(var-stat in o.var-stats)
    format(s, " * %= (u: %=, p: %=, n: %=)\n", var-stat.var, var-stat.usage,
	   var-stat.pos-lit, var-stat.neg-lit);
  end;

  format(s, " Clause Stats:\n");
  for(clause-stat in o.clause-stats)
    format(s, " * %= (e: %=, l: %=, v: %=)\n", clause-stat.clause-idx,
	   clause-stat.no-elements,
	   clause-stat.no-distinct-lits,
	   clause-stat.no-distinct-vars);
  end;
  
  format(s, " backtracks: %=\n", o.backtracks);
  format(s, " iterations: %=\n", o.iterations);
  format(s, " failed assigns: %=\n", o.failed-assignments);
  format(s, " correct assigns: %=\n", o.correct-assignments);
  format(s, " conflicts?: %=\n", o.n-tried-something);
end;

/* All by itself a clause doesnt make much sense, as it uses a collection of
indexes only valid for a certain <sat-solver> */
define class <clause-it> (<object>)
  slot lits :: <stretchy-vector> = make(<stretchy-vector>);
  slot sorted-lits :: <collection>;
end class;

define class <sat-solver-it> (<sat-solver>)
  slot sat-stats :: <sat-stats>;
  
  slot var-stats :: <array>;
  slot sorted-var-stats :: <array>;

  slot clause-stats :: <array>;
end class;

define class <var-stat> ()
  slot var :: <integer>, init-keyword: var:;
  slot usage :: <integer> = 0;
  slot pos-lit :: <integer> = 0;
  slot neg-lit :: <integer> = 0;
end;

define class <clause-stat> ()
  slot clause-idx :: <integer>, init-keyword: clause-idx:;
  slot no-elements :: <integer> = 0;
  slot no-distinct-lits :: <integer> = 0;
  slot no-distinct-vars :: <integer> = 0;
end;

define function sort-var-stat (a :: <var-stat>, b :: <var-stat>) => (r :: <boolean>)
  a.usage > b.usage
end;


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
    // push first
    /* 
    clause.sorted-lits := clause.lits;
    push-last(watchlist[select-lit(clause)], clause);
    */
    
    // push all. weird?
    /*
    clause.sorted-lits := clause.lits;
    for (lit in clause.lits)
      push-last(watchlist[lit], clause);
    end;
    */
    
    // sort literals in clause by usage, get first
    /* */
    clause.sorted-lits := sort(clause.lits, test: method (l1 :: <integer>, l2 :: <integer>) => (r :: <boolean>)
						let v1 = lit-to-var(l1);
						let v2 = lit-to-var(l2);
						
						sort-var-stat(s.var-stats[v1], s.var-stats[v2]);
					      end);
      
    push-last(watchlist[clause.sorted-lits[0]], clause);
    
    /*
    for (lit in clause.sorted-lits)
      push-last(watchlist[lit], clause);
    end;
      */
    
  end;
  
  watchlist;
end;

define function dup-watchlist (watchlist :: <array>) => (duped :: <array>)
  let new-watchlist = copy-sequence(watchlist);
  
  for(i from 0 below size(new-watchlist))
    new-watchlist[i] := copy-sequence(new-watchlist[i]);
  end;
  
  new-watchlist
end;

define method update-watchlist(s :: <sat-solver-it>, watchlist :: <array>, false_literal :: <integer>, assignments :: <table>) => (r :: <boolean>, new-watchlist :: <array>);
  
  let w-watchlist = dup-watchlist(watchlist);
  
  block (main-ret)

    while (size(w-watchlist[false_literal]) > 0)
      let clause :: <clause-it> = w-watchlist[false_literal][0];
      let found-alternative :: <boolean> = #f;

      block (get-out)	
	for (alternative in clause.sorted-lits)
	  let var = lit-to-var(alternative); // ash(alternative, -1); 
	  let is-negated = negated?(alternative); // 
	  let has-assignment = element(assignments, var, default: #"none");
	  if (has-assignment = #"none" | has-assignment = logxor(is-negated, 1))
	    found-alternative := #t;
	    
	    // delete first
	    pop(w-watchlist[false_literal]);
	    
	    // append to end
	    push-last(w-watchlist[alternative], clause);
	    
	    get-out();
	  end if;
	end for;
      end block;
      
      if (~found-alternative)
	main-ret(#f, watchlist);
      end if;
    end while;

    main-ret(#t, w-watchlist);
  end block
end method;

define method solve (o :: <sat-solver-it>) => (r :: false-or(<table>), 
					       stats :: <minisat-stats>);
  let var-count :: <integer> = o.var-count;
  let sat-stats :: <minisat-stats> = make(<minisat-stats>);
  o.sat-stats := sat-stats;

  /*
    Calculate stats on variables
    */
  let stats = make(<array>, dimensions: list(var-count), fill: #f);
  for(i from 0 below var-count)
    stats[i] := make(<var-stat>, var: i);
  end;
  for(clause in o.clauses)
    for(lit in clause.lits)
      let v = lit-to-var(lit);
      stats[v].usage := stats[v].usage + 1;
      if (negated?(lit) = 0)
	stats[v].pos-lit := stats[v].pos-lit + 1;
      else
	stats[v].neg-lit := stats[v].neg-lit + 1;
      end;
    end;
  end;
  
  /*
  Sort by usage
  */
  let sorted-stats = sort(stats, test: sort-var-stat, stable: #t);
  o.var-stats := stats;
  o.sorted-var-stats := sorted-stats;
  sat-stats.var-stats := sorted-stats;

  /*
    Calculate stats on clauses
    */
  let c-stats = make(<array>, dimensions: list(size(o.clauses)), fill: #f);
  for(i from 0 below size(o.clauses))
    c-stats[i] := make(<clause-stat>, clause-idx: i);
  end;
  for(i from 0 below size(o.clauses))
    let clause = o.clauses[i];
    let tmp-info-lits = make(<table>);
    let tmp-info-vars = make(<table>);
    
    for(lit in clause.lits)
      tmp-info-lits[lit] := #t;
      tmp-info-vars[lit-to-var(lit)] := #t;
    end;
    /*
    let tmp-info-lits! = tmp-info-lits.table-vector;
    let tmp-info-vars! = tmp-info-vars.table-vector;
    */
    c-stats[i].no-elements := size(clause.lits);
    c-stats[i].no-distinct-lits := size(tmp-info-lits);
    c-stats[i].no-distinct-vars := size(tmp-info-vars);
  end;
  
  o.clause-stats := c-stats;
  sat-stats.clause-stats := c-stats;
    
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
  
  sat-stats.var-no := var-count;
  sat-stats.clause-no := size(o.clauses);

  format-out("%=\n", sat-stats);
  force-out();
  
  block (solve-ret)
    while (#t)
      sat-stats.iterations := sat-stats.iterations + 1;

      // dh is a helper index
      assert(dh >= 0 & d <= var-count);
      
      if (dh = var-count)
	// All variables are assigned
	// so we will return our assignments
	// we could continue though ...
	solve-ret(assignments, sat-stats);

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
	for (a :: <integer> in list(1, 0))
	  
	  // (state[d] >> a) & 1 == 0
	  if (logand(ash(state[d], -a), 1) = 0) 
	    tried-something := #t;

	    // SOLVER.ENQUEUE (?)
	    // state[d] |= (1 << a)
	    state[d] := logior(state[d], ash(1, a));
	    assignments[d] := a;
	    
	    // this is the value we'll try

	    let v = var-value-to-lit(d, a);
	    let (no-conflict, new-watchlist) = update-watchlist(o, watchlist, v, assignments);
	    watchlist := new-watchlist;
	    if (~no-conflict)
	      // undo assignment, lets try again
	      remove-key!(assignments, d);
	      
	      sat-stats.failed-assignments := sat-stats.failed-assignments + 1;
	    else
	      dh := dh + 1;
	      
	      sat-stats.correct-assignments := sat-stats.correct-assignments + 1;
	      	      
	      exit-for-block();
	    end if;		    
	  end if;
	end for;
      end block;

      if (~tried-something)
	sat-stats.n-tried-something := sat-stats.n-tried-something + 1;
	
	if (dh = 0)
	  solve-ret(#f, sat-stats);
	  // no more solutions
	else
	  // backtrack, undo state, assignment and variable
	  state[d] := 0;
	  remove-key!(assignments, d);
	  dh := dh - 1;
	  sat-stats.backtracks := sat-stats.backtracks + 1;
	end if;
      end if;
    end while;
  end block;  
end;

// TODO watchlists are not adjusted on backtracking, minisat.pdf says that some constraint systems might need this. see undo lists (page 4, Propagation)
// * Right now we're only undoing watchlists when a conflict is detected

// TODO cleanup variables, sort by activity

// TODO stack of variable assignments (trail) - dont follow variable index -, decide variables

// TODO when conflicts arise, learn (STUDY)

// TODO simplify clauses using boolean algebra (local and globally (?))

// TODO activity

// TODO heuristics:
// - sort variables by usage in the input problem (DONE)
// - preprocessing: limited applications of resolution steps (STUDY)
// - aggressive backtracking, ie, dont jump only 1 var at a time
// - dynamic sort of variables (why?)


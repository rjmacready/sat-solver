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


define function lit-to-var (lit :: <integer>) => (var :: <integer>)
  // this is akin to var / 2, gets the variable
  ash(lit, -1);
end;

define function negated? (lit :: <integer>) => (i :: <integer>)
  // this checks if var is negated
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
	  let var-root = lit-to-var(alternative); // ash(alternative, -1); 
	  let is-negated = negated?(alternative); // 
	  let has-assignment = element(assignments, var-root, default: #"none");
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

define method solve (o :: <sat-solver-it>) => (r :: false-or(<table>));

  /*
    assignments: our assignments at the moment
    d: current variable we're working with
    state: will track which values for which variables we already tried.
           0: nothing has been tried
           1: #f
           2: #t
           3: #f and #t
  */

  let watchlist = setup-watchlist(o);
  let var-count :: <integer> = o.var-count;
  let assignments :: <table> = make(<table>);
  let d :: <integer> = 0;
  let state :: <array> = make(<array>, dimensions: list(var-count), fill: 0); 
  let tried-something :: <boolean> = #f;
  
  block (solve-ret)
    while (#t)	      
      if (d = var-count)
	// All variables are assigned
	// so we will return our assignments
	// we could continue though ...
	solve-ret(assignments);

	d := d - 1;
	// FIXME undo assigment too, right? python doesnt do it
	
      end if;

      /*
	try to assign a value; 
	TODO insert heuristics here
	*/
      
      tried-something := #f;
      block (try-block)
	for (a :: <integer> in list(0, 1))
	  
	  // (state[d] >> a) & 1 == 0
	  if (logand(ash(state[d], -a), 1) = 0) 
	    tried-something := #t;

	    // state[d] |= (1 << a)
	    state[d] := logior(state[d], ash(1, a));
	    assignments[d] := a;
	    
	    // this is the value we'll try
	    let v = logior(ash(d, 1), a);
	    if (~update-watchlist(o, watchlist, v, assignments))
	      
	      remove-key!(assignments, d);
	    else
	      d := d + 1;
	      try-block();
	    end if;		    
	  end if;
	end for;
      end block;

      if (~tried-something)
	if (d = 0)
	  solve-ret(#f);
	  // no more solutions
	else
	  // backtrack, undo state, assignment and variable
	  state[d] := 0;
	  remove-key!(assignments, d);
	  d := d - 1;
	end if;
      end if;
    end while;
  end block;  
end;

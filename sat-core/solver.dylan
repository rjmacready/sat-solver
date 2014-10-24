Module: sat-core
Synopsis: 
Author: 
Copyright: 

/* All by itself a clause doesnt make much sense, as it uses a collection of
indexes only valid for a certain <sat-solver> */
define class <clause> (<object>)
  slot vars :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define class <sat-solver> (<object>)
  /* current free index to assign to a variable */
  slot var-count :: <integer> = 0;
  /* mapping from variable (A, B, C ...) to a index */
  slot vars :: <table> = make(<table>);
  /* collection of clauses */
  slot clauses :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define class <sat-solver-rec> (<sat-solver>)
end class;

define generic add-clause (s :: <sat-solver>, c :: <object>) => ();

/*
 * this expects variables to be 1-indexed and the negative of a variable to be -(var-index)
 * we'll turn this into a form where variables are 0-indexed and literals are given by (var-index * 2); to negate, add 1
 */
define method add-clause (s :: <sat-solver>, cnf :: <collection>) => ();
  let c = make(<clause>);
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
    add!(c.vars, i);
  end for;
  add!(s.clauses, c);
end method;

define method add-clause (s :: <sat-solver>, c :: <string>) => ();
  let simple-lexing = split(c, ' ');
  let c = make(<clause>);
  
  for(lex in simple-lexing)
    let is-negative :: <boolean> = #f;
    let e :: <character> = '\0';
    is-negative := starts-with?(lex, "~");
    if (is-negative)
      e := element(lex, 1);
    else
      e := element(lex, 0);
    end if;
    if (~element(s.vars, e, default: #f))
      s.vars[e] := s.var-count;
      s.var-count := s.var-count + 1;
    end if;
    let var-value = s.vars[e] * 2;
    if (is-negative)
      var-value := var-value + 1;
    end if;
    add!(c.vars, var-value);
  end for;
  
  add!(s.clauses, c);
end method;

define generic solve (s :: <sat-solver>) => (r :: false-or(<table>));
define generic update-watchlist (s :: <sat-solver>, watchlist :: <array>, false_literal :: <integer>, assignments :: <table>) => (result :: <boolean>);
define generic solve-impl (s :: <sat-solver>, watchlist :: <array>) => (fail-or-result :: false-or(<table>));

define method update-watchlist(s :: <sat-solver>, watchlist :: <array>, false_literal :: <integer>, assignments :: <table>) => (r :: <boolean>);
  block (main-ret)

    while (size(watchlist[false_literal]) > 0)
      let clause :: <clause> = watchlist[false_literal][0];
      let found-alternative :: <boolean> = #f;

      block (get-out)	
	for (alternative in clause.vars)
	  // this is akin to var / 2, gets the variable
	  let var-root = ash(alternative, -1); 
	  // this checks if var is negated
	  let is-negated = logand(alternative, 1); 
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

define method solve-impl (o :: <sat-solver-rec>, w :: <array>) => (r :: false-or(<table>));
  let var-count :: <integer> = o.var-count;
  local method solve-impl-rec (watchlist, assignment :: <table>, d :: <integer>)
	  block (solve-ret)
	    if (d = var-count)
	      solve-ret(assignment);
	    else
	      for(a :: <integer> in list(0, 1))
		assignment[d] := a;
		//let ashed :: <integer> = ash(d, 1);
		let false_literal = logior(ash(d, 1), a);
		if (update-watchlist(o, watchlist, false_literal, assignment))
		  //for (a in solve-impl-rec(assignment, d + 1))
		  let a = solve-impl-rec(watchlist, assignment, d + 1);
		  if (a)
		    solve-ret(a);
		  end if;
		  //end for;
		end if;
	      end for;
	      remove-key!(assignment, d);
	    end if;
	    
	    solve-ret(#f);
	  end block
	end method;
  
  solve-impl-rec (w, make(<table>), 0)
end method;

define method solve (o :: <sat-solver>) => (r :: false-or(<table>));
  let watchlist = make(<array>, dimensions: list(o.var-count * 2));
  local method setup-watchlist()
	  let i = 0;
	  while (i < size(watchlist))
	    watchlist[i] := make(<deque>);
	    i := i + 1;
	  end while;
	  
	  for (clause in o.clauses)
	    push-last(watchlist[clause.vars[0]], clause);
	  end for;
	end method;
  
  setup-watchlist();

  let r = solve-impl(o, watchlist);
  r
end method;

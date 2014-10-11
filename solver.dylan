Module: sat
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
  slot var-index :: <integer> = 0;
  /* mapping from variable (A, B, C ...) to a index */
  slot vars :: <table> = make(<table>);
  /* collection of clauses */
  slot clauses :: <stretchy-vector> = make(<stretchy-vector>);
end class;

define generic add-clause (s :: <sat-solver>, c :: <object>) => ();

define method add-clause (s :: <sat-solver>, cnf :: <stretchy-vector>) => ();
  let c = make(<clause>);
  for(cnf-var in cnf) 
    let i = cnf-var;
    if (i < 0)
      i := i * -1;
    end if;
    i := i - 1;
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
      s.vars[e] := s.var-index;
      s.var-index := s.var-index + 1;
    end if;
    let var-value = s.vars[e] * 2;
    if (is-negative)
      var-value := var-value + 1;
    end if;
    add!(c.vars, var-value);
  end for;
  
  add!(s.clauses, c);
end method;

define generic solve (s :: <sat-solver>);

define method solve (o :: <sat-solver>);
  let watchlist = make(<array>, dimensions: list(o.var-index * 2));
  local method setup-watchlist()
	  let i = 0;
	  while (i < size(watchlist))
	    watchlist[i] := make(<deque>);
	    i := i + 1;
	  end while;
	  
	  for (clause in o.clauses)
	    add!(watchlist[clause.vars[0]], clause);
	  end for;  
	end method;
  
  local method update-watchlist(watchlist, false_literal, assignments :: <table>)
	  block (main-ret)
	    block (get-out)
	      while (size(watchlist[false_literal]) > 0)
		let clause :: <clause> = watchlist[false_literal][0];
		let found-alternative :: <boolean> = #f;
		
		for (var in clause.vars)
		  // this is akin to var / 2, gets the variable
		  let var-root = ash(var, -1); 
		  // this checks if var is negated
		  let is-negated = logand(var, 1); 
		  let has-assignment = element(assignments, var-root, default: #"none");
		  if (has-assignment = #"none" | has-assignment = logxor(is-negated, 1))
		    found-alternative := #t;
		    
		    pop(watchlist[false_literal]);
		    push(watchlist[var], clause);
		    
		    get-out();
		  end if;
		end for;
		
		if (~found-alternative)
		  main-ret(#f);
		end if;
	      end while;
	    end block;
	    main-ret(#t);
	  end block
	end method;
  
  let var-count :: <integer> = o.var-index;

  local method solve-impl-iterative (watchlist, assignment :: <table>, d :: <integer>)
	  let state :: <array> = make(<array>, dimensions: list(var-count), fill: 0);
	  let tried-something :: <boolean> = #f;
	  
	  block (solve-ret)
	    while (#t)
	      //format-out("d:%s %= \n", d, state); force-out();
	      
	      if (d = var-count)
		solve-ret(assignment);
		d := d - 1;
		// can continue though
	      end if;
	      
	      tried-something := #f;
	      block (try-block)
		for (a :: <integer> in list(0, 1))
		  if (logand(ash(state[d], -a), 1) = 0) // (state[d] >> 1) & 1 == 0
		    tried-something := #t;
		    state[d] := logior(state[d], ash(1, a)); // state[d] |= (1 << a)
		    assignment[d] := a;
		    if (~update-watchlist(watchlist, logior(ash(d, 1), a), assignment))
		      remove-key!(assignment, d);
		    else
		      d := d + 1;
		      try-block();
		    end;		    
		  end;
		end;
	      end;

	      if (~tried-something)
		if (d = 0)
		  solve-ret(#f);
		  // no more solutions
		else
		  // backtrack
		  state[d] := 0;
		  remove-key!(assignment, d);
		  d := d - 1;
		end;
	      end;
	    end;
	  end
	end method;

  local method solve-impl (watchlist, assignment :: <table>, d :: <integer>)
	  block (solve-ret)
	    if (d = var-count)
	      solve-ret(assignment);
	    else
	      for(a :: <integer> in list(0, 1))
		assignment[d] := a;
		//let ashed :: <integer> = ash(d, 1);
		let false_literal = logior(ash(d, 1), a);
		if (update-watchlist(watchlist, false_literal, assignment))
		  //for (a in solve-impl(assignment, d + 1))
		  let a = solve-impl(watchlist, assignment, d + 1);
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

  setup-watchlist();
  let r = solve-impl-iterative(watchlist, make(<table>), 0);
  format-out("result = \n"); 
  format-out("%s\n", print-assignments(o, r)); force-out();
  format-out("solved\n"); force-out();
end method;

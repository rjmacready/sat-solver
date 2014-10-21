define function var-equal(v1 :: <var>, v2 :: <var>) => (r :: <boolean>)
  v1.idx = v2.idx
end;

define class <lit> (<object>)
  slot var :: <var>, init-keyword: var:;
  slot signal :: <boolean>, init-keyword: signal:;
end class;

define function is-true (l :: <lit>) => (r :: <boolean>)
  l.signal
end;

define function negate (l :: <lit>) => (r :: <lit>)
  make(<lit>, var: l.var, signal: ~l.signal)
end;

define function lit-equal(l1 :: <lit>, l2 :: <lit>) => (r :: <boolean>)
  var-equal(l1.var, l2.var) && l1.signal = l2.signal
end;

define class <constr> (<object>)
end class; 

define class <clause> (<constr>)
  slot learnt :: <boolean>;
  slot activity :: <double-float>;
  slot lits :: $v_lit;
end class;

define inline function lifted (t) => (type :: <type>)
  type-union(singleton(#"bottom"), t);
end function;

define constant $v_int :: <type> = limited(<stretchy-vector>, of: <integer>);

define constant $v_lit :: <type> = limited(<stretchy-vector>, of: <lit>);

define constant $v_constr :: <type> = limited(<stretchy-vector>, of: <constr>);

define constant $v_l_constr :: <type> = limited(<stretchy-vector>, of: false-or(<constr>));

define constant $v_clause :: <type> = limited(<stretchy-vector>, of: <clause>);

define constant $v_v_constr :: <type> = limited(<stretchy-vector>, of: $v_constr);

define constant $v_double :: <type> = limited(<stretchy-vector>, of: <double-float>);

define constant $d_lit = limited(<deque>, of: <lit>);

define constant $v_lbool = limited(<stretchy-vector>, of: lifted(<boolean>));

/*
 Interface
*/

define class <var-order> (<object>)
  slot r-assigns :: $v_lbool;
  slot r-activity :: $v_double;
end class;

define method new-var (v :: <var-order>) => ();
  // TODO
end;

define method update (v :: <var-order>, x :: <var>)
  // TODO
end;

define method update-all (v :: <var-order>)
  // TODO
end;

define method undo (v :: <var-order>, x :: <var>)
  // TODO
end;

// select
define method vselect (v :: <var-order>)
  // TODO
end;

// <lit>

define method negate (l :: <lit>) => (r :: <lit>);
  // TODO
end;

define method sign (l :: <lit>) => (r :: <boolean>);
  // TODO
end method;

define method var (l :: <lit>) => (v :: <var>);
  // TODO
end method;

define method index (l :: <lit>) => (i :: <integer>);
  // TODO
end method;

// solver

define class <solver> (<object>)
end class;

define generic new-var(s :: <solver>) => (r :: <var>);

define generic add-clause(s :: <solver>,
			  literals :: $v_lit) => (r :: <boolean>);

define generic simplify-db(s :: <solver>) => (r :: <boolean>);

define generic solve(s :: <solver>, 
		     assumps :: $v_lit) => 
  (r :: false-or(<table>));


// <constr>

// remove is occupied :\
define generic constr-remove(c :: <constr>, s :: <solver>);
define generic constr-propagate(c :: <constr>, s :: <solver>, p :: <lit>);
define generic constr-simplify(c :: <constr>, s :: <solver>);
define generic constr-undo(c :: <constr>, s :: <solver>, p :: <lit>);
define generic constr-calc-reason(c :: <constr>, s :: <solver>, p :: <lit>) => 
  (reason :: $v_lit);


define method constr-remove(c :: <clause>, s :: <solver>);
end;

define method constr-propagate(c :: <clause>, s :: <solver>, p :: <lit>);
end;

define method constr-simplify(c :: <clause>, s :: <solver>);
end;

define method constr-undo(c :: <clause>, s :: <solver>, p :: <lit>);
end;

define method constr-calc-reason(c :: <clause>, s :: <solver>, p :: <lit>) => 
  (reason :: $v_lit);
  
end;



/*
 Implementation
*/

define class <minisat> (<solver>)
  // constraint database
  slot constrs :: $v_constr = make($v_constr);
  slot learnts :: $v_clause = make($v_clause);
  
  slot cla-inc :: <double-float>;
  slot cla-decay :: <double-float>;
  
  // variable order
  slot activity :: $v_double = make($v_double);
  slot var-inc :: <double-float>;
  slot var-decay :: <double-float>;
  slot order :: <var-order>;
  
  // propagation
  slot watches :: $v_v_constr = make($v_v_constr);
  slot undos :: $v_v_constr = make($v_v_constr);
  slot propQ :: $d_lit = make($d_lit);
  
  // assignments
  slot assigns :: $v_lbool = make($v_lbool);
  slot trail :: $v_lit = make($v_lit);
  slot trail-lim :: $v_int = make($v_int);
  slot reason :: $v_constr = make($v_constr);
  slot level :: $v_int = make($v_int);
  slot root-level :: <integer>;
end class;

define method new-var(s :: <minisat>) => (r :: <var>)
  let v :: <var> = make(<var>, index: size(s.assigns));
  add!(s.watches, make($v_constr));
  add!(s.watches, make($v_constr));
  add!(s.undos, make($v_constr));
  add!(s.reason, #f);
  add!(s.assigns, #"bottom");
  add!(s.level, -1);
  add!(s.activity, 0.0);
  new-var(s.order);
  
  v
end;

define method clause-new(s :: <solver>, ps :: $v_lit, learnt :: <boolean>) => (r :: <boolean>, c :: false-or(<clause>));
  block(return)
    let r = #f;
    if(~learnt)
      // TODO shouldn't this be tighted to assignments ??
      // any literal in ps is true, return TRUE
      
      // both l and ~l occurs ub ps
      for(l in ps)
	if(member?(negate(l), ps))
	  return(values(#t, r));
	end;
      end;
      
      // TODO remove false literals from ps
      
      // TODO remove duplicates from ps
      
    end;
    
    if (size(ps) = 0)
      return(values(#f, r));
    elseif (size(ps) = 1)
      return(values(enqueue(s, ps[0]), r));
    else
      let c = make(<clause>);
      // TODO copy?
      c.lits := ps;
      c.learnt := learnt;
      c.activity := 0;
      
      if (learnt)
	
      end;
      
      push!(s.watches[ (negate(c.lits[0])).index ], c);
      push!(s.watches[ (negate(c.lits[1])).index ], c);
      
      r := c;
    end;
    
    
    values(#t, r);
  end;

end method;

define class <search-params> ()
  // TOOD
end class;


define method var-bump-activity() => ();
  // TODO
end method;

define method var-decay-activity() => ();
  // TODO
end method;

define method var-rescale-activity() => ();
  // TODO
end method;

define method cla-bump-activity() => ();
  // TODO
end method;

define method cla-decay-activity() => ();
  // TODO
end method;

define method cla-rescale-activity() => ();
  // TODO
end method;

define method decay-activities() => ();
  var-decay-activity();
  cla-decay-activity();
end method;

define method reduce-db(solver :: <minisat>) => ()
  // TODO
end method;

define method simplify-db(solver :: <minisat>) => (r :: <boolean>)
  block(return)
    if(propagate(solver) != #"bottom")
      return(#f);
    end;
    
    local method simplify-db-vector(cs :: <collection>) => ()
	    // TODO for each element in cs, 
	    // run simplify(solver), if it returns #t
	    // call remove(solver)
	  end;
    
    simplify-db-vector(solver.learnts);
    simplify-db-vector(solver.constrs);
    
    return(#t);
  end;
end method;

// TODO rename this to solve, fix interface
define method solve2(solver :: <minisat>) => (r :: lifted(false-or(<table>)));
  block(return-solve)
    let nof_conflicts = 100;
    let nof_learnts = solver.no-contraints / 3;
    let status :: lifted(false-or(<table>)) = #"bottom";
    let params :: <search-params> = make(<search-params>, 0.95, 0.999);
    
    for(assump in solver.assumps.size)
      // TODO is != really like this?
      if(~assume(solver, assump) or propagate(solver) != #"bottom")
	cancel-until(solver, 0);
	return-solve(#f);
      end;
    end;
    
    solver.root-level = decision-level(solver);
    
    while (status)
      status := search(nof_conflicts, nof_learnts, params);
      nof_conflicts := nof_conflicts * 1.5;
      nof_learnts := nof_learnts * 1.1;
    end;

    cancel-until(solver, 0);
    
    return-solve(status)
  end;
end method;

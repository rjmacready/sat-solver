Module: sat-core-minisat
Synopsis: 
Author: 
Copyright: 


define class <var> (<object>)
  slot idx :: <integer>, init-keyword: idx:;
end class;

define method \= (v0 :: <var>, v1 :: <var>) => (well? :: <boolean>)
  v0.idx = v1.idx
end method;

define class <lit> (<object>)
  slot var :: <var>, init-keyword: var:;
  slot sign :: <boolean>, init-keyword: sign:;
end class;

define method \= (l0 :: <lit>, l1 :: <lit>) => (well? :: <boolean>)
  l0.var = l1.var & l0.sign = l1.sign
end method;

define function negate (l :: <lit>) => (r :: <lit>)
  make(<lit>, var: l.var, sign: ~l.sign);
end function;

define function index (l :: <lit>) => (i :: <integer>)
  l.var.idx
end function;


define inline function lifted (t) => (type :: <type>)
  type-union(singleton(#"bottom"), t);
end function;

define constant $v_int :: <type> = limited(<stretchy-vector>, of: <integer>);
define constant $v_lit :: <type> = limited(<stretchy-vector>, of: <lit>);

define class <constr> (<object>)
end class;

define class <clause> (<constr>)
  slot learnt :: <boolean>;
  slot activity :: <double-float>;
  slot lits :: $v_lit;
end class;

define constant $v_constr :: <type> = limited(<stretchy-vector>, of: <constr>);
define constant $v_l_constr :: <type> = limited(<stretchy-vector>, of: false-or(<constr>));
define constant $v_clause :: <type> = limited(<stretchy-vector>, of: <clause>);
define constant $v_v_constr :: <type> = limited(<stretchy-vector>, of: $v_constr);
define constant $v_double :: <type> = limited(<stretchy-vector>, of: <double-float>);
define constant $d_lit = limited(<deque>, of: <lit>);
define constant $lbool =  lifted(<boolean>);
define constant $v_lbool = limited(<stretchy-vector>, of: $lbool);

define class <var-order> (<object>)
  slot solver :: <solver>;
end class;

define method new-var (v :: <var-order>) => ()
  // TODO
end method;

define method update (v :: <var-order>, x :: <var>) => ()
  // TODO
end method;

define method update-all (v :: <var-order>) => ()
  // TODO
end method;

// FIXME undo is taken
define method v-undo (v :: <var-order>, x :: <var>) => ()
  // TODO
end method;

// FIXME select is taken
define method v-select (v :: <var-order>) => ()
  // TODO
end method;


define class <solver> (<object>)
  slot constrs :: $v_constr = make($v_constr);
  slot learnts :: $v_clause = make($v_clause);
  slot cla-inc :: <double-float>;
  slot cla-decay :: <double-float>;
  
  slot activity :: $v_double = make($v_double);
  slot var-inc :: <double-float>;
  slot var-decay :: <double-float>;
  slot order :: <var-order> = make(<var-order>);

  slot watches :: $v_v_constr;
  slot undos :: $v_v_constr;
  slot prop-q :: $d_lit;
  
  slot assigns :: $v_lbool;
  slot trail :: $v_lit;
  slot trail-lim :: $v_int;
  slot reason :: $v_l_constr;
  slot level :: $v_int;
  slot root-level :: <integer>;
end class;

define method initialize (s :: <solver>, #key)
  next-method();
  s.order.solver := s;
end method;

// FIXME remove is already taken, lets cheat
define generic c-remove (c :: <constr>, s :: <solver>) => ();
define generic propagate (c :: <constr>, s :: <solver>, p :: <lit>) => (r :: <boolean>);
define generic simplify (c :: <constr>, s :: <solver>) => (r :: <boolean>);
define generic undo (c :: <constr>, s :: <solver>, p :: <lit>) => ();
define generic calc-reason (c :: <constr>, s :: <solver>, p :: <lit>) => (r :: $v_lit);

// ---

define function locked (c :: <clause>, s :: <solver>) => (r :: <boolean>)
  s.reason[var(c.lits[0]).idx] = c
end;

define method c-remove (c :: <clause>, s :: <solver>) => ()
  // TODO
  // TODO we need removeElem, whats this?
end;

define method propagate (c :: <clause>, s :: <solver>, p :: <lit>) => (r :: <boolean>)
  // TODO  
end;

define method simplify (c :: <clause>, s :: <solver>) => (r :: <boolean>)
  block(return)
    let i = 0;
    while (i < size(c.lits))
      let v = value(s, c.lits[i]);
      if(v = #t)
	return(#t)
      elseif(v = #"bottom")
	remove!(c.lits, v, count: 1);
      end;
      i := i + 1;
    end;
    
    return(#f)
  end;
end;

define method undo (c :: <clause>, s :: <solver>, p :: <lit>) => ()
  // TODO
  
end;

define method calc-reason (c :: <clause>, s :: <solver>, p :: <lit>) => (r :: $v_lit)
  // TODO
  
end;


define function n-vars (s :: <solver>) => (r :: <integer>)
  size(s.assigns)
end;

define function n-assigns (s :: <solver>) => (r :: <integer>)
  size(s.trail)
end;

define function n-constraints (s :: <solver>) => (r :: <integer>)
  size(s.constrs)
end;

define function n-learnts (s :: <solver>) => (r :: <integer>)
  size(s.learnts)
end;

define method value (s :: <solver>, x :: <var>) => (r :: $lbool)
  s.assigns[x.idx]
end method;

define method value (s :: <solver>, p :: <lit>) => (r :: $lbool)
  let assigned = value(s, var(p));
  if (assigned ~= #"bottom" & sign(p))
    assigned := ~assigned;
  end;
  assigned
end method;

define method new-var (s :: <solver>) => (r :: <var>)
  let index = n-vars(s);
  add!(s.watches, make($v_constr));
  add!(s.watches, make($v_constr));
  
  add!(s.undos, make($v_constr));
  
  add!(s.reason, #f);
  
  add!(s.assigns, #"bottom");
  
  add!(s.level, -1);
  
  add!(s.activity, 0);

  new-var(s.order);
  
  make(<var>, idx: index);
end;


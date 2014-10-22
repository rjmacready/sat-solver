Module: sat-core-minisat
Synopsis: 
Author: 
Copyright: 

// blah
define generic var-index (literal-or-var :: <object>) => (index :: <integer>);

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

define constant $lit-undef = make(<lit>, 
				  var: make(<var>, index: -1), sign: #f);

define function lit-index (l :: <lit>) => (i :: <integer>)
  let signed? = 0;
  if (l.sign)
    signed? := 1;
  end;
  l.var.idx * 2 + signed?
end function;

define method var-index (l :: <lit>) => (i :: <integer>)
  l.var.idx
end;

define method var-index (var :: <var>) => (i :: <integer>)
  var.idx
end;


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
  slot cla-inc :: <double-float> = 1;
  slot cla-decay :: <double-float> = 0.999;
  
  slot activity :: $v_double = make($v_double);
  slot var-inc :: <double-float> = 1;
  slot var-decay :: <double-float> = 0.95;
  slot order :: <var-order> = make(<var-order>);

  slot watches :: $v_v_constr = make($v_v_constr);
  slot undos :: $v_v_constr = make($v_v_constr);
  slot prop-q :: $d_lit = make($d_lit);
  
  slot assigns :: $v_lbool = make($v_lbool);
  slot trail :: $v_lit = make($v_lit);
  slot trail-lim :: $v_int = make($v_int);
  slot reason :: $v_l_constr = make($v_l_constr);
  slot level :: $v_int = make($v_int);
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

define generic s-propagate (s :: <solver>) => (c :: false-or(<constr>));
define generic enqueue (s :: <solver>, p :: <lit>, c-from :: false-or(<constr>)) => 
  (r :: <boolean>);

// ---

define function locked (c :: <clause>, s :: <solver>) => (r :: <boolean>)
  s.reason[var(c.lits[0]).idx] = c
end;

define method c-remove (c :: <clause>, s :: <solver>) => ()
  // TODO
  // TODO we need removeElem, whats this?
end;


define method propagate (c :: <clause>, s :: <solver>, p :: <lit>) => (r :: <boolean>)
  block(return)
    // make sure the false literal is lits[1]
    if(c.lits[0] = negate(p))
      c.lits[0] := c.lits[1];
      c.lits[1] := negate(p);
    end;
    
    // if 0th watch is true, then clause is already satisfied
    if(value(s, c.lits[0]) == #t)
      return(#t);
    end;
    
    // look for a new literal to watch
    let i = 2;
    while (i < size(c.lits))
      if (value(s, c.lits[i]) ~= #f)
	c.lits[1] := c.lits[i];
	c.lits[i] := negate(p);
	add!(s.watches[lit-index(negate(c.lits[1]))], c);
	return(#t);
      end;
      
      i := i + 1;
    end;
    
    // clause is unit under assignment
    add!(s.watches[lit-index(p)], c);
    return(enqueue(s, c.lits[0], c));
  end;
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
  
  let r = make($v_lit);
  let i = 0;
  if (p ~= #"bottom")
    i := 1;
  end if;
  
  while (i < size(c.lits))
    add!(r, negate(c.lits[i]));
    i := i + 1;
  end while;
  
  if(c.learnt)
    cla-bump-activity(s, c);
  end;
  
  r
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
  let v =   make(<var>, idx: index);

  // lit
  add!(s.watches, make($v_constr));
  // ~lit
  add!(s.watches, make($v_constr));
  
  add!(s.undos, make($v_constr));
  
  add!(s.reason, #f);
  
  add!(s.assigns, #"bottom");
  
  add!(s.level, -1);
  
  add!(s.activity, 0.0);

  new-var(s.order);
  
  v
end;

define function new-clause (s :: <solver>,
			    ps :: $v_lit,
			    learnt :: <boolean>) => (r :: <boolean>,
						     c :: false-or(<clause>))
  block(return)
    if (~learnt)
      // TODO any literal in ps is true, return TRUE
      // TODO both p and ~p occurs in ps, return TRUE
      // TODO remove all false literals from ps
      // TODO remove all duplicates from ps
    end;
    
    if (size(ps) = 0)
      return (values(#f, #f));
    else
      if (size(ps) = 1)
	return (values(enqueue(s, ps[0], #f), #f));
      else
	let c = make(<clause>);
	c.lits := ps;
	c.learnt := learnt;
	c.activity := 0.0;
	
	if (learnt)
	  // TODO pick a second literal to watch
	  
	  // bumping
	  cla-bump-activity(s, c);
	  for(p in ps)
	    var-bump-activity(s, p);
	  end;
	end;
	
	add!(s.watches[lit-index(negate(c.lits[0]))], c);
	add!(s.watches[lit-index(negate(c.lits[1]))], c);
	
	return (#t, c);
      end;
    end;
  end;
end function;

define method s-propagate(s :: <solver>) => (c :: false-or(<constr>))
  block(return)
    while(size(s.prop-q) > 0)
      let p :: <lit> = pop(s.prop-q);
      let i = 0;
      let w = s.watches[lit-index(p)];
      
      while (i < size(w))
	let watched = w[i];
	if(~propagate(watched, s, p))
	  // copy rest of watches (?!?!?)
	  let j = i + 1;
	  while (j < size(w))
	    add!(w, w[j]);
	    
	    j := j + 1;
	  end;
	  
	  s.prop-q := make($d_lit); // FIXME clear?
	  
	  return(watched);
	end;
	i := i + 1;
      end;    
    end;
    
    return(#f)
  end;
end;

define method enqueue (s :: <solver>, p :: <lit>, 
		       c-from :: false-or(<constr>)) => 
  (r :: <boolean>);
  let v = value(s, p);
  if(v ~= #"bottom")
    if (v = #f)
      #f
    else
      #t
    end;
  else
    let i = var-index(p);
    s.assigns[i] := ~p.sign; // ?!?!?
    s.level[i] := decision-level(s);
    s.reason[i] := c-from;
    add!(s.trail, p);
    push-last(s.prop-q, p);
    
    #t
  end;
end;

define method analyze(s :: <solver>,
		      confl :: <constr>) => 
  (learnt :: $v_lit, btlevel :: <integer>);
  
  //let p-reason;
  let learnt :: $v_lit = make($v_lit);
  let btlevel :: <integer> = 0;
  let p :: <lit> = $lit-undef;
  let counter :: <integer> = 0;
  let seen :: <array> = make(<array>, dimensions: list(n-vars(s)), fill: #f);
  
  add!(learnt, $lit-undef);
  
  // FIXME we dont have do ... while(something) in dylan
  
  local method do-while-counter ()
	  let p-reason = calc-reason(confl, s, p);
	  
	  // trace reason for p
	  for(q in p-reason)
	    let v = var-index(q);
	    if (~seen[v])
	      seen[v] := #t;
	      if(s.level[v] = decision-level(s))
	      else
		  if (s.level[v] > 0)
		    add!(learnt, negate(q));
		    
		    if (s.level[v] > btlevel)
		      btlevel := s.level[v];
		    end;
		  end;
	      end;
	    end;
	  end;
	  
	  // select next literal to look at
	  local method do-while-not-seen ()
		  
		  p := last(s.trail);
		  confl := s.reason[var-index(p)];
		  
		  undo-one(s);
		  
		  if(~seen[var-index(p)])
		    do-while-not-seen();
		  end;
		end;
	  
	  do-while-not-seen();
	  
	  // update counter
	  counter := counter - 1;
	  
	  // recursive
	  if (counter > 0)
	    do-while-counter();
	  end;
	end;
  
  do-while-counter();
  
  learnt[0] := p;
  values(learnt, btlevel);
end method;

define method record (s :: <solver>, clause :: $v_lit) => ();
  let (v, c) = new-clause(s, clause, #t);
  enqueue(s, clause[0], c);
  if (c)
    add!(s.learnts, c);
  end;
end;

define method undo-one (s :: <solver>) => ();
  let p :: <lit> = last(s.trail);
  let x :: <var> = p.var;
  let i = x.idx;
  s.assigns[i] := #"bottom";
  s.reason[i] := #f;
  s.level[i] := -1;
  
  v-undo(s.order, x);
  pop(s.trail);
  
  while(size(s.undos[i]) > 0)
    undo(last(s.undos[i]), s, p);
    pop(s.undos[i]);
  end;  
end;

define method assume (s :: <solver>, p :: <lit>) => (r :: <boolean>);
  add!(s.trail-lim, size(s.trail));
  enqueue(s, p, #f);
end;

define method cancel (s :: <solver>) => ();
  for(c from (size(s.trail) - size(last(s.trail-lim))) to 1 by -1)
    undo-one(s);
  end;
  
  pop(s.trail-lim);
end;

define method cancel-until (s :: <solver>, level :: <integer>) => ();
  while(decision-level(s) > level)
    cancel(s);
  end;
end;

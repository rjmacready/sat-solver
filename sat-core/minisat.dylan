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

define function free-lits (clause :: <clause-it>, assignments :: <table>) => (assigned :: <integer>,
									      are-all-assigned-false? :: <boolean>, 
									      frees :: <collection>)
  
  let assigned :: <integer> = 0;
  let are-all-assigned-false? :: <boolean> = #t;
  let frees :: <stretchy-vector> = make(<stretchy-vector>);
  
  local method do-stuff (lit :: <integer>)
	  let var = lit-to-var(lit);
	  let elem = element(assignments, var, default: #"none");
	  if (elem ~= "none")
	    assigned := assigned + 1;
	    
	    // we can use a simple xor here, right?
	    elem := logxor(elem, negated?(lit));
	    
	    if (elem = 1)
	      are-all-assigned-false? := #f;
	    end;
	  else
	    frees := add!(frees, lit);
	  end;	  
	end;
  
  do(do-stuff, clause.lits);
  
  values(assigned, are-all-assigned-false?, frees);
end;

define function print-sat-state (solver :: <sat-solver-it>, assignments :: <table>) => ()
  format-out("***** state: \n");
  for(clause in solver.clauses)
    // print clause
    format-out(" C:");

    for(lit in clause.lits)      
      let v = lit-to-var(lit);
      let is-neg = negated?(lit);
      let value = element(assignments, v, default: #"none");

      format-out("  ");
      
      if (value = #"none")
	format-out(".");
      else	
	value := logxor(value, is-neg);
	format-out("%=", value);
      end;
      
      format-out(" [");
      if(is-neg = 1)
	format-out("~");
      end;
      format-out("%=]", v);
    end;
    format-out("\n");
  end;
  format-out("***** assignments: \n");
  print-assignments(solver, assignments);
  format-out("***** end state\n"); force-out();
end;

define class <sat-solver-it> (<sat-solver>)
  slot sat-stats :: <sat-stats>;
  
  slot var-stats :: <array>;

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

define class <var-assign> ()
  slot variable :: <integer>, init-keyword: variable:;
  slot decision-level :: <integer>, init-keyword: decision-level:;
end class;

// just to make sure, maybe we dont need this
define method \= (o1 :: <var-assign>, o2 :: <var-assign>) => (r :: <boolean>)
  o1.variable = o2.variable & o1.decision-level = o2.decision-level
end;

define method print-object (o :: <var-assign>, s :: <stream>) => ()
  format(s, "{var: %=, level: %=}", o.variable, o.decision-level);
end;

define constant $deque-var-assign = limited(<deque>, of: <var-assign>);

define class <variable-pool> ()
  slot solver :: <sat-solver>, init-keyword: solver:;

  slot used :: $deque-var-assign;
  slot unused :: $deque-var-assign;
end class;

define method print-object (p :: <variable-pool>, s :: <stream>) => ()
  format(s, "used: ");
  let l = p.used;
  let sz = size(l);
  if(sz > 0)
    format(s, "%=", l[0]);
    for (i from 1 below sz)
      format(s, " %=", l[i]);
    end;
  end;
  format(s, "\n");

  format(s, "unused: ");
  l := p.unused;
  sz := size(l);
  if(sz > 0)
    format(s, "%=", l[0]);
    for (i from 1 below sz)
      format(s, " %=", l[i]);
    end;
  end;
  format(s, "\n");
end;

define method initialize (pool :: <variable-pool>, #key) => ()
  format-out("initialize\n"); force-out();
  
  // calculate all statistics here
  let o :: <sat-solver> = pool.solver;
  let sat-stats :: <sat-stats> = o.sat-stats;
  let var-count :: <integer> = o.var-count;
  
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

  let sorted-stats = sort(stats, test: sort-var-stat, stable: #t);
  o.sorted-var-stats := sorted-stats;
  */
  
  sat-stats.var-stats := stats;
    
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

  // ...
  
  pool.used := make($deque-var-assign);
  pool.unused := make($deque-var-assign);
  for(i from 0 below var-count)
    push-last(pool.unused, make(<var-assign>, variable: i, decision-level: -1));
  end;

  format-out("ended initialize\n"); force-out();
end;

define generic select-var (pool :: <variable-pool>, current-level :: <integer>, wanted-var :: <integer>) => (selected :: <integer>);
define generic select-next (pool :: <variable-pool>, current-level :: <integer>) => (selected :: <integer>);
define generic undo-level (pool :: <variable-pool>, current-level :: <integer>, assignments :: <table>, state :: <array>) => ();

define method select-var (pool :: <variable-pool>, current-level :: <integer>, wanted-var :: <integer>) => (selected :: <integer>)
  assert(current-level >= 0, "select-var: current-level should be at least 0 (is %=)\n", current-level);
  assert(wanted-var >= 0, "select-var: wanted-var should be at least 0 (is %=)\n", wanted-var);
  assert(size(pool.unused) > 0, "select-var: pool unused shouldnt be empty\n");
  
  // gets a variable from the pool "manually", without resorting unused
  let element = find-key(pool.unused, curry(\=, make(<var-assign>, variable: wanted-var, decision-level: -1)));
  
  // find element in pool.unused, remove it from there
  pool.unused := remove!(pool.unused, element);

  element.decision-level := current-level;
  
  // push it into used
  push(pool.used, element);
  
  // return variable
  element.variable
end;

define method print-list (l :: <collection>) => ()
  let s = size(l);
  if(s > 0)
    format-out("%=", l[0]);
    for (i from 1 below s)
      format-out(" %=", l[i]);
    end;
  end;
end;

define method select-next (pool :: <variable-pool>, current-level :: <integer>) => (selected :: <integer>)
  assert(current-level >= 0, "select-next: current-level should be at least 0 (is %=)\n", current-level);
  assert(size(pool.unused) > 0, "select-next: pool unused shouldnt be empty (current-level: %=)\n", current-level);

  // resort unused
  format-out("* resorting ...\n"); force-out();
  
  let solver = pool.solver;
  let stats = solver.sat-stats;

  assert(size(pool.unused) + size(pool.used) = solver.var-count, 
	 "size of unused (%=) + used (%=) is different than var-count (%=)\n",
	 size(pool.unused), size(pool.used), solver.var-count);
  
  // TODO start with most used vars, it will help improve unit propagation
  local method unused-test (a1 :: <var-assign>, a2 :: <var-assign>) => (r :: <boolean>)
	  let a1-stats :: <var-stat> = stats.var-stats[a1.variable];
	  let a2-stats :: <var-stat> = stats.var-stats[a2.variable];
	  a1-stats.usage > a2-stats.usage
	end;

  pool.unused := sort!(pool.unused, test: unused-test, stable: #t);

  format-out("* used: ");
  print-list(pool.used); 
  format-out("\n");
  format-out("* sorted: ");
  print-list(pool.unused); 
  format-out("\n");
  force-out();
  
  format-out("* selecting ...\n"); force-out();

  assert(pool.unused[0].decision-level <= current-level, 
	 "select-next: current-level isnt at least the level of the head of unused (current-level: %=, head level: %=) \n",
	 current-level,
	 pool.unused[0].decision-level);
  
  let element = pop(pool.unused);
  
  assert(element.decision-level = -1, "select-next: decision-level of head should be -1 (is %=)\n", element.decision-level);
  
  element.decision-level := current-level;
  
  push(pool.used, element);

  format-out("* next %=\n", element); force-out();
  
  element.variable;
end;

define method undo-level (pool :: <variable-pool>, current-level :: <integer>, assignments :: <table>, state :: <array>) => ()
  assert(current-level >= 0, "undo-level: current-level should be at least 0 (is %=)\n", current-level);
  assert(size(pool.used) > 0, "undo-level: used shouldnt be empty\n");
  let done-something = #f;
  let solver = pool.solver;
  
  // * move all assigned-vars-on-this-level to unused, set decision-level to -1
  // * remove all assignments on all assigned-vars-on-this-level
  // * set state to 0 on all assigned-vars-on-this-level

   while(size(pool.used) > 0 & pool.used[0].decision-level = current-level)
    done-something := #t;
    
    let element = pop(pool.used);   
    let v = element.variable;
    remove-key!(assignments, v);
    state[v] := 0;
    element.decision-level := -1;
    push(pool.unused, element);
  end;  
  
  assert(done-something, "undo-level: should have done something (current-level: %=)\n", current-level);

  assert(size(pool.unused) + size(pool.used) = solver.var-count, 
	 "size of unused (%=) + used (%=) is different than var-count (%=)\n",
	 size(pool.unused), size(pool.used), solver.var-count);
  
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
  assert(value = 1 | value = 0, "var-value-to-lit: value to be either 1 or 0 (is %=)\n", value);
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
  format-out("* add clause\n");
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
    
    // is negative? negate
    if (cnf-var < 0)
      i := i + 1;
    end if;
    
    format-out("** adding cnf-var %= as %=\n", cnf-var, i);
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
    /* */
    clause.sorted-lits := clause.lits;
    push-last(watchlist[select-lit(clause)], clause);
    
    
    // push all. weird?
    /*
    clause.sorted-lits := clause.lits;
    for (lit in clause.lits)
      push-last(watchlist[lit], clause);
    end;
    */
    
    // sort literals in clause by usage, get first
    /* 
    clause.sorted-lits := sort(clause.lits, test: method (l1 :: <integer>, l2 :: <integer>) => (r :: <boolean>)
						let v1 = lit-to-var(l1);
						let v2 = lit-to-var(l2);
						
						sort-var-stat(s.var-stats[v1], s.var-stats[v2]);
					      end);

    push-last(watchlist[clause.sorted-lits[0]], clause);
      */
    
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
  let sat-stats :: <minisat-stats> = make(<minisat-stats>);
  let var-count :: <integer> = o.var-count;  
  o.sat-stats := sat-stats;
  let v-pool = make(<variable-pool>, solver: o);
  
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
  
  let dh :: <integer> = -1;
  let d :: <integer> = -1; // stats[dh].var;
  let can-select-next :: <boolean> = #t;

  let state :: <array> = make(<array>, dimensions: list(var-count), fill: 0); 
  let tried-something :: <boolean> = #f;
  
  sat-stats.var-no := var-count;
  sat-stats.clause-no := size(o.clauses);

  format-out("%=\n", sat-stats);
  force-out();  
  
  block (solve-ret)
    while (#t)
      sat-stats.iterations := sat-stats.iterations + 1;
      
      /*
      // TODO preprocessing
      for (i from 0 below size(o.clauses))
	let clause = o.clauses[i];
	let (assigned, all-assigned-are-false?, frees) = free-lits(clause, assignments);
	
	// assigned > 0 & 
	if (all-assigned-are-false?)
	  
	  // TODO if we find empty clauses, there's a conflict, either cancel or UNSAT, ...
	  if(size(frees) = 0)
	    format-out("*OPP UNSAT at clause #%= (", i);
	    print-list(map(lit-to-var, clause.lits));
	    format-out(")\n");
	    force-out();
	  end;
	  
	  // TODO lets try unit propagation
	  // but look out for possible conflicts, from which we have to learn (?)
	  if(size(frees) = 1)
	    format-out("*OPP UNIT PROPAGATION at clause #%= (", i);
	    print-list(map(lit-to-var, clause.lits));
	    format-out(") has free: %=\n", lit-to-var(frees[0]));
	    force-out();
	  end;	
	end;
	
      end;
      */
      // end preprocesseing
      
      
      // dh is a helper index
      //assert(dh >= 0, "solve: before branching, dh (%=) is less than 0\n", dh); 
      assert(d <= var-count, "solve: before branching, d (%=) is greater than var-count (%=)\n", d, var-count); 
      //assert(dh <= size(v-pool.used), "solve: before branching, dh (%=) is greater than size(v-pool.used) (%=)\n", dh, size(v-pool.used));
      
      
      if (dh = var-count)
	// All variables are assigned
	// so we will return our assignments
	// we could continue though ...
	solve-ret(assignments, sat-stats);
	assert(#f, "should be unreachable\n");
	// * UNREACHABLE, unless we want more solutions *
	// dh := dh - 1;
	// d := stats[dh].var;
	// d := 
	// undo-level(v-pool, dh, assignments, state);
	// FIXME undo assigment too, right? python doesnt do it
	// FIXME fix new d 
      else
	// select next var
	// d := stats[dh].var;
	if (can-select-next)
	  dh := dh + 1;
	  d := select-next(v-pool, dh);
	end if;
      end if;
      
      can-select-next := #f;

      // d is an actual variable
      assert(d >= 0 & d < var-count, "solve: after branching, d (%=) is outside [%=, %=[", d, 0, var-count);
      //assert(dh = size(v-pool.used) - 1, 
	//     "solve: after branching, dh (%=) is different of size(v-pool.used) - 1 (%=)\n", dh, size(v-pool.used) - 1);
      
      /*
	try to assign a value; 
	TODO insert heuristics here
	*/
      
      tried-something := #f;
      block (exit-for-block)
	// trying falses first will improve unit propagation and consequent conflict learning
	for (a :: <integer> in list(1, 0))
	  
	  // (state[d] >> a) & 1 == 0
	  if (logand(ash(state[d], -a), 1) = 0) 
	    tried-something := #t;

	    // SOLVER.ENQUEUE (?)
	    // state[d] |= (1 << a)
	    state[d] := logior(state[d], ash(1, a));
	    assignments[d] := a;
	    
	    print-sat-state(o, assignments);

	    // this is the value we'll try

	    let v = var-value-to-lit(d, a);
	    let (no-conflict, new-watchlist) = update-watchlist(o, watchlist, v, assignments);
	    watchlist := new-watchlist;
	    if (~no-conflict)
	      // undo assignment, lets try again
	      remove-key!(assignments, d);
	      
	      sat-stats.failed-assignments := sat-stats.failed-assignments + 1;
	    else
	      can-select-next := #t;
	      
	      sat-stats.correct-assignments := sat-stats.correct-assignments + 1;
	      	      
	      exit-for-block();
	    end if;		    
	  end if;
	end for;
      end block;
      
      if (~tried-something)
	sat-stats.n-tried-something := sat-stats.n-tried-something + 1;
	
	if (dh = -1)
	  format-out("UNSAT\n");
	  format-out("dh: %=\n", dh);
	  format-out("pool: %=\n", v-pool);
	  format-out("state: %=\n", state);
	  solve-ret(#f, sat-stats);
	  // no more solutions
	else
	  format-out("BACKTRACKING AT %=\n", d);
	  format-out("dh: %=\n", dh);
	  format-out("pool: %=\n", v-pool);
	  format-out("state: %=\n", state);
	  
	  // backtrack, undo state, assignment and variable	  
	  undo-level(v-pool, dh, assignments, state);
	  dh := dh - 1;
	  
	  // can-select-next := #t;
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


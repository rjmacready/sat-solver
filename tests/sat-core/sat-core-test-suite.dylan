module: sat-core-test-suite
synopsis: Test suite for the sat-core library.
author: 
copyright: See LICENSE file in this distribution.

/*
TODO
- simple tests (A \/ B), (A \/ ~A), ...
- test 4 queens problem
*/

define test pop-deletes-first ()
  let x = make(<deque>, dimensions: list(3));
  x[0] := 1;
  x[1] := 2;
  x[2] := 3;
  pop(x);
  
  // test first
  assert-equal(2, x[0]);
  // test size
  assert-equal(2, size(x));
end test;

define test push-last-puts-on-end ()
  let x = make(<deque>, dimensions: list(2));
  x[0] := 1;
  x[1] := 2;
  push-last(x, 3);
  
  // test first
  assert-equal(1, x[0]);
  // test size
  assert-equal(3, size(x));
  // test last
  assert-equal(3, x[2]);
end test;

define test shift-left-is-ash-one ()
  let x = ash(1, 1);
  assert-equal(2, x);
end test;

define test shift-right-is-ash-minus-one ()
  let x = ash(2, -1);
  assert-equal(1, x);
end test;

define test simple-sat ()
  let x = make(<sat-solver-rec>);
  x.var-count := 1;
  add-clause(x, list(1));

  let solution = solve(x);
  assert-not-equal(#f, solution);

  assert-equal(1, solution[0]);
end test;

define test simple-sat-2 ()
  let x = make(<sat-solver-rec>);
  x.var-count := 2;
  add-clause(x, list(1));
  add-clause(x, list(-2));

  let solution = solve(x);
  assert-not-equal(#f, solution);

  assert-equal(1, solution[0]);
  assert-equal(0, solution[1]);
end test;

define test simple-unsat ()
  let x = make(<sat-solver-rec>);
  x.var-count := 1;
  add-clause(x, list(1));
  add-clause(x, list(-1));

  let solution = solve(x);
  assert-equal(#f, solution); 
end test;

define test var-equal ()
  let v0a = make(<var>, idx: 0);
  let v0b = make(<var>, idx: 0);
  let v1a = make(<var>, idx: 1);

  assert-equal(v0a, v0a);
  assert-equal(v0a, v0b);
  assert-not-equal(v0a, v1a);
end test;

define test lit-equal ()
  let v0a = make(<var>, idx: 0);
  let v0b = make(<var>, idx: 0);
  let v1a = make(<var>, idx: 1);
  let l0a = make(<lit>, var: v0a, sign: #t);
  let l0a2 = make(<lit>, var: v0a, sign: #t);
  let l0af = make(<lit>, var: v0a, sign: #f);
  let l0b = make(<lit>, var: v0b, sign: #t);
  let l1a = make(<lit>, var: v1a, sign: #t);

  assert-equal(l0a, l0a);
  assert-equal(l0a, l0a2);
  assert-not-equal(l0a, l0af);
  assert-equal(l0a, l0b);
  assert-not-equal(l0a, l1a);
end test;

define test lit-negate ()
  let v0a = make(<var>, idx: 0);
  let l0a = make(<lit>, var: v0a, sign: #t);

  assert-equal(#f, negate(l0a).sign);
  assert-not-equal(l0a, negate(l0a));
  assert-equal(l0a, negate(negate(l0a)));
end test;

define test stretchy-manipulation ()
  let x = make(<stretchy-vector>);
  assert-equal(0, size(x));
  
  add!(x, 1);
  assert-equal(1, size(x));
  assert-equal(1, x[0]);
  
  add!(x, 2);
  assert-equal(2, size(x));
  assert-equal(1, x[0]);
  assert-equal(2, x[1]);

  add!(x, 3);
  add!(x, 3);
  add!(x, 3);

  assert-equal(5, size(x));
  assert-equal(1, x[0]);
  assert-equal(2, x[1]);
  assert-equal(3, x[2]);
  assert-equal(3, x[3]);
  assert-equal(3, x[4]);

  remove!(x, 3, count: 1);
  assert-equal(4, size(x));
  assert-equal(1, x[0]);
  assert-equal(2, x[1]);
  assert-equal(3, x[2]);
  assert-equal(3, x[3]);

end test;

define suite sat-core-test-suite ()
  test pop-deletes-first;
  test push-last-puts-on-end;
  test shift-left-is-ash-one;
  test shift-right-is-ash-minus-one;
  test simple-sat;
  test simple-sat-2;
  test simple-unsat;
  test var-equal;
  test lit-equal;
  test lit-negate;
  test stretchy-manipulation;
end suite;

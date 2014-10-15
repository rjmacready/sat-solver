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
  x.var-count := 2;
  add-clause(x, list(1));
  add-clause(x, list(2));
  
  assert-not-equal(#f, solve(x))
end test;

define test simple-unsat ()
  let x = make(<sat-solver-rec>);
  x.var-count := 1;
  add-clause(x, list(1));
  add-clause(x, list(-1));
  
  assert-equal(#f, solve(x))
end test;

define suite sat-core-test-suite ()
  test pop-deletes-first;
  test push-last-puts-on-end;
  test shift-left-is-ash-one;
  test shift-right-is-ash-minus-one;
  test simple-sat;
  test simple-unsat;
end suite;

module: sat-core-test-suite
synopsis: Test suite for the sat-core library.
author: 
copyright: See LICENSE file in this distribution.

define test test-some-test ()
  assert-true(#t);
end test;

define suite sat-core-test-suite ()
  test test-some-test;
  test test-simple-cnf;
end suite;

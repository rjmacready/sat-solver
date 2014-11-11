# Tiny SAT Solver

Emphasis on code simplicity, rather than performance, at least for now

Inspired by: 

http://sahandsaba.com/understanding-sat-by-implementing-a-simple-sat-solver-in-python.html

## Running benchmarks

You can download .cnf files from here http://www.cs.ubc.ca/~hoos/SATLIB/benchm.html, extract the files such that the Makefile can see them (for instance, extract uf20-91.tar.gz into a directory "uf20-91", make sure the directory is included in the SRC variable)

## TODO

* Read the minisat paper, the interface is not great...
* Are there coroutines? Try to find a way to milk all possible solutions, right know we only get the first one

### CNF / benchmarks

http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html

https://toughsat.appspot.com/

http://www.miroslav-velev.com/sat_benchmarks.html

http://www.cs.ubc.ca/~hoos/SATLIB/benchm.html

### Introduction

http://sahandsaba.com/understanding-sat-by-implementing-a-simple-sat-solver-in-python.html (http://www.reddit.com/r/programming/comments/25r9xr/understanding_sat_by_implementing_a_simple_sat/)

### Further reading

http://www-cs-faculty.stanford.edu/~uno/programs.html

http://www.msoos.org/

#### Wikipedia

http://en.wikipedia.org/wiki/Boolean_satisfiability_problem

http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories

http://en.wikipedia.org/wiki/DPLL_algorithm

#### Comunity

http://www.satlive.org/

#### Ocaml

https://github.com/andrejbauer/lvr-sat

#### C/C++ implementations

http://minisat.se/Papers.html

https://github.com/niklasso/minisat/blob/master/minisat/core/Solver.cc

http://fmv.jku.at/picosat/ 

#### Prolog implementations

http://www.soi.city.ac.uk/~jacob/solver/

http://ww2.cs.mu.oz.au/~pjs/papers/pearl.pdf

### In the future

http://research.microsoft.com/en-us/um/people/leonardo/fmcad06.pdf

http://www.win.tue.nl/mdseminar/pres/zantema-17-02-11.pdf

http://cvc4.cs.nyu.edu/web/

### Applications

https://www.cs.bu.edu/~hwxi/academic/papers/pldi98.pdf

http://blog.lse.epita.fr/articles/24-using-sat-and-smt-to-defeat-simple-hashing-algorit.html

https://github.com/hugomg/hexiom (http://www.reddit.com/r/programming/comments/pxpzd/solving_hexiom_really_fast_with_a_sat_solver/)

#### Sudoku solved by SATs

https://www.lri.fr/~conchon/mpri/weber.pdf

http://continuum.io/blog/sudoku

http://www.cs.qub.ac.uk/~i.spence/SuDoku/SuDoku.html

http://eli.thegreenplace.net/2007/04/08/sudoku-as-a-sat-problem/

### Is this even related?

http://www.doc.ic.ac.uk/~clh/PALectures/cfa.pdf

http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/

### Biosphere

http://www.eecs.berkeley.edu/~sseshia/219c/spr11/

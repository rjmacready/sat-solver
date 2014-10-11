# Tiny SAT Solver

Emphasis on code simplicity, rather than performance, at least for now

Inspired by: 

http://sahandsaba.com/understanding-sat-by-implementing-a-simple-sat-solver-in-python.html

## Running benchmarks

You can download .cnf files from here http://www.cs.ubc.ca/~hoos/SATLIB/benchm.html, extract the files such that the Makefile can see them (for instance, extract uf20-91.tar.gz into a directory "uf20-91", make sure the directory is included in the SRC variable)

## TODO

* Read the minisat paper, the interface is not great...
* Are there coroutines? Try to find a way to milk all possible solutions, right know we only get the first one

### CNF 

http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html

### Further reading

http://minisat.se/Papers.html

https://github.com/niklasso/minisat/blob/master/minisat/core/Solver.cc

http://fmv.jku.at/picosat/ 

http://www.soi.city.ac.uk/~jacob/solver/

http://www-cs-faculty.stanford.edu/~uno/programs.html

http://en.wikipedia.org/wiki/Boolean_satisfiability_problem

http://en.wikipedia.org/wiki/Satisfiability_Modulo_Theories

http://en.wikipedia.org/wiki/DPLL_algorithm

http://www.satlive.org/

### In the future

http://research.microsoft.com/en-us/um/people/leonardo/fmcad06.pdf

http://www.win.tue.nl/mdseminar/pres/zantema-17-02-11.pdf

### Is this even related?

http://www.doc.ic.ac.uk/~clh/PALectures/cfa.pdf

http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/


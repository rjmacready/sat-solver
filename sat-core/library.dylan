Module: dylan-user

define library sat-core
  use collections;
  use common-dylan;
  use dylan;
  use io;
  use strings;
  use system, import: { file-system };

  export 
    sat-core,
    sat-core-minisat;
end library sat-core;

define module sat-core
  use collections;
  use common-dylan;
  use file-system, import: { with-open-file };
  use format;
  use format-out;
  use standard-io;
  use print;
  use streams;
  use strings;
  use threads;
    
  export 
    parse-cnf-stream!;
  
  export
    print-assignments,
    print-watchlist;

  export
    <sat-solver>,
    <sat-solver-it>,
    <sat-solver-rec>,
    var-count,
    var-count-setter,
    add-clause,
    solve;
end module sat-core;

define module sat-core-minisat
  use collections;
  use common-dylan;
  use file-system, import: { with-open-file };
  use format;
  use format-out;
  use standard-io;
  use print;
  use streams;
  use strings;
  use threads;
  
  export 
    <var>,
    <lit>, negate, sign;
  
end module;


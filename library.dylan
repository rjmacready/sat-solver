Module: dylan-user

define library sat
  use collections;
  use common-dylan;
  use io;
  use strings;
  use system, import: { file-system };
end library sat;

define module sat-parser
  use collections;
  use common-dylan;
  use streams;
  use strings;
  
  export 
    <parser>,
    parse-stream,
    comment-callback,
    comment-callback-setter,
    info-callback,
    info-callback-setter,
    clause-callback,
    clause-callback-setter;
end module;

define module sat
  use collections;
  use common-dylan;
  use file-system, import: { with-open-file };
  use format;
  use format-out;
  use print;
  use sat-parser;
  use streams;
  use strings;
end module sat;

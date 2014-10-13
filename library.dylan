Module: dylan-user

define library sat
  use collections;
  use common-dylan;
  use dylan;
  use io;
  use strings;
  use system, import: { file-system };
  
  use sat-core;
end library sat;

define module sat
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

  use sat-parser;
  use sat-core;
end module sat;

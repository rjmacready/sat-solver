module: dylan-user
author: 
copyright: See LICENSE file in this distribution.

define library sat-core-test-suite
  use common-dylan;
  use io;
  use sat-core;
  use testworks;

  export sat-core-test-suite;
end library;

define module sat-core-test-suite
  use common-dylan, exclude: { format-to-string };
  use format;
  use sat-core;
  use streams, import: { <buffer> };
  use testworks;

  export sat-core-test-suite;
end module;

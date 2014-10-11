Module: sat-parser
Synopsis: 
Author: 
Copyright: 

define class <parser> (<object>);
  slot no-vars :: <integer> = 0;
  slot no-clauses :: <integer> = 0;

  slot comment-callback :: false-or(<function>) = #f;
  slot info-callback :: false-or(<function>) = #f;
  slot clause-callback :: false-or(<function>) = #f;
end class;

define generic parse-stream (parser :: <parser>, s :: <stream>) => ();
define generic parse-read (parser :: <parser>, c :: <character>, s :: <stream>) => ();
define generic parse-read-clauses(parser :: <parser>, format :: <symbol>, s :: <stream>, n-vars :: <integer>, n-cli :: <integer>) => ();

define method parse-stream (parser :: <parser>, s :: <stream>) => ();
  parse-read(parser, read-element(s), s);
end method;

define method parse-read (parser :: <parser>, c == 'c', s :: <stream>) => ();
  if (~stream-at-end?(s))
    // read rest of the line as a comment
    let comment = read-line(s);
    
    if (parser.comment-callback)
      parser.comment-callback(comment);
    end if;
    
    // read next char
    parse-read(parser, read-element(s), s);
  end if;
end method;

define method parse-read (parser :: <parser>, c == 'p', s :: <stream>) => ();
  if (~stream-at-end?(s))
    
    // read rest of the line as info
    let info = read-line(s);
    let tokens = split(info, ' ', remove-if-empty?: #t); 

    let n-vars :: <integer> = string-to-integer(tokens[1]);
    let n-cl :: <integer> = string-to-integer(tokens[2]);
    
    parser.no-vars := n-vars;
    parser.no-clauses := n-cl;
    
    if (parser.info-callback)
      parser.info-callback(n-vars, n-cl);
    end if;
    
    // read next char
    // FIXME tokens[0] doesnt work here ... for some reason
    parse-read-clauses(parser, #"cnf", s, n-vars, n-cl);
  end if;
end method;

define method parse-read-clauses (parser :: <parser>, format == #"cnf", s :: <stream>, 
				  n-vars :: <integer>, n-cl == 0) => ()
  // do nothing
end method;

define method parse-read-clauses (parser :: <parser>, format == #"cnf", s :: <stream>, 
				  n-vars :: <integer>, n-cl :: <integer>) => ();
  if (~stream-at-end?(s))
    let line = read-line(s);
    let tokens = split(line, ' ', remove-if-empty?: #t);
    let vars = make(<stretchy-vector>);
    
    block (quit)
      for (token in tokens)
	let v = string-to-integer(token);
	if (v = 0)
	  quit();
	end if;
	
	add!(vars, v);
      end for;
    end block;
        
    if (parser.clause-callback)
      parser.clause-callback(vars);
    end if;
    
    parse-read-clauses(parser, format, s, n-vars, n-cl - 1);
  end if;
end method;

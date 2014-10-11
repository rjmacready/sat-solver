Module: sat
Synopsis: 
Author: 
Copyright: 

define function main
    (name :: <string>, arguments :: <vector>)
/* 
  let s = make(<sat-solver>);
  add-clause(s, "~A");
  add-clause(s, "A");
  format-out("%=\n", s);
  solve(s);
  exit-application(0);
  */
  // for(file in arguments)
  let file = arguments[0];
  let out-file = arguments[1];
  
  with-open-file(stream = file, direction: #"input")
    with-open-file(out-stream = out-file, direction: #"output")
      let s = make(<sat-solver>);
      let x = make(<parser>);
      x.info-callback := method (n-vars, n-clauses)
			   s.var-index := n-vars;
			 end method;
      
      x.clause-callback := method (tokens)
			     add-clause(s, tokens);
			   end method;
      
      parse-stream(x, stream);
      solve(s);
    end;
  end with-open-file;  
    //end for;

    exit-application(0);
end function main;

main(application-name(), application-arguments());

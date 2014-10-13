Module: sat
Synopsis: 
Author: 
Copyright: 

define function main
    (name :: <string>, arguments :: <vector>)

  // for(file in arguments)
  let mode = arguments[0];
  let file = arguments[1];
  let out-file = arguments[2];
  
  with-open-file(stream = file, direction: #"input")
    with-open-file(out-stream = out-file, direction: #"output")
      dynamic-bind(*standard-output* = out-stream)
	//begin
	  let s = if (mode = "it")
		    make(<sat-solver-it>);
		  else
		    make(<sat-solver-rec>);  
		  end;
	  
	  parse-cnf-stream!(stream, s);
          let solved = solve(s);
          if (solved)
	    print-assignments(s, solved);
	  else
	    format-out("#f");
	  end;
	end;
    end;
  end with-open-file;  
    //end for;

    exit-application(0);
end function main;

main(application-name(), application-arguments());

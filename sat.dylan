Module: sat
Synopsis: 
Author: 
Copyright: 

define function main
    (name :: <string>, arguments :: <vector>)

  // for(file in arguments)
  let mode = arguments[0];
  let file = arguments[1];
  let out-file = element(arguments, 2, default: #f);
  
  with-open-file(stream = file, direction: #"input")
    
    let out-stream = if (out-file)
		       open-file-stream(out-file, 
					direction: #"output");
		     else
			 *standard-output*
		     end;
    
    // with-open-file(out-stream = out-file, direction: #"output")
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
	    format-out("#f\n");
	  end;
	end;

  if (out-file)
    close(out-stream);
  end;
    // end;
  end;  
    //end for;

    exit-application(0);
end function main;

main(application-name(), application-arguments());

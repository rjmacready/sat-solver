Module: sat
Synopsis: 
Author: 
Copyright: 


define method print-object(o :: <clause>, s :: <stream>) => ();
  format(s, "< ");
  for(v in o.vars)
    format(s, "%= ", v);
  end for;
  format(s, ">");
end method;

define method print-assignments (s :: <sat-solver>, o :: false-or(<table>)) => ()
  if (o)
    let i = 0;
    format-out("{\n");
    while (i < s.var-index)
      format-out(" var %= = %=\n", i, element(o, i, default: #"none"));
      i := i + 1;
    end while;
    format-out("}\n");
  else
    format-out("{no solution}\n");
  end if;
end method;

define method print-object (o :: <sat-solver>, s :: <stream>) => ();
  format(s, "{\n");
  for(c in o.clauses)
    format(s, "\t%=\n", c);
  end for;
  format(s, "}");
end method;

define method print-watchlist (a :: <array>) => ();
  let i = 0;
  while (i < size(a))
    let d :: <deque> = aref(a, i);
    format-out("Literal %= watched by", i);
    for(clause in d) 
      format-out(" %=", clause);
    end for;
    format-out("\n");
    i := i + 1;
  end while;
end method;

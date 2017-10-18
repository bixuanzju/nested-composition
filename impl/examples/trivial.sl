--> "((4.0 + 3.0) - 3.0) = 4.0"

-- Examples in "The Expression Problem, Trivially!"


type IEval = {eval : Int};
trait lit (x : Int) { self : IEval =>
  eval : Int = x
};
trait add (e1 : IEval, e2 : IEval) { self : IEval =>
  eval : Int = e1.eval + e2.eval
};


trait sub (e1 : IEval, e2 : IEval) { self : IEval =>
  eval : Int = e1.eval - e2.eval
};

type IPrint = IEval & { print : String };
trait litP (x : Int) : IPrint inherits lit(x) { self : IPrint =>
  print = x.toString
};
trait addP (e1 : IPrint, e2 : IPrint) : IPrint inherits add(e1,e2)  { self : IPrint =>
  print = "(" ++ e1.print ++ " + " ++ e2.print ++ ")"
};
trait subP (e1 : IPrint, e2 : IPrint) : IPrint inherits sub(e1,e2)  { self : IPrint =>
  print = "(" ++ e1.print ++ " - " ++ e2.print ++ ")"
};


l1 = new[IPrint] litP(4);
l2 = new[IPrint] litP(3);
l3 = new[IPrint] addP(l1, l2);
e  = new[IPrint] subP(l3, l2);
main = e.print ++ " = " ++ e.eval.toString

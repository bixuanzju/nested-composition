--> "7.0 + 2.0 - 4.0 = 5.0"


type IEval = {eval : Int};

type ExpAlg[E] = {
  lit : Int -> E,
  add : E -> E -> E
};

evalAlg : ExpAlg[IEval] = {
  lit x =  {eval = x},
  add x y = {eval = x.eval + y.eval}
};

type SubExpAlg[E] = ExpAlg[E] & { sub : E -> E -> E };

subEvalAlg : SubExpAlg[IEval] = evalAlg ,, {
  sub x y = { eval = x.eval - y.eval }
};

type IPrint = { print : String };

printAlg : SubExpAlg[IPrint] = {
  lit x = { print = x.toString },
  add x y = { print = x.print ++ " + " ++ y.print },
  sub x y = { print = x.print ++ " - " ++ y.print }
};


combine A [B * A] (f : SubExpAlg[A]) (g : SubExpAlg[B]) = f ,, g;

e1 E (f : ExpAlg[E]) = f.add (f.lit 7) (f.lit 2);

e2 E (f : SubExpAlg[E]) = f.sub (e1 E f) (f.lit 4);

evalPrintAlg : SubExpAlg[IEval & IPrint] =
  combine IEval IPrint subEvalAlg printAlg;

o = e2 (IEval & IPrint) evalPrintAlg;

main = o.print ++ " = " ++ o.eval.toString

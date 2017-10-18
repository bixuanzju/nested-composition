--> "2.0 + 3.0 and 5 + 4 = 9.0"

type ExpAlg[E] = {
  lit : Int -> E,
  add : E -> E -> E
};

type Exp = { accept : forall E . ExpAlg[E] -> E };

type IEval = { eval : Int };
type IPrint = { print : String };


trait evalAlg : ExpAlg[IEval] { self =>
  lit x   = { eval = x };
  add x y = { eval = x.eval + y.eval }
};


e1 : Exp = { accept = /\E . \f -> f.add (f.lit 2) (f.lit 3) };


-- Family self reference
trait printAlg3 : ExpAlg[IPrint] { fself : ExpAlg[IEval & IPrint] =>
  lit x  = { print = x.toString };
  add e1 e2 = {print =
    let plus54 : IEval = fself.add (fself.lit 5) (fself.lit 4)
    in e1.print ++ " + " ++ e2.print ++ " and " ++ "5 + 4 = " ++ plus54.eval.toString
  }
};

trait evalAlg2 : ExpAlg[IEval] { self =>
  lit x = { eval = x + 1 };
  add x y = { eval = x.eval + y.eval }
};

o = new[ExpAlg[IEval & IPrint]] evalAlg & printAlg3;

main = (e1.accept (IEval & IPrint) o).print

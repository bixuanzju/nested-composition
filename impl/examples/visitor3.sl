--> "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0"

type ExpAlg[E] = {
  lit : Int -> E,
  add : E -> E -> E
};

type Exp = { accept : forall E . ExpAlg[E] -> E };

type IEval = { eval : Int };

trait evalAlg : ExpAlg[IEval] { self =>
  lit x   = { eval = x };
  add x y = { eval = x.eval + y.eval }
};


type SubExpAlg[E] = ExpAlg[E] & { sub : E -> E -> E };
trait subEvalAlg : SubExpAlg[IEval]  inherits evalAlg  { self =>
  sub x y = { eval = x.eval - y.eval }
};
type ExtExp = { accept: forall E. SubExpAlg[E] -> E };



type IPrint = { print : String };


trait printAlg : SubExpAlg[IPrint] { self =>
  lit x   = { print = x.toString };
  add x y = { print = "(" ++ x.print ++ " + " ++ y.print ++ ")" };
  sub x y = { print = "(" ++ x.print ++ " - " ++ y.print ++ ")" }
};



lit (n : Int) : Exp = {
  accept E f = f.lit n
};
add (e1 : Exp) (e2 : Exp) : Exp = {
  accept E f = f.add (e1.accept E f) (e2.accept E f)
};
sub (e1 : ExtExp) (e2 : ExtExp) : ExtExp = {
  accept E f = f.sub (e1.accept E f) (e2.accept E f)
};


trait combine1 A [B * A] (f : Trait[SubExpAlg[A]] , g : Trait[SubExpAlg[B]]) { self =>
  lit (x : Int)   = (new[SubExpAlg[A]] f).lit x   ,, (new[SubExpAlg[B]] g).lit x;
  add (x : A & B) (y : A & B) = (new[SubExpAlg[A]] f).add x y ,, (new[SubExpAlg[B]] g).add x y;
  sub (x : A & B) (y : A & B) = (new[SubExpAlg[A]] f).sub x y ,, (new[SubExpAlg[B]] g).sub x y
};



-- BEGIN_COMBINE_DEF
trait combine A [B * A] (f : Trait[SubExpAlg[A]], g : Trait[SubExpAlg[B]]) inherits f & g { };
-- END_COMBINE_DEF

e1 : Exp = {accept E f = f.add (f.lit 2) (f.lit 3)};
e2 : ExtExp = { accept E f = f.sub (f.lit 5) (e1.accept E f) };
e3 : ExtExp = { accept E f = f.add (e2.accept E f) (f.lit 3) };

-- BEGIN_COMBINE1_TEST
alg = combine IEval IPrint subEvalAlg printAlg;
o = e3.accept (IEval & IPrint) (new[SubExpAlg[IEval & IPrint]] alg);
main = o.print ++ " = " ++ o.eval.toString
-- Output: "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0"
-- END_COMBINE1_TEST

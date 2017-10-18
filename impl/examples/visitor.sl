--> "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0 and (2.0 + 3.0) = 5.0"

-- BEGIN_ALGEBRA_DEF
type ExpAlg[E] = { lit : Int -> E, add : E -> E -> E };
-- END_ALGEBRA_DEF

-- BEGIN_MKVALUE_DEF
mkExp E (f : ExpAlg[E]) : E = f.add (f.lit 2) (f.lit 3);
-- END_MKVALUE_DEF

-- BEGIN_CHURCH_DEF
type Exp = { accept : forall E . ExpAlg[E] -> E };
-- END_CHURCH_DEF

-- BEGIN_DC_DEF
lit (n : Int) : Exp = {
  accept E f = f.lit n
};
add (e1 : Exp) (e2 : Exp) : Exp = {
  accept E f = f.add (e1.accept E f) (e2.accept E f)
};
-- END_DC_DEF


-- BEGIN_EVAL_DEF
type IEval = { eval : Int };
-- END_EVAL_DEF

-- BEGIN_EVAL_IMPL
trait evalAlg { self =>
  lit (x : Int)            = { eval = x };
  add (x : IEval) (y : IEval) = { eval = x.eval + y.eval } };
-- END_EVAL_IMPL


-- BEGIN_SUB_DEF
type SubExpAlg[E] = ExpAlg[E] & { sub : E -> E -> E };
trait subEvalAlg inherits evalAlg { self =>
  sub (x : IEval) (y : IEval) = { eval = x.eval - y.eval } };
type ExtExp = { accept: forall E. SubExpAlg[E] -> E };
-- END_SUB_DEF



-- BEGIN_PRINT_DEF
type IPrint = { print : String };
-- END_PRINT_DEF


-- BEGIN_PRINT_IMPL
trait printAlg { self =>
  lit (x : Int)              = { print = x.toString };
  add (x : IPrint) (y : IPrint) = { print = "(" ++ x.print ++ " + " ++ y.print ++ ")" };
  sub (x : IPrint) (y : IPrint) = { print = "(" ++ x.print ++ " - " ++ y.print ++ ")" } };
-- END_PRINT_IMPL

sub (e1 : ExtExp) (e2 : ExtExp) : ExtExp = {
  accept E f = f.sub (e1.accept E f) (e2.accept E f)
};
-- BEGIN_EVAL_PRINT
eval (e : ExtExp)  : Int = (e.accept IEval (new[SubExpAlg[IEval]] subEvalAlg)).eval;
print (e : ExtExp) : String = (e.accept IPrint (new[SubExpAlg[IPrint]] printAlg)).print;
-- END_EVAL_PRINT

-- BEGIN_EXPRESSION1_EG
e1 : Exp = { accept E f = f.add (f.lit 2) (f.lit 3) };
-- END_EXPRESSION1_EG
-- BEGIN_EXPRESSION2_EG
e2 : ExtExp = { accept E f = f.sub (f.lit 5) (e1.accept E f) };
e3 : ExtExp = { accept E f = f.add (e2.accept E f) (f.lit 3) };
-- END_EXPRESSION2_EG

{-
-- BEGIN_EXPRESSION_WRONG
def e3 : ExtExp = add e2 (lit 3)
-- END_EXPRESSION_WRONG
-}

-- BEGIN_VISITOR_EG
main = print e3 ++ " = " ++ (eval e3).toString ++ " and " ++ print e1 ++ " = " ++ (eval e1).toString
-- Output: "((5.0 - (2.0 + 3.0)) + 3.0) = 3.0 and (2.0 + 3.0) = 5.0"
-- END_VISITOR_EG

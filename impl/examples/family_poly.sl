--> ()

-- BEGIN_MY_EXAMPLE
type IEval = {eval : Int} ;

type IPrint = { print : String } ;

type A = {
   lit : Int -> IEval & IPrint,
   add : IEval & IPrint -> IEval & IPrint -> IEval & IPrint
} ;

type B = {lit : Int -> IEval , add : IEval -> IEval -> IEval} ;

type C = {lit : Int -> IPrint, add : IPrint -> IPrint -> IPrint} ;

test (x : B & C) : A  = x ;
-- END_MY_EXAMPLE

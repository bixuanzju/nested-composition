--> true

type GCircuitAlg[In, Out] = {
  identity : Int -> Out,
  fan : Int -> Out,
  beside : In -> In -> Out,
  above : In -> In -> Out
};

type CircuitAlg[E] = GCircuitAlg[E, E];

type Circuit = { accept : forall E. CircuitAlg[E] -> E };

-- Example
e1 : Circuit = { accept E f =
  f.above (f.beside (f.fan 2) (f.fan 2))
          (f.beside (f.beside (f.identity 1) (f.fan 2)) (f.identity 1))
};

-- Width interpretation
type Width = { width : Int };

trait widthCircuit : CircuitAlg[Width] { self =>
  identity n   = { width = n };
  fan n        = { width = n };
  beside c1 c2 = { width = c1.width + c2.width };
  above c1 c2  = { width = c1.width }
};

width (c : Circuit) : Int = (c.accept Width (new[CircuitAlg[Width]] widthCircuit)).width;


-- Depth interpretation
type Depth = { depth : Int };

max (x : Int) (y : Int) = if x > y then x else y;

trait depthCircuit : CircuitAlg[Depth] { self =>
  identity n   = { depth = 0 };
  fan n        = { depth = 1 };
  beside c1 c2 = { depth = max c1.depth c2.depth };
  above c1 c2  = { depth = c1.depth + c2.depth }
};

depth (c : Circuit) : Int = (c.accept Depth (new[CircuitAlg[Depth]] depthCircuit)).depth;

-- Well-sized interpretation

type WellSized = { wellSized : Bool };

trait sizedCircuit : GCircuitAlg[Width & WellSized, WellSized] { self =>
  identity n   = { wellSized = true };
  fan n        = { wellSized = true };
  beside c1 c2 = { wellSized = c1.wellSized && c2.wellSized };
  above c1 c2  = { wellSized = c1.wellSized && c2.wellSized && c1.width == c2.width }
};

trait merge A [B * A] (a : Trait[CircuitAlg[A]], b : Trait[GCircuitAlg[A & B, B]]) : CircuitAlg[A & B] { self =>
  identity n   = (new[CircuitAlg[A]] a).identity n ,, (new[GCircuitAlg[A & B, B]] b).identity n;
  fan n        = (new[CircuitAlg[A]] a).fan n ,, (new[GCircuitAlg[A & B, B]] b).fan n;
  beside c1 c2 = (new[CircuitAlg[A]] a).beside c1 c2 ,, (new[GCircuitAlg[A & B, B]] b).beside c1 c2;
  above c1 c2  = (new[CircuitAlg[A]] a).above c1 c2 ,, (new[GCircuitAlg[A & B, B]] b).above c1 c2
};

alg = merge Width WellSized widthCircuit sizedCircuit;

o = new[CircuitAlg[Width & WellSized]] alg;

main = (e1.accept (Width & WellSized) o).wellSized

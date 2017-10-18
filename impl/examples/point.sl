--> 3.0


type Point = { x : Int, y : Int };
trait point(x : Int, y: Int) { self : Point =>
  x = x;
  y = y
};

type Point3D = Point & { z : Int };

-- BEGIN_DESUGAR1
trait point3D(x: Int, y : Int) inherits point(x,y) { self : Point3D => z = self.x };
-- END_DESUGAR1

test = new[Point3D] point3D(3,4);

main = test.x

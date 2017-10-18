--> "Round button has area: 28.26"

-- BEGIN_POINT_DEF
type Point = { x : Int, y : Int };
trait point(x : Int, y: Int) { self =>
  x = x;
  y = y };
-- END_POINT_DEF

-- BEGIN_POINT_TEST
pointTest  = new[Point] point(3, 4);
-- END_POINT_TEST

abs (x : Int) = if x < 0 then (0 - x) else x;

square (x : Int) = x * x;


-- BEGIN_CIRCLE_DEF
type Circle = Point & {radius : Int};
trait circle(center : Point, radius : Int) inherits point(center.x, center.y) { self =>
  radius = radius };
-- END_CIRCLE_DEF

-- BEGIN_CIRCLE_TEST
circleTest = new[Circle] circle(pointTest, 3);
-- END_CIRCLE_TEST

-- BEGIN_CIRCLE_FNS
type CircleFns = { area : Int, grow : Int, shrink : Int };
trait circleFns { self : Circle =>
  area   = self.radius * self.radius * 3.14;
  grow   = self.radius + 1;
  shrink = self.radius - 1 };
-- END_CIRCLE_FNS

-- BEGIN_CIRCLE_FULL
circleWithFns = new[Circle & CircleFns] circle(pointTest, 3) & circleFns;
-- END_CIRCLE_FULL


-- BEGIN_BUTTON_DEF
type Button = { label : String };
trait button(label : String) { self => label = label };
-- END_BUTTON_DEF

-- BEGIN_BUTTON_FNS
type ButtonFns = { hover : Bool -> String, press : Bool -> String };
trait buttonFns { self : Button =>
  hover (b : Bool) = if b then "hovering..." else "no hovering";
  press (b : Bool) = if b then "pressing..." else "no pressing" };
-- END_BUTTON_FNS

-- BEGIN_ROUNDBUTTON_DEF
type RoundButton = Circle & Button;
trait roundButton(radius : Int, center: Point, label : String)
  inherits circle(center, radius) & button(label) {};
-- END_ROUNDBUTTON_DEF


-- BEGIN_ASOVAL_DEF
trait asOval(shortRadius : Int, longRadius : Int) { self =>
  radius = shortRadius;
  longRadius = longRadius };
-- END_ASOVAL_DEF

{-
-- BEGIN_CONFLICT_DEF
-- Invalid SEDEL code:
trait oval(shortRadius : Int, longRadius : Int, center: Point)
  inherits circle(center, shortRadius) & asOval(shortRadius, longRadius)
-- END_CONFLICT_DEF
-}


-- BEGIN_CONFLICT_RESOLVE
trait oval(shortRadius : Int, longRadius : Int, center: Point)
  inherits circle(center, shortRadius) \ { radius : Int } & asOval(shortRadius, longRadius) { };
-- END_CONFLICT_RESOLVE

-- BEGIN_NORM_DEF
type Norm = { norm : Int -> Int -> Int };
trait euclideanNorm { self : Point =>
  norm (x : Int) (y : Int) = (square(self.x - x) + square(self.y - y)).sqrt };
trait manhattanNorm { self : Point =>
  norm (x : Int) (y : Int) = abs((self.x - x)) + abs((self.y - y)) };
-- END_NORM_DEF

-- BEGIN_CIRCLE_FNS2
type CircleFns2 = CircleFns & { inCircle : Int -> Int -> Bool };
trait circleFns2 inherits circleFns { self : Circle & Norm =>
  inCircle (x : Int) (y : Int) = self.norm x y < self.radius };
-- END_CIRCLE_FNS2

-- BEGIN_POINT_FUNC
roundButtonFac (radius : Int) (center : Point) (norm : Trait[Point, Norm]) =
  new[RoundButton & CircleFns2 & ButtonFns & Norm]
    roundButton(radius, center, "Round button") & circleFns2 & buttonFns & norm;
-- END_POINT_FUNC

-- BEGIN_ROUNDBUTTON_TEST2
roundButtonTest2 = roundButtonFac 3 pointTest euclideanNorm;
-- END_ROUNDBUTTON_TEST2

test = roundButtonTest2.inCircle 3 4;


-- BEGIN_ROUNDBUTTON_TEST
roundButtonTest = new[RoundButton & CircleFns & ButtonFns]
  roundButton(3, pointTest, "Round button") & circleFns & buttonFns;
main = roundButtonTest.label ++ " has area: " ++ (roundButtonTest.area).toString
-- Output: "Round button has area: 28.26"
-- END_ROUNDBUTTON_TEST

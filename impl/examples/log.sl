--> "logging..."

extend S [U*S] (first : S) (second : U) : S&U = first ,, second;

type Person   = {name : String, male : Bool};
type Loggable = {log : String};
trait person(n : String, s : Bool) { self =>
  name = n;
  male = s
};
trait consoleLogger { self =>
  log = "logging..."
};

jim = new[Person & Loggable] person("jim", true) & consoleLogger;


type Employee = {name : String, male : String};
trait employee(n : String, s: String) { self =>
  name = n;
  male = s
};
-- The following doesn't type check
-- val fool = new[Employee & Person] employee("Tom", "yes") & person("Jim", true)

-- The following type checks
-- Though the type looks ugly
fool = new[(T & {male : String}) & Person] employee("Tom", "yes") \ { name : String } & person("Jim", true);

main = jim.log

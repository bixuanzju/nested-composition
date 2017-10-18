--> 3.0

type IdType [B] = B -> B;

id : IdType [Int] = \x -> x;

main = id 3

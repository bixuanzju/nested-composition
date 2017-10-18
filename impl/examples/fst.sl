--> true

fst A [B*A] (x : A&B) : A = x;

snd A [B*A] (x : A&B) : B = x;

main = snd Int Bool (1,,true)

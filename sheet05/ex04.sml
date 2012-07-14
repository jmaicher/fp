(* ex04 *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

fun from k = Cons(k, fn() => from(k+1));

fun take(xq, 0) = []
|   take(Cons(x,xq),n) = x :: take(xq(),n-1);

fun iterates f x  = Cons(x, fn() => iterates f (f x));

fun vermoegen zins kap = iterates (fn(kap) => kap + kap*zins/100.0) kap;
(* signature: real -> real -> real seq *)
val entwicklung1 = vermoegen 2.5;
(* signature: real -> real seq *)
val konto1 = entwicklung1 1000.0;
(* signature: real seq *)

fun howlong (Cons(x:real, xq), k:real) =
  if x > k then 0
  else 1 + howlong(xq(), k);

howlong(konto1, 2000.0);

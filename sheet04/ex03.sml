(* ex03) *)

(* a) *)

fun curry f a b = f(a, b);
fun uncurry f = fn (a, b) => f a b;

val double = curry op* 2;
double 21;

fun summation f 0 = 0
|   summation f m = f(m - 1) + summation f (m - 1);

val sum = uncurry summation;

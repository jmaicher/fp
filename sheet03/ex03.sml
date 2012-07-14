(* ex03) *)

(* a) / c) *)

datatype operator = plus
                  | minus
                  | times
                  | divide;

datatype 'a ktree = leaf of 'a | inner of 'a ktree * operator * 'a ktree;


(* b) *)

val t1 = leaf(13);
val t2 = inner(t1, plus, leaf(12));
val t3 = inner(inner(leaf(2), times, t1), minus, t2);

fun evaluate (leaf(n)) = n
|   evaluate (inner(t1, plus, t2)) =
      evaluate(t1) + evaluate(t2)
|   evaluate (inner(t1, minus, t2)) =
      evaluate(t1) + evaluate(t2)
|   evaluate (inner(t1, times, t2)) =
      evaluate(t1) * evaluate(t2)
|   evaluate (inner(t1, divide, t2)) =
      evaluate(t1) div evaluate(t2);

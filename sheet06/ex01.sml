(* ex01) *)

(* sequences and function 'take' *)
datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

fun take(xq, 0 ) = []
|   take(Cons(x,xq),n) = x :: take(xq(),n-1);

Control.Print.printLength := 150;

(* function 'breadthFirst' from slide 7.12 *)
fun breadthFirst (next, pred) root =
  let
    fun bfs [] = Nil
    |   bfs (x::xs) = if pred x then Cons (x, fn () => bfs (xs @ next x))
                      else bfs (xs @ next x)
  in bfs [root] end;

(* a) *)

(* tree description *)
fun next n = ["0"^n, "1"^n];

(* next spans a binary tree which represents binary numbers
 * (sequences of 1's and 0's) *)

(* b) *)

(* predicate --> task *)
val pred = String.isSubstring ("101010");

val solutionSeq = breadthFirst (next, pred) "";
take(solutionSeq, 16);

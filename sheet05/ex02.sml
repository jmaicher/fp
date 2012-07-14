(* ex02) *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

fun from k = Cons(k, fn() => from(k+1));

fun take(xq, 0) = []
|   take(Cons(x,xq),n) = x :: take(xq(),n-1);

val txt = "Standard ML is a formally defined programming language. The 'Definition " ^
  "of Standard ML (Revised)' is the formal definition of the language.";

fun string2CharStream s =
  if size(s) = 0 then Nil
  else Cons(String.sub(s, 0), fn() =>
    (string2CharStream (String.substring(s, 1, size(s) - 1))));

fun read Nil = []
|   read (Cons(x, xq)) = x :: (read (xq()));

read (string2CharStream txt);

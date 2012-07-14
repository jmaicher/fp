(* ex03 *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

fun from k = Cons(k, fn() => from(k+1));

fun take(xq, 0) = []
|   take(Cons(x,xq),n) = x :: take(xq(),n-1);

fun iterates f x  = Cons(x, fn() => iterates f (f x));

(* a) *)

val ppow2 = iterates (fn x => x * 2) 1;

take(ppow2, 5) = [1, 2, 4, 8, 16];

(* b) *)

fun filter pred Nil = Nil
|   filter pred (Cons(x,xf)) = if pred x then Cons (x, fn()=> filter pred(xf()))
      else filter pred (xf());

val txt = "Standard ML is a formally defined programming language. The 'Definition " ^
  "of Standard ML (Revised)' is the formal definition of the language.";

fun string2CharStream s =
  if size(s) = 0 then Nil
  else Cons(String.sub(s, 0), fn() =>
    (string2CharStream (String.substring(s, 1, size(s) - 1))));

fun read Nil = []
|   read (Cons(x, xq)) = x :: (read (xq()));

val alpha = filter (fn c => Char.isAlpha(c));

read(alpha(string2CharStream txt));

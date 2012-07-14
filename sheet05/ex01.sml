(* ex01 *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

fun from k = Cons(k, fn() => from(k+1));

fun take(xq, 0) = []
|   take(Cons(x,xq),n) = x :: take(xq(),n-1);


(* a) *)

fun seqMap f Nil = Nil
|   seqMap f (Cons(x, xq)) = Cons(f(x), fn() => seqMap f (xq()));

val nat = from 1;
val even = seqMap (fn x => 2 * x) (from 0); 
val odd = seqMap (fn x => 2 * x - 1) (from 1);

take(nat, 5) = [1,2,3,4,5];
take(even, 5) = [0, 2, 4, 6, 8];
take(odd, 5) = [1, 3, 5, 7, 9];

(* b) *)

fun pairTwo (Cons(x, xq)) (Cons(y, yq)) =
  Cons((x,y), fn() => pairTwo (xq()) (yq()));

take((pairTwo even odd), 5) = [(0,1), (2,3), (4,5), (6,7), (8,9)];

(* c) *)

fun pairOne Nil = Nil
|   pairOne (Cons(x, xq)) =
      let
        val Cons(y, yq) = xq()
      in
        Cons((x,y), fn() => pairOne (yq()))
      end;

take(pairOne even, 3) = [(0,2), (4,6), (8, 10)];

(* d) *)

fun interleaveThree (Cons(x, xq)) (Cons(y, yq)) (Cons(z, zq)) =
  Cons(x, fn() =>
    Cons(y, fn() =>
      Cons(z, fn() => (interleaveThree (xq()) (yq()) (zq())))));

take(interleaveThree nat even odd, 9) = [1, 0, 1, 2, 2, 3, 3, 4, 5];

(* e) *)

(* 1, 1, 2, 3, 5, 8, 13, 21, .. *)

fun pfibs(a,b) = Cons(a, fn() => pfibs(b, a+b));
val fibs = pfibs(1,1);

take(fibs, 8) = [1, 1, 2, 3, 5, 8, 13, 21];


(* f) *)

fun takeWhile f Nil = []
|   takeWhile f (Cons(x, xq)) =
      if (f x) then x::(takeWhile f (xq()))
      else [];

takeWhile (fn x => x < 1000) fibs;

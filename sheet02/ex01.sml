(* a) write a function qubeA(x) which calculates
 *    the surface of a qube with edge length x *)
val square = fn x => x * x;
val qubeA = fn x => (square x) * 6;

(* b) write a function qubeV(x) which calculates
*     the volume of a quabe with edge length x *)
val qubeV = fn x => x * x * x;

(* c) write a function gcd(a,b) which calculates
*     the greatest common divisor of a and b *)
fun gcd (0, b) = 0
|   gcd (a, 0) = 0
|   gcd (a, b) =
      if b > a then gcd(b, a)
      else
        if a mod b = 0 then b else gcd(b, a mod b); 

(* d) write a function reverse(l) which reverses a list l *)

(* version 1: without pattern matching *)
fun reverse_v1 (l) =
      if (null l) then [] else
        (reverse_v1 (tl l)) @ (hd l)::[];

(* version 2: with pattern matching *)
fun reverse_v2 nil    = []
|   reverse_v2 (h::t) = (reverse_v2 t) @ h::[]

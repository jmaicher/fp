(* ex02 *)

fun summation f 0 = 0
|   summation f m = f(m-1) + summation f (m-1);
(* signature: (int -> int) -> int -> int  *)

(* summation from slide 607 *)
fun summation_slides f m =
  let fun sum (i, z) : real =
    if (i = m) then z else sum(i + 1, z + (f i))
  in 
    sum(0, 0.0)
  end;
(* signature: (int -> real) -> int -> real *)


(* a) *)

summation (fn n => n) 10;
summation_slides (fn n => real(n)) 10;

(* criterias:
 * - signature: different
 * - they calculate the same sum f(0) + ... + f(m-1)
 * - but summation calculates f(m - 1) + ... + f(0)
 *  and summation_slides calculates 0.0 + f(0) + f(1) + ... + f(m - 1) 
 * - time complexity is basically the same, but summation_slides may have
 *  advantages because it's end-recursive. *)

(* b) *)

summation (fn k => k*k) 5;
(* (4*4 + (3*3 + (2*2 + (1*1 + (0*0))))) = 30 *)

summation (summation (fn i => 2 * i + 1)) 5;
(* (((2 * 3 + 1) + ((2 * 2 + 1) + ((2 * 1 + 1) + ((2 * 0 + 1)))))
 * + ((2 * 2 + 1) + ((2 * 1 + 1) + ((2 * 0 + 1))))
 * + ((2 * 1 + 1) + ((2 * 0 + 1)))
 * + ((2 * 0 + 1))) = 30 *)

(* a) write a function with the following signature:
 *    ('a -> 'b) * ('b -> 'c) -> ('a -> 'c) *)

fun f (g, h, x) = h(g(x));

(* b) define the signatures of the following functions: *)

fun x(li, m) =  if null li
                then 0
                else x(tl li, m) + (if hd li = m then 1 else 0);

(* signature: 'a list * 'a -> int *)

fun y(f, nil)   = nil
|   y(f, x::xs) = f x :: y(f, xs);

(* signature: ('a -> 'b) * 'a list -> 'b list *)

fun z(a, b, c) = if null c
                 then nil
                 else if b
                      then a
                      else 1.5 :: tl c;

(* siganture: real list * bool * real list -> real list *)

(* note: a must be real list because z cannot return
 *  can't return different types depending on b.
 *  b must be real list since 1.5 is real and lists cannot
 *  contain elements with different types. *)

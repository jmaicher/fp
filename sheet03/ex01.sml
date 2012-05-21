(* #####################################################
 * Helpers
 * ##################################################### *)

(* assert helpers from http://bit.ly/JJXRN6 *)
exception Assert of string;

val assert : bool * string -> unit =
  fn  (false, s)  => raise (Assert s)
  |   (true, _)   => ignore 0;

(* returns true when list contains element x with f(x) = true *)
fun find(f, nil)   = false
|   find(f, x::xs) = if f(x) then true else find(f, xs); 

(* run-length-encoding of lists
 *  ["a","a","a","a","a","a","b","b","b","b","b","c","c","c","d","d","d","d"]
 *  => [("a", 6), ("b", 5), ("c", 3), ("d", 4)] *)

(* write a function rlencode which encodes a general list in run-length format *)

local
  fun rlencode_impl(nil, x, count) = (x, count)::nil
  |   rlencode_impl(y::ys, x, count) =
        if(x = y) then rlencode_impl(ys, x, count + 1)
        else (x, count)::rlencode_impl(ys, y, 0);
in
  fun rlencode(x::xs) = rlencode_impl(xs, x, 1);
end;

assert(hd (rlencode ["a", "a", "b"]) = ("a", 2),
  "#rlencode returns list with (el, el count) tupels");

assert(length (rlencode ["a", "a", "b", "b", "c", "c"]) = 3,
  "#rlencode returns list containing one tupel for each unique element");

(* write a function rlexpand which converts a list in run-length-format into
 * general format *)

fun rlexpand(nil) = nil
|   rlexpand((x, 0)::xs) = rlexpand(xs)
|   rlexpand((x, count)::xs) = x::rlexpand((x, count - 1)::xs);

assert(List.take (rlexpand([("a", 3), ("b", 2)]), 3) = ["a", "a", "a"],
  "#rlexpand returns list with a sequence of element x of length defined in the tupel");

assert(length (rlexpand [("a", 3), ("b", 2), ("c", 4)]) = 9,
  "#rlexpand returns list with the corrent total number of elements");

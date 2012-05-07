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


(* note: we're implementing sets with lists => list elements must be unique *)

(* #####################################################
 * a) implement the following functions: 
 *    1) fun member(el, set)
 *    2) fun insert(el, set)
 *    3) fun delete(el, set)
 * ##################################################### *)

(* 1) *)
fun member(el, li) = find(fn n => n = el, li);

(* specs *)
assert(member(5, [1,2,3,4,5]),
  "#member returns true when the given set contains the given el");

assert(member(1, [2,3,4,5]) = false,
  "#member returns false when given set doesn't contain the given el");


(* 2) *)
fun insert(el, nil) = [el]
|   insert(el, set) = if find(fn n => n = el, set) then set else [el] @ set;

(* specs *)
assert(insert(1, nil) = [1],
  "#insert creates a new list with the given element when the given list is nil");

assert(find(fn n => n = 3, insert(3, [1, 2])),
  "#insert returns a list cotaining the given el");

assert(find(fn n => n = 1, insert(3, [1, 2]))
  andalso find(fn n => n = 2, insert(3, [1, 2])),
  "#insert preserves existing list elements");

assert(length(insert(1, [1])) = 1,
  "#insert does not add elements twice");


(* 3) *)
fun delete(el, nil)   = []
|   delete(el, x::xs) = if x = el then xs else x::delete(el, xs);

(* specs *)
assert(delete(1, [1, 2]) = [2],
  "#delete removes the given el from the given list");

assert(delete(3, [1, 2, 3]) = [1, 2],
  "#delete returns the given list when it doesn't contain el");



(* #####################################################
 * b) which of your implementations is central-recursive
 *    and which is tail-recursive?
 *    implement a tail-recursive version for each func.
 * ##################################################### *)

(* none of my implementations is tail-recursive. *)

(* tail-recursive implementations: *)

fun member_v2 (el, nil, found)   = found
|   member_v2 (el, x::xs, found) =
      if el = x then member_v2(el, xs, true) 
      else member_v2(el, xs, found);

(* specs *)
assert(member_v2(3, [1, 2, 3, 4], false),
  "tail-rec #member_v2 returns true when given list contains the given el");

assert(member_v2(3, [1, 2, 4], false) = false,
  "tail-rec #member_v2 returns false when given list doens't contain the given el");


fun delete_v2 (el, nil, result)   = result
|   delete_v2 (el, x::xs, result) =
      if el = x then delete_v2(el, xs, result)
      else delete_v2(el, xs, result @ [x]);

(* specs *)
assert(delete_v2(3, [1, 2, 3], []) = [1, 2],
  "tail-rec #delete_v2 removes the given el from the given list");

assert(delete_v2(4, [1, 2, 3], []) = [1, 2, 3],
  "tail-rec #delete_v2 returns the given list when it doens't contain the given el");



(* #####################################################
 * c) implement the following functions: 
 *    1) fun union(set1, set2)
 *    2) fun intersection(set1, set2)
 * ##################################################### *)

(* 1) *)
local
  fun union_impl (nil, nil, result)   = result 
  |   union_impl (nil, y::ys, result) =
        if member(y, result) then union_impl(nil, ys, result)
        else union_impl(nil, ys, y::result)
  |   union_impl (x::xs, set2, result) =
        if member(x, result) then union_impl(xs, set2, result)
        else union_impl(xs, set2, x::result);
in
  fun union (set1, set2) =  union_impl(set1, set2, [])
end;

(* specs *)
assert(find(fn n => n = 2, union([1, 2], [3])),
  "#union contains elements from the first set");

assert(find(fn n => n = 3, union([1, 2], [3])),
  "#union contains elements from the second set");

assert(length(union([1, 2, 3], [4, 5, 6])) = 6,
  "#union returns list with the correct length");

assert(length(union([1, 2, 3], [1, 2, 3, 4])) = 4,
  "#union returns list with unique elements from the given sets");


(* 2) *)
local
  fun intersection_impl (nil, _, result)      = result
  |   intersection_impl (x::xs, set2, result)  =
        if(member(x, set2)) then intersection_impl(xs, set2, x::result) 
        else intersection_impl(xs, set2, result);
in
  fun intersection (set1, set2) = intersection_impl(set1, set2, [])
end;

(* specs *)
assert(find(fn n => n = 2, intersection([1, 2, 3], [2, 3, 4])),
  "#intersection returns list with elements which are in both sets");

assert(find(fn n => n = 1, intersection([1, 2, 3], [2, 3, 4])) = false,
  "#intersection returns list without elements which are only in one set");

assert(length(intersection([1, 2, 3, 4, 5, 6], [4, 5, 6, 7, 8, 9])) = 3,
  "#intersection returns list with corrent length");

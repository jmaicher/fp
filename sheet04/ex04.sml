(* ex04) *)

fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

fun map f nil     = nil
|   map f (x::xs) = (f x)::map f xs;

fun filter pred nil     = nil
|   filter pred (x::xs) = if pred(x) then x::filter pred xs else filter pred xs;

fun foldl f e nil   = e
|   foldl f e (x::xs) = foldl f (f (x,e)) xs;

fun exists pred nil     = false
|   exists pred (x::xs) = (pred x) orelse (exists pred xs);

fun all pred nil     = true
|   all pred (x::xs) = (pred x) andalso (exists pred xs);


(* (Stadt, Einwohnerzahl) *)
val ewznrw =  [
  ("Koeln", 1007119),
  ("Duesseldorf",588735),
  ("Dortmund",580444),
  ("Essen",574635),
  ("Duisburg",489599),
  ("Bochum",374737)
];

(* a) *)

val thousands = map (fn (n:string, p:int) => (n, p div 1000));
thousands ewznrw;

(* b) *)

val halfmil = filter (fn (_:string, p) => p >= 500000);
halfmil ewznrw;

(* c) *)

val nothalfmil = filter (op not o (fn (_:string, p) => p >= 500000));
(* f o g => f(g(x)) *)
nothalfmil ewznrw;

(* d) *)

val totalewz = foldl (fn ((_:string, p), total) => p + total) 0;
totalewz ewznrw;

(* e) *)

fun there name = exists (fn (n:string, _) => name = n);
there "Paderborn" ewznrw;
there "Bochum" ewznrw;

(* f) *)

val milioncities = all (fn (_:string, p) => p >= 1000000);
milioncities ewznrw;

(* g) *)

val ruhrgebiet = ["Bochum", "Bottrop", "Dortmund", "Duisburg", "Essen", "Gelsenkirchen", "Hagen"];
fun ruhrewz l r = filter (fn (s,e) => exists (secr op= s) r) l;
ruhrewz ewznrw ruhrgebiet;

(*
 * signature: ('a * 'b) list -> 'a list -> ('a * 'b) list
 * returns list with pairs (x, y) € l where x € r
 *)

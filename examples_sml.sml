(* fac *)
val rec fac1 = fn n => if n = 0 then 1 else n * fac1(n - 1);

(fac1 5);


local
  fun facA (0, a) = a
  |   facA (n, a) = facA (n - 1, a * n) 
in
  fun fac2 (n) = facA (n, 1)
end;

(fac2 5);


(* functions over lists *)
(*fun listProd (nil)    = 1
|   listProd (x::xs)  =
      x * (listProd xs);

(listProd [1, 2, 3, 4, 5]);*)


fun maxEl (nil)       = 0
|   maxEl (x::nil)    = x
|   maxEl (x::y::xs)  =
      if x > y then maxEl(x::xs)
      else maxEl(y::xs);

(maxEl [1,9,8,4,3,6,0]);

fun append (nil, nil) = []
|   append (nil, y) = y
|   append (x::xs, y) =
      x::(append(xs, y));

(append ([1,2,3,4,5], [6,7,8,9]));

(* coinchange problem *)
val euro_coins = [200, 100, 50, 20, 10, 5, 2, 1];

fun change(_, 0, coins) = [coins]
|   change([], _, _) = []
|   change(c::coinvals, amount, coins) =
      (* cannot take coins *)
      if (amount < 0) then []
      (* span solution tree *)
      else change(c::coinvals, amount - c, c::coins) @
        change(coinvals, amount, coins);

(change (euro_coins, 9, []));

(* sequence *)
datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

(* coinchange sequence *)
fun changeSeq(_, 0, coins, next) = Cons(coins, next)
|   changeSeq([], _, _, next) = next()
|   changeSeq(c::coinvals, amount, coins, next) =
      if (amount < 0) then next()
      else changeSeq(c::coinvals, amount - c, c::coins,
        fn() => changeSeq(coinvals, amount, coins, next));

val changeVals = (changeSeq (euro_coins, 15, [], fn() => Nil));
 
fun takeSeq Nil _ = nil
|   takeSeq _ 0   = nil
|   takeSeq (Cons(x, xf)) n =
      x::(takeSeq (xf()) (n - 1));

(takeSeq changeVals 10);


(* map *)
fun map f nil = nil
|   map f (x::xs) =
      f(x)::(map f xs);

fun double x = x * x;

(map double [1,2,3,4,5,6,7,8,9]);

(* transpose matrix *)
val m = [[3, 4, 5], [1, 3, 9]];

fun transpose (nil::_) = nil
|   transpose rows =
  (map hd rows)::(transpose (map tl rows));

(transpose m);


(* matrix multiplication *)

fun dotProd nil nil = 0
|   dotProd (x::xs) (y::ys) =
      (x * y) + (dotProd xs ys);

fun rowProd _ nil = []
|  rowProd rowA (rowB::rowsB) =
    (dotProd rowA rowB)::(rowProd rowA rowsB);

fun listProd nil _ = []
|   listProd (rowA::rowsA) rowsB =
      (rowProd rowA rowsB)::(listProd rowsA rowsB);   

fun matrixProd rowsA rowsB =
  (listProd rowsA (transpose rowsB));

val m = [[1, 2], [3, 4]];
(matrixProd m m);

val p1 = [(4, 1), (2, 2), (0, 5)];
val p2 = [(3, 2), (2, 1), (1, 1), (0, ~4)];

fun polySum(x, nil) = x
|   polySum(nil, y) = y
|   polySum(((n, a)::xs), ((m, b)::ys)) =
      if n > m then (n, a)::polySum(xs, (m, b)::ys)
      else if m > n then (m, b)::polySum((n, a)::xs, ys)
      else if (a + b = 0) then polySum(xs, ys)
      else (n, a + b)::polySum(xs, ys);

(polySum (p1, p2));


(* Halbierungsverfahren *)

(* take and drop *)
fun take (nil, _)  = []
|   take (_, 0)    = []
|   take ((x::xs), n) =
      x::take(xs, n - 1);

fun drop (nil, _) = []
|   drop (xs, 0)  = xs
|   drop ((x::xs), n) = drop(xs, n - 1);

val l = [1,2,3,4,5,6,7,8,9];
(take (l, 5));
(drop (l, 5));

fun termProd ((n, a), []) = []
|   termProd ((n, a), ((m, b)::ys)) =
      (m + n, a * b)::termProd((n, a), ys);

fun polyProd ([], _) = []
|   polyProd ([(n, a)], ys) = termProd((n,a), ys)
|   polyProd (xs, ys) =
      let val k = (length xs) div 2
      in polySum (polyProd (take(xs, k), ys),
        polyProd (drop(xs, k), ys))
      end;

(polyProd ([(2, 2), (0, 5)], [(2, 2), (0, 5)]));


(* Konzept der Fehlerwerte *)
datatype 'a option = NONE | SOME of 'a;
fun foo bar = if bar then NONE else SOME(4);


(* binary trees *)
datatype 'a binTree = Lf | Br of 'a * 'a binTree * 'a binTree;

fun preorder Lf = nil
|   preorder (Br(v, t1, t2)) =
      v::(preorder(t1) @ preorder(t2));

fun preorderA (Lf, a) = a
|   preorderA (Br(v, t1, t2), a) =
      v::preorderA(t1, preorderA(t2, a));

val t = Br(1, Br(2, Lf, Br(3, Lf, Lf)), Br(4, Lf, Lf));
(preorder t);
(preorderA (t, []));

fun balpre nil = Lf
|   balpre (x::xs) =
      let val k = (length xs) div 2
      in Br(x, balpre(take (xs, k)), balpre(drop (xs, k)))
      end;

(preorder (balpre (preorder t)));

(* abstype *)

abstype color =
  rgb of int * int * int
with
  val BLACK = rgb(0,0,0);
  val WHITE = rgb(255,255,255);

  fun darker(rgb(r,g,b)) =
    rgb(r - 50, g - 50, b - 50);
  
  fun brighter(rgb(r, g, b)) =
    rgb(r + 50, g + 50, b + 50);
end;

(darker WHITE);
(brighter BLACK);

(* composition of functions *)
infix o;
fun (f o g) x = f(g(x));
(* Signature: 'b -> 'c * 'a -> 'b -> 'a -> 'c *)

(* secl/secr *)
fun secl x f y = f(x, y);
fun secr f y x = f(x, y);

fun repeat f x 0 = x
|   repeat f x n =
      (repeat f (f(x)) (n - 1));

val inc = secr op+ 1;
(repeat inc 0 10);

fun summation f 0 = f(0)
|   summation f m =
      f(m - 1) + summation f (m - 1);

(summation double 5);

(summation (fn i => summation (fn j => i + j) 3) 3);
(summation (fn i => summation (secl i op+) i) 3);

(* filter *)
fun filter pred nil = nil
|   filter pred (x::xs) =
      if pred x then x::(filter pred xs)
      else (filter pred xs);

val l = [1,2,3,4,5,6,7,8,9];
filter (secr op> 5) l;

fun reject f = filter ((op not) o f);
reject (fn x => x > 5) l;

fun takeWhile pred nil = nil
|   takeWhile pred (x::xs) =
      if pred x then x::(takeWhile pred xs)
      else nil;

takeWhile (secl 5 op>) l;

infix mem;
fun (x mem nil) = false
|   (x mem (y::ys)) =
      if x = y then true else (x mem ys);

fun intersect xs ys =
  filter (secr (op mem) ys) xs;

intersect [1,2,3,4,5,6,7,8,9] [2, 4, 6, 8, 10];


fun exists pred nil = false
|   exists pred (x::xs) =
      if pred x then true else exists pred xs;

fun (x mem xs) = exists (secr op= x) xs;

(5 mem [1,2,3,4,5]);

fun all pred nil = true
|   all pred (x::xs) =
      if pred x then all pred xs else false;

(map (all (secr op> 5)) [[6,7,8,9,10], [8,9,10,4]]);

fun disjoint xs ys = all (fn x => all (fn y => x <> y) ys) xs;

(disjoint [1,2,3,4,5] [6,7,8,9,10]);
(disjoint [1,2,3,4] [2,3,4,5]);


fun plus (x, y) = x + y;
fun sub (x, y) = x - y; 

fun myFoldl f e nil = e
|   myFoldl f e (x::xs) =
      myFoldl f (f(x, e)) xs;

fun myFoldr f e nil = e
|   myFoldr f e (x::xs) =
      f(x, myFoldr f e xs);

(myFoldl sub 0 [1,10]);
(myFoldr sub 0 [1,10]);
(foldr sub 0 [1,10]);
(foldl sub 0 [1,10]);


fun max (x, y) = if x > y then x else y;

fun treefold f e Lf = e
|   treefold f e (Br(v, t1, t2)) =
      f(v, (treefold f e t1), (treefold f e t2));
 
fun nodes t = treefold (fn (v, t1, t2) => 1 + t1 + t2) 0 t;
fun depth t = treefold (fn (v, t1, t2) => 1 + max(t1, t2)) 0 t;
fun mirror t = treefold (fn (v, t1, t2) => Br(v, t2, t1)) Lf t;
fun flatten t = treefold (fn (v, t1, t2) => v::(t1 @ t2)) [] t;

val t = Br(1, Br(2, Lf, Lf), Br(3, Br(4, Lf, Lf), Lf));

(nodes t);
(depth t);
(mirror t);
(flatten t);


fun iterates f x = Cons(x, fn () => (iterates f (f(x))));
fun from k = iterates (secr op+ 1) k;

fun take _ Nil = nil
|   take 0 _ = nil
|   take n (Cons(x, xf)) =
      x :: (take (n - 1) (xf()));

fun map f Nil = Nil
|   map f (Cons(x, xf)) =
      Cons(f(x), fn() => (map f (xf())));

fun square x = x * x;

(take 5 (iterates (secr op+ 2) 1));
(take 5 (map square (from 10)));

fun add Nil y = y
|   add x Nil = x
|   add (Cons(x, xf)) (Cons(y, yf)) =
      Cons(x + y, fn () => add (xf()) (yf()));

(take 5 (add (from 10) (from 20)));

fun interleave Nil yq = yq
|   interleave (Cons(x, xf)) yq =
      Cons(x, fn() => interleave yq (xf()));

(take 10 (interleave (from 10) (from 20)));

(* Konvergenzabbruch *)
fun within (eps:real) (Cons(x, xf)) =
  let val Cons(y, yf) = (xf())
  in
    if abs(x - y) < eps then y
    else within eps (Cons(y, yf))
  end;

fun nextApproach a x = (a / x + x) / 2.0;
fun qroot a = within 1E~12 (iterates (nextApproach a) 1.0);

(qroot 10.0);

(* depth-first search *)

fun depthFirst next pred root =
  let
    fun dfs nil = Nil
    |   dfs (x::xs) =
          if pred x then Cons(x, fn() => dfs ((next x) @ xs))
          else dfs ((next x) @ xs)
  in
    dfs [root]
  end;


fun breadthFirst next pred root =
  let
    fun bfs nil = Nil
    |   bfs (x::xs) =
          if pred x then Cons(x, fn() => bfs (xs @ (next x)))
          else bfs (xs @ (next x))
  in
    bfs [root]
  end;

fun next x = [x ^ "0", x ^ "1"];
val pred = String.isSubstring "01";

(take 5 (breadthFirst next pred ""));
(* (take 5 (depthFirst next pred "")); *)

(* erastos *)

fun filter pred Nil = Nil
|   filter pred (Cons(x, xf)) =
      if pred x then Cons(x, fn() => filter pred (xf()))
      else filter pred (xf());

fun sift p = filter (fn x => x mod p > 0);
fun sieve (Cons(p, xf)) = (Cons(p, fn() => sieve (sift p (xf()))));
val primes = sieve (from 2);

(take 5 primes);




(* bredth-first search *)

(* records *)
(*
type date = {day:int, month:string, year:int};

(* untyped *)
val today = {day = 24, month = "May", year = 2012};

(* expects type date *)
fun addYear ({day = d, month = m, year = y} : date) =
  {day = d, month = m, year = (y + 1)};

(* works because of structural equiality *)
(addYear today);
(* selector function *)
(#year today);
*)

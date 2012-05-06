(* a) write a function wtab(f,s,t) which calculates
 *    (f(s), f(s+1), ..., f(t-1), f(t)) and use
 *    the function to calculate 2^0 - 2^10 *)
fun wtab (f, s, t) = if s > t then wtab(f, t, s)
      else
        if s = t then f(t)::nil
        else f(s) ::  wtab(f, s+1, t);

fun pow (0, b) = 0
|   pow (a, 0) = 1
|   pow (a, b) = a * pow(a, b-1)

val it = wtab(fn n => pow(2, n), 0, 10);

(* b) write a function straight_line(m, b, s, t) which calculates
 *    y-values for a straight line with gradient m and y intersection b
 *    between s and t. Use wtab with a closure. *)
fun straight_line(m, b, s, t) = wtab(fn x => m * x + b, s, t);

val it = straight_line(2, 5, 0, 10);

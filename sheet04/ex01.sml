(* ex01) *)

fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

(* a) *)

fun dec1 x = secl x op- 1;
val dec2 = secr op- 1;

dec1(5) = dec2(5);

(* b) *)

val endmarker = secr op@ [255];

endmarker([1,2,3]) = [1,2,3,255];

(* c) *)

val rparen = secr op^ ")";
val lparen = secl "(" op^;
val paren = rparen o lparen;

rparen("foo") = "foo)";
lparen("foo") = "(foo";
paren("foo") = "(foo)";

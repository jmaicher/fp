Functional Programming - Lecture: 2012-05-14
============================================

5. Type constructors, modules
-----------------------------

### Encapsulated type definitions

#### abstype vs. datatype

*datatype*:
  - open (functions can be added)

*abstype* (abstract datatype):
  - constructors are hidden from the outside
  - closed (no functions can be added)
  - public interface: defined functions

### Example to use datatype dictionary (Slide: FP-5.5)

val dict = insert(insert(empty, "Hund", "dog"), "Katze", "cat");
lookup(dict, "Hund");

Note: Module constructs are NOT relevant for the exam

6. Functions as data
--------------------

### Lambda expressions (FP-6.2)

  - list of functions =>  all functions must have the same signature

### Currying (FP-6.3)

    fun prefix pre = fn post => pre ^ post
    % signature: string -> (string -> string) 

    val knightify = (prefix "Sir ");
    % signature: string -> string

    (knightify "Julian Maicher")
    % => "Sir Julian Maicher"


#### Currying with three parameters

*Tupel*:
fun f(a, b, c) = Expression...;
'a x 'b x 'c -> 'd 

*Curry*:
fun f a b c = Expression...;
'a -> 'b -> 'c -> 'd

### Currying as functional (FP-6.5)

    - `secl` and `secr` transform a function with two arguments into a curryied version
    - think in terms of functions and not in terms of invocation

    val inverse = (secl 1.0 fn (r, q) => r/q);
    inverse(-4);
    % => 1/-4 = 4

    (secr List.take 3);
    % => fn l => List.take(l, 3)
    ((secr List.take 3) [1,2,3,4,5,6,7,8,9]);
    % => [1,2,3]

### Composition of functions (FP-6.6)

  - expressions over functions 
  - klassische Kombinatoren (S K I): All functions of the lambda calculus can be written without variables with "klassische Kombinatoren" K and S
    => That is all we have to now about "klassische Kombinatoren" in the exam

    fun o (f, g) x => f(g x);
    % <=> 
    infix o;
    fun (f o g) x = f(g x);

    fn x => (3 - x) * 5
    % <=>
    (secr op* 5) o (secl 3 op-)
    % => algebraic formulation without variables

    (repeat secr op/ 2.0 3) (800.0)
    % => (((800 / 2) / 2) / 2)

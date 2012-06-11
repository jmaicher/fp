Functional Programming - Lecture: 2012-06-04
============================================

7. Infinite lists (lazy lists)
------------------------------

### 7.1 Principals of infinite lists

list datatype:

  datatype 'a list = nil | :: of 'a x 'a list

sequence datatype (lazy):

  datatype seq = Nil | Cons of 'a x (unit -> 'a seq)

  - (unit -> 'a seq) is a function to calculate the rest of the list (if called).
  - sequence must not terminate, but can.

  alternative definition of the sequence datatype without alternative:

    datatype 'a seq = Cons of 'a (unit -> 'a seq)

Observations:
  - recursion without break can be useful
  - first element of the flow will always be calculated (lazier is possible: 7.13)
  - flows cannot be reversed (unless they are finite)

### Example: Flow generator and consumer

`from` generates integers starting with the given argument in increasing order.
`take` consumes a flow and takes the given number of elements.

[take 2]<--------[from 30]
=> [30, 31]

take(from 30, 2)
evaluation: take(Cons(30, fn() => from (30 + 1), 1)

= 30::take(from(30 + 1), 1)
evaluation: 30::take(Cons(31, fn() => from (30 + 1 + 1)), 1)

= 30::31::take(from(30 + 1 + 1), 0)
evaluation: 30::31::take(Cons(32, fn() => from(30 + 1 + 1 + 1)), 0)

= 30::31::nil
=> [31, 32]

### Append for flows?

append(fl, ow)

Doesn't make sense when fl is infinite.

### Functionals (Funktionale) for flows

Functions like `square` (see slide 7.4) can be generalized for flows like they can be generalized
for lists with the `map` function.

Slide 7.5: `iterate` is written in curry form

Example usage of `iterate`:

  fun half *k* = iterates(secr op/ 2.0) *k*;
  <=> fun half = iterates(secr op/ 2.0);

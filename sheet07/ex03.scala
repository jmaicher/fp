/** ex03) */

def merge (xs : Stream[Int], ys : Stream[Int]) : Stream[Int] = {
  if(xs.head == ys.head) Stream.cons(xs.head, merge(xs.tail, ys.tail))
  else if(xs.head < ys.head) Stream.cons(xs.head, merge(xs.tail, ys))
  else Stream.cons(ys.head, merge(xs, ys.tail))
}

def hamming : Stream[Int] = {
  Stream.cons(1, merge(hamming map(_*2), merge(hamming map(_*3), hamming map(_*5))))
}

hamming take 100 print

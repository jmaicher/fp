/** fib */

def pfib (a : Int, b : Int) : Stream[Int] =
  Stream.cons(a, pfib(b, a + b))

val fibs = pfib(1,1)

abstract class Person
case class King extends Person
case class Knight(name : String) extends Person

def title (p : Person) : String = p match {
  case King() => "Majesty, the King"
  case Knight(n) => "Sir " + n
}

def myMap[A,B] (f: A => B) (l : List[A]) : List[B] = l match {
  case List() => List()
  case (x::xs) => f(x)::myMap (f) (xs)
}

val guestList = List(King(), Knight("Julian"))
myMap (title) (guestList)

def from (k : Int) : Stream[Int] =
  Stream.cons(k, from (k + 1))

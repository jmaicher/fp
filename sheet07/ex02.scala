/** ex02) */

def sum (list : List[Int]) : Int = list.foldLeft (0) ((e, x) => x + e)
val it = sum(List(1,3,5,7))

/** a) */
def length (list : List[Any]) : Int = list.foldLeft (0) ((e, _) => e + 1)

/** b) */
val words = List("Functional", "Programming", "in", "Scala")
def join (list : List[String]) : String = {
  /** list.foldLeft ("") ((e, x) => if (e == "") x else  e + " " + x) */
  list.tail.foldLeft (list.head) ((a : String, b : String) => a + " " + b)
}
val it = join(words)


/** c) */
def last[A] (list : List[A]) : A = {
  list.foldLeft (list.head) ((_, x) => x)
}
val it = last (List(1,2,3,4,5,6,7,8,9))

/** d) */
def contains[A] (list : List[A], a : A) : Boolean = {
  list.foldLeft (false) ((result : Boolean, b : A) => result || (a == b))
}
val it = contains(List(1,2,3,4,5), 4)
val it = contains(List(1,2,3,4,5), 6)

/** e) */
def unique[A] (list : List[A]) : List[A] = {
  list.foldLeft (List[A]()) ((result : List[A], x : A) => if (contains(result, x)) result else result ::: List(x))
}
val it = unique(List(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9))

/** f) */
def myMap[A,B] (list : List[A], f : A => B) : List[B] = {
  list.foldLeft (List[B]()) ((result : List[B], x : A) => result ::: List(f(x)))
}
val it = myMap(List(1,2,3,4,5,6,7,8,9), ((x : Int) => x - 1))

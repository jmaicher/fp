/** ex01) */

/** a) */
def fac(n : Int) : Int = if (n == 0) 1 else n * fac(n-1)

/** b) */
def divTest(a : Int) : (Int => Boolean) = (b : Int) => (a % b == 0)
val divTest10 = divTest(10)

/** c) */
def divTestCurry (a : Int) (b : Int) : Boolean = a % b == 0
val divTestCurry20 = divTestCurry (20) _

/** d) */
/** o (f) (g) (x) => f(g(x)) */
def o[A, B, C] (f : B => C) (g : A => B) (x : A) : C = f(g(x))

def inc (x : Int) : Int = x + 1
def pow (x : Int) (k : Int) : Int = {
  if (k == 0) 1
  else if (k == 1) x
  else if (k % 2 == 0) pow (x*x) (k/2)
  else x * pow(x*x) (k/2)
}

val incPow = o (inc) (pow (2) (_ : Int)) _;

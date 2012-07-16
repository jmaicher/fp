/** ex04) */

/** a) & b) */

abstract class Term
case class Num(x : Int) extends Term
case class Plus(x : Term, y : Term) extends Term
case class Minus(x : Term, y : Term) extends Term
case class Times(x : Term, y : Term) extends Term
case class Div(x : Term, y : Term) extends Term

/** (3 + (5 - 2)) / (1 * 2) */
val t1 = Div(Plus(Num(3), Minus(Num(5), Num(2))), Times(Num(1), Num(2)))

def eval (t : Term) : Int = t match {
  case Num(x) => x
  case Plus(t1, t2) => eval(t1) + eval(t2)
  case Minus(t1, t2) => eval(t1) - eval(t2)
  case Times(t1, t2) => eval(t1) * eval(t2)
  case Div(t1, t2) => eval(t1) / eval(t2)
}

val it = eval(t1)

/** c) */

def postfix(t : Term) : String = t match {
  case Num(x) => x.toString
  case Plus(t1, t2) => postfix(t1) + " " + postfix(t2) + " +"
  case Minus(t1, t2) => postfix(t1) + " " + postfix(t2) + " -"
  case Times(t1, t2) => postfix(t1) + " " + postfix(t2) + " *"
  case Div(t1, t2) => postfix(t1) + " " + postfix(t2) + " /"
}

val it = postfix(t1)

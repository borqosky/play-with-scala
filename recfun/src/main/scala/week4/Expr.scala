package week4

/**
  * Created by wborkows on 11/12/2016.
  */
trait Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def isVar: Boolean
  def varValue: String
  def isProduct: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr

  def eval: Int = this match {
    case Number(n) => numValue
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr {
  def varValue = ""
  def isNumber = true
  def isSum = false
  def isVar = false
  def isProduct = false
  def numValue = n
  def leftOp = throw new Error("Number left.on")
  def rightOp = throw new Error("Number right.on")
}

case class Sum(e1: Expr, e2: Expr) extends Expr {
  def varValue = ""
  def isNumber = false
  def isSum = true
  def isVar = false
  def isProduct = false
  def numValue = throw new Error("Sum nan.value")
  def leftOp = e1
  def rightOp = e2
}

case class Prod(e1: Expr, e2: Expr) extends Expr {
  def varValue = ""
  def isNumber = false
  def isSum = false
  def isVar = false
  def isProduct = true
  def numValue = throw new Error("Product nan.value")
  def leftOp = e1
  def rightOp = e2
}

case class Var(x: String) extends Expr {
  def isNumber = false
  def varValue = x
  def isVar = true
  def isSum = false
  def isProduct = false
  def numValue = throw new Error("Var nan.value")
  def leftOp = throw new Error("Var left.on")
  def rightOp = throw new Error("Var right.on")
}

object Expr {
  def show(e: Expr): String = e match {
    case Number(n) => e.numValue.toString
    case Var(x) => e.varValue
    case Prod(Sum(e1, e2), e3) => "(" + show(e1) + " + " + show(e2) + ")" + " * " + show(e3)
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) => show(e1) + " * " + show(e2)
  }
}
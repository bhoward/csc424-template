package exprprob

case class Const(value: Int)
case class Add[L, R](left: L, right: R)

trait Expr[T]

given Expr[Const] with {}
given [L: Expr, R: Expr]: Expr[Add[L, R]] with {}

// Add eval operation

trait Eval[T: Expr]:
  def eval(t: T): Int

given Eval[Const] with
  def eval(c: Const): Int = c.value

given [L: Expr, R: Expr](
    using leftEval: Eval[L], rightEval: Eval[R]): Eval[Add[L, R]] with
  def eval(a: Add[L, R]): Int = leftEval.eval(a.left) + rightEval.eval(a.right)

// Add neg variant

case class Neg[T](t: T)

given [T: Expr]: Expr[Neg[T]] with {}

given [T: Expr](using subEval: Eval[T]): Eval[Neg[T]] with
  def eval(a: Neg[T]): Int = -subEval.eval(a.t)

// Add show operation (as an extension method, for variety)

trait Show[T: Expr]:
  extension (t: T) def show: String

given Show[Const] with
  extension (c: Const) def show: String = c.value.toString

given [L: Expr, R: Expr](
    using leftShow: Show[L], rightShow: Show[R]): Show[Add[L, R]] with
  extension (a: Add[L, R]) def show: String = s"(${leftShow.show(a.left)} + ${rightShow.show(a.right)})"

given [T: Expr](using subShow: Show[T]): Show[Neg[T]] with
  extension (a: Neg[T]) def show: String = s"-${subShow.show(a.t)}"

// Example of use:

@main def exprDemo: Unit = {
  def eval[T: Expr](t: T)(using theEval: Eval[T]): Int = theEval.eval(t)

  val e = Neg(Add(Const(17), Neg(Const(59))))
  println(eval(e))
  println(e.show)
}
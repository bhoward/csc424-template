// Based on https://www.slideshare.net/slideshow/the-expression-problem-part-2/251441361
// which was based on an earlier presentation by Ralf Laemmel using Haskell

package exprprob

trait Expr[T]

// Initial list of variants

case class Const(value: Int)
case class Add[L, R](left: L, right: R)

given Expr[Const] with {}
given [L:Expr, R:Expr]: Expr[Add[L, R]] with {}

// Add eval operation

trait Eval[T:Expr]:
  def eval(t: T): Int

given Eval[Const] with
  def eval(c: Const): Int = c.value

given [L:Expr:Eval, R:Expr:Eval]: Eval[Add[L, R]] with
  def eval(a: Add[L, R]): Int = {
    val leftEval = summon[Eval[L]].eval
    val rightEval = summon[Eval[R]].eval
    leftEval(a.left) + rightEval(a.right)
  }

// Add neg variant

case class Neg[T](t: T)

given [T:Expr]: Expr[Neg[T]] with {}

given [T:Expr:Eval]: Eval[Neg[T]] with
  def eval(a: Neg[T]): Int = {
    val argEval = summon[Eval[T]].eval
    -argEval(a.t)
  }

// Add show operation (as an extension method, for variety)

trait Show[T:Expr]:
  extension (t: T) def show: String

given Show[Const] with
  extension (c: Const) def show: String = c.value.toString

given [L:Expr:Show, R:Expr:Show]: Show[Add[L, R]] with
  extension (a: Add[L, R])
    def show: String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} + ${rightShow(a.right)})"
    }

given [T:Expr:Show]: Show[Neg[T]] with
  extension (a: Neg[T])
    def show: String = {
      val argShow = summon[Show[T]].show
      s"-${argShow(a.t)}"
    }

// Add mul variant

case class Mul[L, R](left: L, right: R)

given [L:Expr, R:Expr]: Expr[Mul[L, R]] with {}

given [L:Expr:Eval, R:Expr:Eval]: Eval[Mul[L, R]] with
  def eval(t: Mul[L, R]): Int = {
    val leftEval = summon[Eval[L]].eval
    val rightEval = summon[Eval[R]].eval
    leftEval(t.left) * rightEval(t.right)
  }

given [L:Expr:Show, R:Expr:Show]: Show[Mul[L, R]] with
  extension (a: Mul[L, R])
    def show: String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} * ${rightShow(a.right)})"
    }

// Example of use:

@main def exprDemo: Unit = {
  def eval[T:Expr:Eval](t: T): Int = summon[Eval[T]].eval(t)

  val e = Neg(Add(Const(15), Neg(Mul(Const(19), Const(3)))))
  println(eval(e))
  println(e.show)

// The following does not work because there is no type "Expr",
// only a type class Expr[T] giving "evidence" that T is an expression
//   def foo(n: Int): Expr = n match
//     case 0 => Const(0)
//     case _ => Add(foo(n - 1), Const(1))

//   val x = foo(4)
//   println(x)
//   println(eval(x))
//   println(x.show)
}
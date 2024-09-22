// Based on https://www.slideshare.net/slideshow/the-expression-problem-part-2/251441361
// which was based on an earlier presentation by Ralf Laemmel using Haskell

package exprprob2

trait Expr

// Initial list of variants

case class Const(value: Int) extends Expr
case class Add[L <: Expr, R <: Expr](left: L, right: R) extends Expr

// Add eval operation

trait Eval[E <: Expr]:
  def eval(e: E): Int

object Eval:
  given Eval[Const] with
    def eval(c: Const): Int = c.value

  given [L <: Expr : Eval, R <: Expr : Eval]: Eval[Add[L, R]] with
    def eval(a: Add[L, R]): Int = {
      val leftEval = summon[Eval[L]].eval
      val rightEval = summon[Eval[R]].eval
      leftEval(a.left) + rightEval(a.right)
    }

// Add neg variant

case class Neg[E <: Expr](e: E) extends Expr

object Neg:
  given [T <: Expr : Eval]: Eval[Neg[T]] with
    def eval(a: Neg[T]): Int = {
      val argEval = summon[Eval[T]].eval
      -argEval(a.e)
    }

// Add show operation (as an extension method, for variety)

trait Show[E <: Expr]:
  def show(e: E): String

object Show:
  given Show[Const] with
    def show(c: Const): String = c.value.toString

  given [L <: Expr : Show, R <: Expr : Show]: Show[Add[L, R]] with
    def show(a: Add[L, R]): String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} + ${rightShow(a.right)})"
    }

  given [E <: Expr : Show]: Show[Neg[E]] with
    def show(a: Neg[E]): String = {
      val argShow = summon[Show[E]].show
      s"-${argShow(a.e)}"
    }

// Add mul variant

case class Mul[L <: Expr, R <: Expr](left: L, right: R) extends Expr

object Mul:
  given [L <: Expr : Eval, R <: Expr : Eval]: Eval[Mul[L, R]] with
    def eval(t: Mul[L, R]): Int = {
      val leftEval = summon[Eval[L]].eval
      val rightEval = summon[Eval[R]].eval
      leftEval(t.left) * rightEval(t.right)
    }

  given [L <: Expr : Show, R <: Expr : Show]: Show[Mul[L, R]] with
    def show(a: Mul[L, R]): String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} * ${rightShow(a.right)})"
    }

// Add extension syntax
object ExprSyntax:
  extension [E <: Expr: Eval](e: E) def eval: Int = summon[Eval[E]].eval(e)
  extension [E <: Expr: Show](e: E) def show: String = summon[Show[E]].show(e)

// Example of use:

@main def exprDemo2: Unit = {
  import ExprSyntax.*

  val e = Neg(Add(Const(15), Neg(Mul(Const(19), Const(3)))))
  println(e.eval)
  println(e.show)

  // These also work?
  println(eval(e))
  println(show(e))

  // In this version, we can write foo: Int => Expr because Expr
  // is a real type
  def foo(n: Int): Expr = n match
    case 0 => Const(0)
    case _ => Add(foo(n - 1), Const(1))

  val x = foo(4)
  println(x)
  // However, the following are still not possible, because just having
  // a value of type Expr does not give us access at compile time to the
  // appropriate evidence to resolve which instance it is:
  // println(x.eval)
  // println(x.show)
  // println(eval(x))
  // println(show(x))
}
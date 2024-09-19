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

given [T <: Expr : Eval]: Eval[Neg[T]] with
  def eval(a: Neg[T]): Int = {
    val argEval = summon[Eval[T]].eval
    -argEval(a.e)
  }

// Add show operation (as an extension method, for variety)

trait Show[E <: Expr]:
  extension (e: E) def show: String

given Show[Const] with
  extension (c: Const) def show: String = c.value.toString

given [L <: Expr : Show, R <: Expr : Show]: Show[Add[L, R]] with
  extension (a: Add[L, R])
    def show: String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} + ${rightShow(a.right)})"
    }

given [E <: Expr : Show]: Show[Neg[E]] with
  extension (a: Neg[E])
    def show: String = {
      val argShow = summon[Show[E]].show
      s"-${argShow(a.e)}"
    }

// Add mul variant

case class Mul[L <: Expr, R <: Expr](left: L, right: R) extends Expr

given [L <: Expr : Eval, R <: Expr : Eval]: Eval[Mul[L, R]] with
  def eval(t: Mul[L, R]): Int = {
    val leftEval = summon[Eval[L]].eval
    val rightEval = summon[Eval[R]].eval
    leftEval(t.left) * rightEval(t.right)
  }

given [L <: Expr : Show, R <: Expr : Show]: Show[Mul[L, R]] with
  extension (a: Mul[L, R])
    def show: String = {
      val leftShow = summon[Show[L]].show
      val rightShow = summon[Show[R]].show
      s"(${leftShow(a.left)} * ${rightShow(a.right)})"
    }

// Example of use:

@main def exprDemo2: Unit = {
  def eval[E <: Expr: Eval](e: E): Int = summon[Eval[E]].eval(e)

  val e = Neg(Add(Const(15), Neg(Mul(Const(19), Const(3)))))
  println(eval(e))
  println(e.show)

  // In this version, we can write foo: Int => Expr because Expr
  // is a real type
  def foo(n: Int): Expr = n match
    case 0 => Const(0)
    case _ => Add(foo(n - 1), Const(1))

  val x = foo(4)
  println(x)
  // However, the following are still not possible, because just having
  // a value of type Expr does not give us access at compile time to the
  // appropriate evidence to resolve which instance it is
  // println(eval(x))
  // println(x.show)
}
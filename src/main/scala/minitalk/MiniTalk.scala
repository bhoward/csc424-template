package minitalk

import scala.io.StdIn.readLine
import scala.annotation.tailrec

/**
  * This is a very simple REPL (read-eval-print loop) for MiniTalk.
  */
@main def miniTalk() = {
  val predef = Map(
    "true" -> TrueValue,
    "false" -> FalseValue,
    "nil" -> NullValue
  )

  type M[T] = Either[String, T]
  object M:
    def pure[T](t: T): M[T] = Right(t)

    def lookup[T](name: String, default: T) =
      pure(predef.getOrElse(name, default))

    // extension [T](m: M[T])
    //   def map[U](f: T => U): M[U] = f(m)

    //   def flatMap[U](f: T => M[U]): M[U] = f(m)

    extension [T](xs: Seq[T])
      def traverse[U](f: T => M[U]): M[Seq[U]] = ???

  import M.*

  /**
    * Evaluate a MiniTalk abstract syntax tree and produce an object
    *
    * @param expr an expression to be evaluated
    * @return the Value representing the resulting object
    */
  def eval(expr: Expr): M[Value] = {
    import Expr.*

    expr match
      case IntLit(value) =>
        pure(IntValue(value))

      case RealLit(value) =>
        pure(RealValue(value))

      case CharLit(value) =>
        pure(CharValue(value))

      case StrLit(value) =>
        pure(StrValue(value))

      case Ident(name) =>
        lookup(name, NullValue)

      case UnOp(left, op) =>
        for
          receiver <- eval(left)
        yield
          receiver.receive(Message(op, Nil))

      case BinOp(left, op, right) =>
        for
          receiver <- eval(left)
          v <- eval(right)
        yield
          receiver.receive(Message(op, Seq(v)))

      case KeyOp(left, op, args) =>
        for
          receiver <- eval(left)
          vs <- args.traverse(eval)
        yield
          receiver.receive(Message(op, vs))

      case Assign(left, right) =>
        // TODO
        eval(right)

      case Block(exprs) =>
        pure(BlockValue(() =>
          for
            vs <- exprs.traverse(eval)
          yield
            if vs.nonEmpty then vs.last else NullValue
        ))
  }

  @tailrec
  def loop: Unit = {
    print("> ")
    val input = readLine()

    if input == null || input == "exit" then
      println("Goodbye")
    else if input == "" then
      loop
    else
      Parser(input) match
        case Right(expr) =>
          val result = eval(expr) // TODO run the M[] here
          println(result)
        case Left(message) =>
          println("Error: " + message)
      loop
  }

  loop
}

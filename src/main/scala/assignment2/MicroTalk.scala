package assignment2

import scala.io.StdIn.readLine
import scala.annotation.tailrec

/**
  * This is a very simple REPL (read-eval-print loop) for MicroTalk.
  */
@main def microTalk() = {
  val predef = Map(
    "true" -> TrueValue,
    "false" -> FalseValue,
    "nil" -> NullValue
  )

  /**
    * Evaluate a MicroTalk abstract syntax tree and produce an object
    *
    * @param expr an expression to be evaluated
    * @return the Value representing the resulting object
    */
  def eval(expr: Expr): Value = {
    import Expr.*

    expr match
      case IntLit(value) =>
        IntValue(value)

      case RealLit(value) =>
        RealValue(value)

      case CharLit(value) =>
        CharValue(value)

      case StrLit(value) =>
        StrValue(value)

      case Ident(name) =>
        predef.getOrElse(name, NullValue)

      case UnOp(left, op) =>
        val receiver = eval(left)
        receiver.receive(Message(op, Nil))

      case BinOp(left, op, right) =>
        val receiver = eval(left)
        val v = eval(right)
        receiver.receive(Message(op, Seq(v)))

      case KeyOp(left, op, args) =>
        val receiver = eval(left)
        val vs = args.map(eval)
        receiver.receive(Message(op, vs))

      case Block(exprs) =>
        BlockValue(() =>
          val vs = exprs.map(eval)
          if vs.nonEmpty then vs.last else NullValue
        )
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
          val result = eval(expr)
          println(result)
        case Left(message) =>
          println("Error: " + message)
      loop
  }

  loop
}

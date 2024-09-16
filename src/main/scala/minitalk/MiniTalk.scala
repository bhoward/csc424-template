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

  import C.*

  /**
    * Evaluate a MiniTalk abstract syntax tree and produce an object
    *
    * @param expr an expression to be evaluated
    * @return the Value representing the resulting object
    */
  def eval(expr: Expr): C[Value] = {
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
          result <- receiver.receive(Message(op, Nil))
        yield
          result

      case BinOp(left, op, right) =>
        for
          receiver <- eval(left)
          v <- eval(right)
          result <- receiver.receive(Message(op, Seq(v)))
        yield
          result

      case KeyOp(left, op, args) =>
        for
          receiver <- eval(left)
          vs <- args.traverse(eval)
          result <- receiver.receive(Message(op, vs))
        yield
          result

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
  def loop(state: State): Unit = {
    print("> ")
    val input = readLine()

    if input == null || input == "exit" then
      println("Goodbye")
    else if input == "" then
      loop(state)
    else
      Parser(input) match
        case Right(expr) =>
          eval(expr)(state) match
            case Right(result, state2) =>
              println(result)
              loop(state2)
            case Left(error) =>
              println("Error: " + error)
              loop(state)
        case Left(message) =>
          println("Error: " + message)
          loop(state)
  }

  loop(State(predef))
}
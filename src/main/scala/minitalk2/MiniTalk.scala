package minitalk2

import scala.io.StdIn.readLine
import scala.annotation.tailrec

import cats.*
import cats.implicits.*

/**
  * This is a very simple REPL (read-eval-print loop) for MiniTalk.
  */
@main def miniTalk2() = {
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
        IntValue(value).pure

      case RealLit(value) =>
        RealValue(value).pure

      case CharLit(value) =>
        CharValue(value).pure

      case StrLit(value) =>
        StrValue(value).pure

      case Ident(name) =>
        lookup(name)

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
        for
          result <- eval(right)
          _ <- update(left, result)
        yield
          result

      case Block(params, temps, exprs) =>
        closure(state => BlockValue((args: Seq[Value]) =>
          for
            save <- swapState(state)
            _ <- pushScope
            _ <- params.zip(args).traverse(bind) // will stop at the shorter length
            _ <- temps.map(name => (name, NullValue)).traverse(bind)
            vs <- exprs.traverse(eval)
            _ <- popScope
            _ <- swapState(save)
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
          eval(expr).run(state) match
            case Right((state2, result)) =>
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

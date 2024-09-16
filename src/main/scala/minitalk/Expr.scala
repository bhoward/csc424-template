package minitalk

/**
  * Abstract Syntax for MicroTalk
  */
enum Expr:
  case IntLit(value: Int)
  case RealLit(value: Double)
  case CharLit(value: Char)
  case StrLit(value: String)
  case Ident(name: String)
  case UnOp(left: Expr, op: String)
  case BinOp(left: Expr, op: String, right: Expr)
  case KeyOp(left: Expr, op: String, args: Seq[Expr])
  case Assign(left: String, right: Expr)
  case Block(exprs: Seq[Expr])

object Expr:
  def makeNumber(literal: String): Expr = {
    if literal.contains(".") then
      RealLit(literal.toDouble)
    else
      IntLit(literal.toInt)
  }

  def makeChar(literal: String): Expr = {
    CharLit(literal(0))
  }


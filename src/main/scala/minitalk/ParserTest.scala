package minitalk

@main def parserTest() = {
  import TestParsers.*
  import TestParsers.given
  import Expr.*

  val test = "12 + 34 * 5 mod 678"

  object TestParsers extends Parsers:
    def whitespace: P[Any] =
      "[ \t\n]*".r

  def expr: P[Expr] =
    (term ~ (addop ~ term).rep).map {
      case (f, ofs) => ofs.foldLeft(f) {
        case (e, (o, f2)) => BinOp(e, o, f2)
      }
    }
  
  def term: P[Expr] =
    (factor ~ (mulop ~ factor).rep).map {
      case (f, ofs) => ofs.foldLeft(f) {
        case (e, (o, f2)) => BinOp(e, o, f2)
      }
    }
  
  def factor: P[Expr] =
    num.map(Expr.makeNumber) | ("(" ~> expr <~ ")")

  def addop: P[String] =
    "+" | "-"

  def mulop: P[String] =
    "*" | "/" | "mod"

  def num: P[String] =
    "\\d+".r

  (expr ~ end)(test) match
    case Left(message) => println(message)
    case Right(e, rest) => println(s"Found $e followed by $rest")
}

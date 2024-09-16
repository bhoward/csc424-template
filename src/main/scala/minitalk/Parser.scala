package minitalk

/**
  * This is a parser for MiniTalk, a small subset of SmallTalk.
  */
object Parser:
  import MTParsers.*
  import MTParsers.given
  import Expr.*

  object MTParsers extends Parsers:
    def whitespace: P[Any] = "[ \t\n]*".r
  
  def apply(input: String): Either[String, Expr] = 
    top(input).map(_._1)
    
  def top: P[Expr] = expr <~ end

  def expr: P[Expr] = ((ident <~ ":=").rep ~ expr2)
    .map { case (ids, e) => ids.foldRight(e) { case (left, right) => Assign(left, right) }}

  def expr2: P[Expr] = binexpr | keyexpr | primary

  def binexpr: P[Expr] = (primary ~ binmsg.rep1)
    .map { case (e, bins) => bins.foldLeft(e) { case (e1, (op, e2)) => BinOp(e1, op, e2) }}

  def binmsg: P[(String, Expr)] = op ~ primary

  def keyexpr: P[Expr] = (keyexpr2 ~ keymsg)
    .map { case (e, keys) => KeyOp(e, keys.map(_._1).mkString, keys.map(_._2)) }

  def keyexpr2: P[Expr] = binexpr | primary

  def keymsg: P[Seq[(String, Expr)]] = (keySelector ~ keyexpr2).rep1

  def primary: P[Expr] = (unit ~ ident.rep)
    .map { case (u, ids) => ids.foldLeft(u)(UnOp(_, _)) }

  def unit: P[Expr] = id | literal | block | ("(" ~> expr <~ ")")

  def id: P[Expr] = ident.map(Ident(_))

  def literal: P[Expr] =
    number.map(Expr.makeNumber) |
    char.map(Expr.makeChar) |
    string.map(StrLit(_))

  def block: P[Expr] = "[" ~> expr.repsep(".").map(Block(_)) <~ "]"

  // Lexical Syntax
  def keySelector: P[String] = "[A-Za-z][A-Za-z0-9]*:".r

  def ident: P[String] = "[A-Za-z][A-Za-z0-9]*(?![A-Za-z0-9:])".r

  def number: P[String] = "-?[0-9]+(\\.[0-9]+)?".r

  def char: P[String] = "$.".r

  def string: P[String] = "'" ~> "([^']|'')*".r <~ "'"

  def op: P[String] = "[+\\-*/~|,<>=&@?\\\\%]+".r

package assignment2

import fastparse._, SingleLineWhitespace._

/**
  * This is a parser for MicroTalk, a very small subset of SmallTalk.
  * It is implemented using the FastParse combinator parsing library:
  * see https://com-lihaoyi.github.io/fastparse/
  */
object Parser:
  import Expr.*

  def apply(input: String): Either[String, Expr] = {
    parse(input, { case given P[_] => Parser.top }) match
    case Parsed.Success(value, _) => Right(value)
    case result: Parsed.Failure => Left(result.msg)
  }
    
  def top[$: P]: P[Expr] = P ( Start ~ expr ~ End )

  def expr[$: P]: P[Expr] = P( binexpr | keyexpr | primary )

  def binexpr[$: P]: P[Expr] = P( primary ~ binmsg.rep(1) )
    .map { case (e, bins) => bins.foldLeft(e) { case (e1, (op, e2)) => BinOp(e1, op, e2) }}

  def binmsg[$: P]: P[(String, Expr)] = P( op ~ primary )

  def keyexpr[$: P]: P[Expr] = P( keyexpr2 ~ keymsg )
    .map { case (e, keys) => KeyOp(e, keys.map(_._1).mkString, keys.map(_._2)) }

  def keyexpr2[$: P]: P[Expr] = P( binexpr | primary )

  def keymsg[$: P]: P[Seq[(String, Expr)]] = P( (keySelector ~ keyexpr2).rep(1) )

  def primary[$: P]: P[Expr] = P( unit ~ ident.rep )
    .map { case (u, ids) => ids.foldLeft(u)(UnOp(_, _)) }

  def unit[$: P]: P[Expr] = P( id | literal | block | "(" ~ expr ~ ")" )

  def id[$: P]: P[Expr] = P( ident.map(Ident(_)) )

  def literal[$: P]: P[Expr] = P(
    number.map(Expr.makeNumber) |
    char.map(Expr.makeChar) |
    string.map(StrLit(_))
  )

  def block[$: P]: P[Expr] = P( "[" ~ expr.rep(sep = ".").map(Block(_)) ~ "]" )

  // Lexical Syntax
  def keySelector[$: P] = P( CharIn("A-Za-z").! ~~ CharsWhileIn("A-Za-z0-9").?.! ~~ ":" )
    .map { case (init, rest) => init + rest + ":" }

  def ident[$: P] = P( CharIn("A-Za-z").! ~~ CharsWhileIn("A-Za-z0-9").?.! ~~ !":" )
    .map { case (init, rest) => init + rest }

  def number[$: P] = P( ("-".? ~~ CharsWhileIn("0-9") ~~ ("." ~~ CharsWhileIn("0-9")).?).! )

  def char[$: P] = P( "$" ~~ AnyChar.! )

  def string[$: P] = P( ("'" ~~ CharsWhile(_ != '\'', 0).! ~~ "'").repX(1).map(_.mkString("'")) )

  def op[$: P] = P( CharsWhileIn("+\\-*/~|,<>=&@?\\\\%").! )

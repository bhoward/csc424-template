package assignment3

import java.io.File
import fastparse.*, fastparse.NoWhitespace.*
import scala.util.Using, scala.io.Source

/**
  * This is a parser for a small subset of HTML.
  * It is implemented using the FastParse combinator parsing library:
  * see https://com-lihaoyi.github.io/fastparse/
  */
object Parser:
  def apply(input: String): Either[String, DOM] = {
    parse(input, { case given P[_] => Parser.top }) match
      case Parsed.Success(value, _) => Right(value)
      case result: Parsed.Failure => Left(result.msg)
  }

  def apply(inFile: File): Either[String, DOM] = {
    Using.resource(Source.fromFile(inFile)) { input =>
      parse(input.getLines(), { case given P[_] => Parser.top }) match
        case Parsed.Success(value, _) => Right(value)
        case result: Parsed.Failure => Left(result.msg)
    }
  }
  
  def top[$: P]: P[DOM] = P ( Start ~ ws ~ element ~ ws ~ End )

  def dom[$: P]: P[DOM] = P ( element | text )

  def element[$: P]: P[DOM] = P (
      selfTag.map(tag => Element(tag.name, tag.attributes, Seq()))
    | (for
        tag <- openTag
        children <- dom.rep
        _ <- closeTag(tag)
      yield
        Element(tag.name, tag.attributes, children.filter(dom => !dom.isEmpty))
      )
    )

  def text[$: P]: P[DOM] = P ( CharsWhile(c => c != '<').! )
    .map(s => Text(s))

  def openTag[$: P]: P[Tag] = P ( "<" ~ ws ~ name ~ (ws ~ attribute).rep ~ ws ~ ">" )
    .map { case (name, attributes) => Tag(name, attributes) }

  def closeTag[$: P](tag: Tag): P[Unit] = P ( "</" ~ ws ~ IgnoreCase(tag.name) ~ ws ~ ">" )

  def selfTag[$: P]: P[Tag] = P ( "<" ~ ws ~ name ~ (ws ~ attribute).rep ~ ws ~ "/>" )
    .map { case (name, attributes) => Tag(name, attributes) }

  def name[$: P]: P[String] = P ( CharsWhile(c => Character.isLetterOrDigit(c)).! )
    .map(s => s.toLowerCase())

  def value[$: P]: P[String] = P ( CharsWhile(c => c != '"').! )

  def attribute[$: P]: P[Attribute] = P ( name ~ ws ~ "=" ~ ws ~ "\"" ~ value ~ "\"" )
    .map { case (name, value) => Attribute(name, value) }

  def ws[$: P]: P[Unit] = P ( CharsWhileIn(" \t\n\r").? )

@main def parserTest(): Unit = {
  import scala.io.StdIn.readLine
  import scala.annotation.tailrec

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
        case Right(dom) =>
          println(dom)
        case Left(message) =>
          println("Error: " + message)
      loop
  }

  loop
}

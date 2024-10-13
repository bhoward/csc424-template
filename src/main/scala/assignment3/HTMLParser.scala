package assignment3

type ErrOr[T] = Either[String, T]

object HTMLParser:
  def apply(input: String): ErrOr[HTML] = {
    for
      dom <- Parser(input)
      html <- parseHTML(dom)
    yield
      html
  }

  def parseHTML(dom: DOM): ErrOr[HTML] = {
    dom match
      case Element("html", _, Seq(h, b)) =>
        for
          head <- parseHead(h)
          body <- parseBody(b)
        yield
          HTML(head, body)
      case _ =>
        Left("Expected <html> with two children")
  }

  def parseHead(dom: DOM): ErrOr[Head] = {
    dom match
      case Element("head", _, Seq(t)) =>
        for
          title <- parseTitle(t)
        yield
          Head(title)
      case _ =>
        Left("Expected <head> with one child")
  }

  def parseTitle(dom: DOM): ErrOr[Title] = {
    dom match
      case Element("title", _, Seq(Text(s))) =>
        Right(Title(s))
      case _ =>
        Left("Expected <title> element")
  }

  def parseBody(dom: DOM): ErrOr[Body] = {
    dom match
      case Element("body", _, children) =>
        for
          content <- collect(children, parseFlow)
        yield
          Body(content)
      case _ =>
        Left("Expected <body> element")
  }

  def parseFlow(dom: DOM): ErrOr[FlowContent] = {
    parseGrouping(dom) orElse parsePhrasing(dom)
  }

  def parseGrouping(dom: DOM): ErrOr[GroupingContent] = {
    import GroupingContent.*

    dom match
      case Element("h1", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H1(content)
      case Element("h2", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H2(content)
      case Element("h3", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H3(content)
      case Element("h4", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H4(content)
      case Element("h5", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H5(content)
      case Element("h6", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          H6(content)
      case Element("p", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          P(content)
      case Element("hr", _, Seq()) =>
        Right(HR)
      // TODO ol, ul, table
      case _ =>
        Left("Expected grouping element")
  }

  def parsePhrasing(dom: DOM): ErrOr[PhrasingContent] = {
    import PhrasingContent.*

    dom match
      case Element("em", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          Em(content)
      case Element("strong", _, children) =>
        for
          content <- collect(children, parsePhrasing)
        yield
          Strong(content)
      case Element("a", attributes, children) =>
        for
          href <- attributes.find(_.name == "href")
            .map(_.value).toRight("Expected href")
          content <- collect(children, parsePhrasing)
        yield
          A(href, content)
      case Element("br", _, Seq()) =>
        Right(BR)
      case Text(content) =>
        Right(Txt(content))
      case _ =>
        Left("Expected phrasing element")
  }

  def collect[T](children: Seq[DOM], parser: DOM => ErrOr[T]): ErrOr[Seq[T]] = {
    children.foldLeft(Right(Seq()): ErrOr[Seq[T]]) {
      case (accum, child) =>
        for
          xs <- accum
          x <- parser(child)
        yield
          xs :+ x
    }
  }

@main def htmlTest(): Unit = {
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
      HTMLParser(input) match
        case Right(html) =>
          println(html)
        case Left(message) =>
          println("ErrOr: " + message)
      loop
  }

  loop
}

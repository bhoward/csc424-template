package minitalk

import scala.util.matching.Regex

trait Parsers:
  type P[+T] = String => Either[String, (T, String)]

  def whitespace: P[Any]

  def succeed[T](result: T): P[T] =
    in => Right(result, in)

  def fail[T](message: String): P[T] =
    in => Left(message)

  def end: P[Unit] = {
    case "" => Right((), "")
    case s => Left(s"expected end of input at $s")
  }

  def satisfy(pred: Char => Boolean, message: String): P[Char] = {
    case "" =>
      Left(message)
    case s =>
      if pred(s.charAt(0)) then
        Right(s.charAt(0), s.substring(1))
      else
        Left(message)
  }

  def literal(ch: Char): P[Char] =
    satisfy(_ == ch, s"expected $ch")

  def sequence(seq: List[Char]): P[List[Char]] =
    seq match
      case Nil => succeed(Nil)
      case head :: tail => (literal(head) ~~ sequence(tail)).map(_ :: _)

  def string(s: String): P[String] =
    sequence(s.toList).map(_.mkString)

  def regex(r: Regex): P[String] =
    in => r.findPrefixMatchOf(in) match
      case None => Left(s"expected $r")
      case Some(m) => Right((m.matched, m.after.toString))
    
  given Conversion[String, P[String]] with
    def apply(s: String): P[String] = string(s)

  given Conversion[Regex, P[String]] with
    def apply(r: Regex): P[String] = regex(r)

  extension [T](p: P[T])
    def |(q: => P[T]): P[T] =
      in => p(in).orElse(q(in))

    def ~[T2](q: => P[T2]): P[(T, T2)] =
      in => for
        (o1, in1) <- p(in)
        (_, in2) <- whitespace(in1)
        (o2, rest) <- q(in2)
      yield
        ((o1, o2), rest)
    //   in => p(in).flatMap {
    //     case (o1, in1) => whitespace(in1).flatMap {
    //       case (_, in2) => q(in2).map {
    //         case (o2, rest) => ((o1, o2), rest)
    //       }
    //     }
    //   }
    
    def ~~[T2](q: => P[T2]): P[(T, T2)] =
      in => for
        (o1, in1) <- p(in)
        (o2, rest) <- q(in1)
      yield
        ((o1, o2), rest)

    def map[T2](f: T => T2): P[T2] =
      in => p(in).map {
        case (o, rest) => (f(o), rest)
      }

    def ? : P[Option[T]] =
      p.map(Some(_)) | succeed(None)

    def rep: P[List[T]] =
      rep1 | succeed(Nil)

    def rep1: P[List[T]] =
      (p ~ p.rep).map(_ :: _)

    def repX: P[List[T]] =
      repX1 | succeed(Nil)

    def repX1: P[List[T]] =
      (p ~~ p.repX).map(_ :: _)

    def repsep(q: => P[Any]): P[List[T]] =
      repsep1(q) | succeed(Nil)

    def repsep1(q: => P[Any]): P[List[T]] =
      (p ~ (q ~> p).rep).map(_ :: _)

    def <~[T2](q: => P[T2]): P[T] =
      (p ~ q).map(_._1)

    def ~>[T2](q: => P[T2]): P[T2] =
      (p ~ q).map(_._2)

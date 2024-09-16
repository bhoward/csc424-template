package minitalk

class State(env: Map[String, Value]):
  def lookup(name: String, default: Value) = env.getOrElse(name, default)

type Error = String

class C[T](c: State => Either[Error, (T, State)]):
  def apply(state: State): Either[Error, (T, State)] = c(state)

  def map[U](f: T => U): C[U] = C { state =>
    c(state) match
      case Left(error) => Left(error)
      case Right(t, state2) => Right(f(t), state2)
  }

  def flatMap[U](f: T => C[U]): C[U] = C { state =>
    c(state) match
      case Left(error) => Left(error)
      case Right(t, state2) => f(t)(state2)
  }

object C:
  def pure[T](t: T): C[T] = C { state =>
    Right(t, state)
  }

  def lookup(name: String, default: Value): C[Value] = C { state =>
    Right(state.lookup(name, default), state)
  }

  extension [T](xs: Seq[T])
    def traverse[U](f: T => C[U]): C[Seq[U]] = xs match
      case Seq() => pure(Seq())
      case head +: tail =>
        for
          u <- f(head)
          us <- tail.traverse(f)
        yield u +: us
    
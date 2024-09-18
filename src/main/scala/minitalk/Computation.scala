package minitalk

class Cell[T](var contents: T)

type Env = Map[String, Value]
type Scope = Map[String, Cell[Value]]
type Error = String

class State(predef: Env, scopes: List[Scope] = Nil):
  private def findCell(name: String): Option[Cell[Value]] = {
    def aux(ss: List[Scope]): Option[Cell[Value]] = {
      ss match
        case Nil => None
        case head :: tail =>
          if head.contains(name)
          then Some(head(name))
          else aux(tail)
    }

    aux(scopes)
  }

  def lookup(name: String): Either[Error, Value] = {
    findCell(name).map(cell => Right(cell.contents))
      .getOrElse(predef.get(name).map(value => Right(value))
        .getOrElse(Left(s"variable not found: $name")))
  }

  def bind(name: String, value: Value): Either[Error, State] = {
    scopes match
      case Nil => Left(s"no local scope for binding to $name")
      case head :: tail =>
        Right(State(predef, (head + (name -> Cell(value))) :: tail))
  }

  def update(name: String, value: Value): Either[Error, Unit] = {
    findCell(name). match
      case None => Left(s"variable not found: $name")
      case Some(cell) => Right(cell.contents = value)
  }

  def pushScope: State = State(predef, Map() :: scopes)

  def popScope: Either[Error, State] = {
    scopes match
      case Nil => Left("no local scope to pop")
      case head :: tail => Right(State(predef, tail))
  }

class C[+T](c: State => Either[Error, (T, State)]):
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

  def closure[T](f: State => T): C[T] = C { state =>
    Right(f(state), state)
  }

  def lookup(name: String): C[Value] = C { state =>
    state.lookup(name).map(value => (value, state))
  }

  def bind(name: String, value: Value): C[Unit] = C { state =>
    state.bind(name, value).map(state2 => ((), state2))
  }

  def update(name: String, value: Value): C[Unit] = C { state =>
    state.update(name, value).map(_ => ((), state))
  }

  def pushScope: C[Unit] = C { state =>
    Right((), state.pushScope)
  }

  def popScope: C[Unit] = C { state =>
    state.popScope.map(state2 => ((), state2))
  }

  def swapState(state2: State): C[State] = C{ state =>
    Right(state, state2)
  }

  extension [T](xs: Seq[T])
    def traverse[U](f: T => C[U]): C[Seq[U]] = xs match
      case Seq() => pure(Seq())
      case head +: tail =>
        for
          u <- f(head)
          us <- tail.traverse(f)
        yield u +: us
    
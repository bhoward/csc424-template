package minitalk2

import cats.*
import cats.implicits.*
import cats.data.StateT

class Cell[T](var contents: T)

type Env = Map[String, Value]
type Scope = Map[String, Cell[Value]]
type Error = String
type ErrorOr[T] = Either[Error, T]

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

  def lookup(name: String): ErrorOr[Value] = {
    findCell(name).map(cell => Right(cell.contents))
      .getOrElse(predef.get(name).map(value => Right(value))
        .getOrElse(Left(s"variable not found: $name")))
  }

  def bind(name: String, value: Value): ErrorOr[State] = {
    scopes match
      case Nil => Left(s"no local scope for binding to $name")
      case head :: tail =>
        Right(State(predef, (head + (name -> Cell(value))) :: tail))
  }

  def update(name: String, value: Value): ErrorOr[Unit] = {
    findCell(name). match
      case None => Left(s"variable not found: $name")
      case Some(cell) => Right(cell.contents = value)
  }

  def pushScope: State = State(predef, Map() :: scopes)

  def popScope: ErrorOr[State] = {
    scopes match
      case Nil => Left("no local scope to pop")
      case head :: tail => Right(State(predef, tail))
  }

type C[T] = StateT[ErrorOr, State, T]

object C:
  def closure[T](f: State => T): C[T] = StateT { state =>
    Right(state, f(state))
  }

  def lookup(name: String): C[Value] = StateT { state =>
    state.lookup(name).map(value => (state, value))
  }

  def bind(name: String, value: Value): C[Unit] = StateT { state =>
    state.bind(name, value).map(state2 => (state2, ()))
  }

  def update(name: String, value: Value): C[Unit] = StateT { state =>
    state.update(name, value).map(_ => (state, ()))
  }

  def pushScope: C[Unit] = StateT { state =>
    Right(state.pushScope, ())
  }

  def popScope: C[Unit] = StateT { state =>
    state.popScope.map(state2 => (state2, ()))
  }

  def swapState(state2: State): C[State] = StateT { state =>
    Right(state2, state)
  }
    
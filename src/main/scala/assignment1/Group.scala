package assignment1

class Group[T]:
  private var contents = List[T]()

  def add(x: T): Unit = {
    if !has(x)
    then contents = ???
  }

  def delete(x: T): Unit = {
    contents = ???
  }

  def has(x: T): Boolean = {
    ???
  }

object Group:
  def from[T](xs: Iterable[T]): Group[T] = {
    var result = Group[T]
    for x <- xs do
      ???
    result
  }

@main def test() = {
  val group = Group.from(List(10, 20))
  println(group.has(10))
  // → true
  println(group.has(30))
  // → false
  group.add(10)
  group.delete(10)
  println(group.has(10))
  // → false
}
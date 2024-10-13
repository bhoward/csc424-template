package assignment3

trait DOM:
  def isEmpty: Boolean

case class Element(name: String, attributes: Seq[Attribute], children: Seq[DOM]) extends DOM:
  def isEmpty: Boolean = false

case class Text(content: String) extends DOM:
  def isEmpty: Boolean = content.strip() == ""

case class Attribute(name: String, value: String)

case class Tag(name: String, attributes: Seq[Attribute])

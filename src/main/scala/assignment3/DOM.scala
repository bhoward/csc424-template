package assignment3

enum DOM:
  case Element(name: String, attributes: Seq[Attribute], children: Seq[DOM])
  case Text(content: String)

case class Attribute(name: String, value: String)

case class Tag(name: String, attributes: Seq[Attribute])

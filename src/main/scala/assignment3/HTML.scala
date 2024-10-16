package assignment3

case class HTML(head: Head, body: Body)

case class Head(title: Title)

case class Title(title: String)

case class Body(content: Seq[GroupingContent])

enum GroupingContent:
  case H1(children: Seq[PhrasingContent])
  case H2(children: Seq[PhrasingContent])
  case H3(children: Seq[PhrasingContent])
  case P(children: Seq[PhrasingContent])
  case OL(items: Seq[Item])
  case UL(items: Seq[Item])
  case HR

enum PhrasingContent:
  case Em(children: Seq[PhrasingContent])
  case Strong(children: Seq[PhrasingContent])
  case A(href: String, children: Seq[PhrasingContent])
  case Txt(text: String)

case class Item(children: Seq[PhrasingContent])

package assignment3

case class HTML(head: Head, body: Body)

case class Head(title: Title)

case class Title(title: String)

case class Body(content: Seq[FlowContent])

trait FlowContent

enum GroupingContent extends FlowContent:
  case H1(children: Seq[PhrasingContent])
  case H2(children: Seq[PhrasingContent])
  case H3(children: Seq[PhrasingContent])
  case H4(children: Seq[PhrasingContent])
  case H5(children: Seq[PhrasingContent])
  case H6(children: Seq[PhrasingContent])
  case P(children: Seq[PhrasingContent])
  case OL(items: Seq[Item])
  case UL(items: Seq[Item])
  case Table(rows: Seq[TableRow])
  case HR

enum PhrasingContent extends FlowContent:
  case Em(children: Seq[PhrasingContent])
  case Strong(children: Seq[PhrasingContent])
  case A(href: String, children: Seq[PhrasingContent])
  case Txt(text: String)
  case BR

case class Item(children: Seq[PhrasingContent])

case class TableRow(cells: Seq[TableCell])

case class TableCell(children: Seq[PhrasingContent])

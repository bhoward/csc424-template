package minitalk

case class Message(selector: String, args: Seq[Value])

/**
  * A Value represents a MicroTalk object. Since MicroTalk does not
  * (currently) provide a way to define new classes or methods, the
  * standard class library is implemented here.
  */
trait Value:
  def receive(msg: Message): Value = {
    msg match
      case Message("~=", Seq(that)) =>
        this.receive(Message("=", Seq(that)))
            .receive(Message("not", Nil))
      case Message(">=", Seq(that)) =>
        this.receive(Message("<", Seq(that)))
            .receive(Message("not", Nil))
      case Message(">", Seq(that)) =>
        that.receive(Message("<", Seq(this)))
      case Message("<=", Seq(that)) =>
        that.receive(Message(">=", Seq(this)))

      case _ =>
        println("Unknown message: " + msg + " received by " + this)
        NullValue
  }

case object NullValue extends Value:
  override def toString: String = "nil"

case class IntValue(number: Int) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): Value = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        IntValue(number + that)
      case Message("+", Seq(RealValue(that))) =>
        RealValue(number + that)

      case Message("-", Seq(IntValue(that))) =>
        IntValue(number - that)
      case Message("-", Seq(RealValue(that))) =>
        RealValue(number - that)

      case Message("*", Seq(IntValue(that))) =>
        IntValue(number * that)
      case Message("*", Seq(RealValue(that))) =>
        RealValue(number * that)

      case Message("/", Seq(IntValue(that))) =>
        IntValue(number / that)
      case Message("/", Seq(RealValue(that))) =>
        RealValue(number / that)

      case Message("%", Seq(IntValue(that))) =>
        IntValue(number % that)
      case Message("%", Seq(RealValue(that))) =>
        RealValue(number % that)

      case Message("**", Seq(IntValue(that))) =>
        IntValue(math.round(math.pow(number, that)).toInt)
      case Message("**", Seq(RealValue(that))) =>
        RealValue(math.pow(number, that))

      case Message("sqrt", Nil) =>
        RealValue(math.sqrt(number))

      case Message("asInteger", Nil) =>
        this
      case Message("asFloat", Nil) =>
        RealValue(number)
      case Message("asCharacter", Nil) =>
        CharValue(number.toChar)
      case Message("asString", Nil) =>
        StrValue(number.toString)

      case Message("=", Seq(IntValue(that))) =>
        BoolValue(number == that)
      case Message("=", Seq(RealValue(that))) =>
        BoolValue(number == that)

      case Message("<", Seq(IntValue(that))) =>
        BoolValue(number < that)
      case Message("<", Seq(RealValue(that))) =>
        BoolValue(number < that)

      case _ =>
        super.receive(msg)
  }

case class RealValue(number: Double) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): Value = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        RealValue(number + that)
      case Message("+", Seq(RealValue(that))) =>
        RealValue(number + that)

      case Message("-", Seq(IntValue(that))) =>
        RealValue(number - that)
      case Message("-", Seq(RealValue(that))) =>
        RealValue(number - that)

      case Message("*", Seq(IntValue(that))) =>
        RealValue(number * that)
      case Message("*", Seq(RealValue(that))) =>
        RealValue(number * that)

      case Message("/", Seq(IntValue(that))) =>
        RealValue(number / that)
      case Message("/", Seq(RealValue(that))) =>
        RealValue(number / that)

      case Message("%", Seq(IntValue(that))) =>
        RealValue(number % that)
      case Message("%", Seq(RealValue(that))) =>
        RealValue(number % that)

      case Message("**", Seq(IntValue(that))) =>
        RealValue(math.pow(number, that))
      case Message("**", Seq(RealValue(that))) =>
        RealValue(math.pow(number, that))

      case Message("sqrt", Nil) =>
        RealValue(math.sqrt(number))

      case Message("asInteger", Nil) =>
        IntValue(math.round(number).toInt)
      case Message("asFloat", Nil) =>
        this
      case Message("asString", Nil) =>
        StrValue(number.toString)

      case Message("=", Seq(IntValue(that))) =>
        BoolValue(number == that)
      case Message("=", Seq(RealValue(that))) =>
        BoolValue(number == that)

      case Message("<", Seq(IntValue(that))) =>
        BoolValue(number < that)
      case Message("<", Seq(RealValue(that))) =>
        BoolValue(number < that)

      case _ =>
        super.receive(msg)
  }

case class CharValue(char: Char) extends Value:
  override def toString: String = char.toString

  override def receive(msg: Message): Value = {
    msg match
      case Message("+", Seq(StrValue(that))) =>
        StrValue(char + that)

      case Message("*", Seq(IntValue(that))) =>
        StrValue(char.toString * that)

      case Message("asInteger", Nil) =>
        IntValue(char.toInt)
      case Message("asString", Nil) =>
        StrValue(char.toString)

      case Message("=", Seq(CharValue(that))) =>
        BoolValue(char == that)
      case Message("<", Seq(CharValue(that))) =>
        BoolValue(char < that)

      case _ =>
        super.receive(msg)
  }

case class StrValue(string: String) extends Value:
  override def toString: String = string

  override def receive(msg: Message): Value = {
    msg match
      case Message("+", Seq(CharValue(that))) =>
        StrValue(string + that)
      case Message("+", Seq(StrValue(that))) =>
        StrValue(string + that)

      case Message("*", Seq(IntValue(that))) =>
        StrValue(string * that)

      case Message("charAt:", Seq(IntValue(that))) =>
        CharValue(string(that))
      case Message("size", Nil) =>
        IntValue(string.length)

      case Message("asInteger", Nil) =>
        IntValue(string.toDouble.toInt)
      case Message("asFloat", Nil) =>
        RealValue(string.toDouble)
      case Message("asString", Nil) =>
        this

      case Message("=", Seq(StrValue(that))) =>
        BoolValue(string == that)
      case Message("<", Seq(StrValue(that))) =>
        BoolValue(string < that)

      case _ =>
        super.receive(msg)
  }

trait BoolValue extends Value

object BoolValue:
  def apply(value: Boolean): BoolValue = {
    if value then TrueValue else FalseValue
  }

case object TrueValue extends BoolValue:
  override def toString: String = "true"

  override def receive(msg: Message): Value = {
    msg match
      // "and": true & x is x, for any x (not just booleans)
      case Message("&", Seq(that)) =>
        that
      // "or": true | x is true, for any x
      case Message("|", Seq(_)) =>
        this
      case Message("not", Nil) =>
        FalseValue

      case Message("asInteger", Nil) =>
        IntValue(1)
      case Message("asString", Nil) =>
        StrValue("true")

      case Message("=", Seq(TrueValue)) =>
        TrueValue
      case Message("=", Seq(FalseValue)) =>
        FalseValue
      // Note that we consider "false" to be less than "true"
      case Message("<", Seq(TrueValue)) =>
        FalseValue
      case Message("<", Seq(FalseValue)) =>
        FalseValue

      case Message("ifTrue:", Seq(trueBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifTrue:ifFalse:", Seq(trueBlock, falseBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifFalse:", Seq(falseBlock)) =>
        NullValue
      case Message("ifFalse:ifTrue:", Seq(falseBlock, trueBlock)) =>
        trueBlock.receive(Message("value", Nil))

      case _ =>
        super.receive(msg)
  }

case object FalseValue extends BoolValue:
  override def toString: String = "false"

  override def receive(msg: Message): Value = {
    msg match
      // Removed for assignment 2
      
      case _ =>
        super.receive(msg)
  }

case class BlockValue[M[_]](body: () => M[Value]) extends Value:
  override def toString: String = "<block>"

  override def receive(msg: Message): Value = {
    msg match
      case Message("value", Nil) =>
        body(); ??? // TODO all receives return M[Value]?

      case _ =>
        super.receive(msg)
  }
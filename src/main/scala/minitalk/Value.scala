package minitalk

import C.*

case class Message(selector: String, args: Seq[Value])

/**
  * A Value represents a MicroTalk object. Since MicroTalk does not
  * (currently) provide a way to define new classes or methods, the
  * standard class library is implemented here.
  */
trait Value:
  def receive(msg: Message): C[Value] = {
    msg match
      case Message("~=", Seq(that)) =>
        for
          equal <- this.receive(Message("=", Seq(that)))
          result <- equal.receive(Message("not", Nil))
        yield result
      case Message(">=", Seq(that)) =>
        for
          less <- this.receive(Message("<", Seq(that)))
          result <- less.receive(Message("not", Nil))
        yield result
      case Message(">", Seq(that)) =>
        that.receive(Message("<", Seq(this)))
      case Message("<=", Seq(that)) =>
        that.receive(Message(">=", Seq(this)))

      case _ =>
        println("Unknown message: " + msg + " received by " + this)
        pure(NullValue)
  }

case object NullValue extends Value:
  override def toString: String = "nil"

case class IntValue(number: Int) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        pure(IntValue(number + that))
      case Message("+", Seq(RealValue(that))) =>
        pure(RealValue(number + that))

      case Message("-", Seq(IntValue(that))) =>
        pure(IntValue(number - that))
      case Message("-", Seq(RealValue(that))) =>
        pure(RealValue(number - that))

      case Message("*", Seq(IntValue(that))) =>
        pure(IntValue(number * that))
      case Message("*", Seq(RealValue(that))) =>
        pure(RealValue(number * that))

      case Message("/", Seq(IntValue(that))) =>
        pure(IntValue(number / that))
      case Message("/", Seq(RealValue(that))) =>
        pure(RealValue(number / that))

      case Message("%", Seq(IntValue(that))) =>
        pure(IntValue(number % that))
      case Message("%", Seq(RealValue(that))) =>
        pure(RealValue(number % that))

      case Message("**", Seq(IntValue(that))) =>
        pure(IntValue(math.round(math.pow(number, that)).toInt))
      case Message("**", Seq(RealValue(that))) =>
        pure(RealValue(math.pow(number, that)))

      case Message("sqrt", Nil) =>
        pure(RealValue(math.sqrt(number)))

      case Message("asInteger", Nil) =>
        pure(this)
      case Message("asFloat", Nil) =>
        pure(RealValue(number))
      case Message("asCharacter", Nil) =>
        pure(CharValue(number.toChar))
      case Message("asString", Nil) =>
        pure(StrValue(number.toString))

      case Message("=", Seq(IntValue(that))) =>
        pure(BoolValue(number == that))
      case Message("=", Seq(RealValue(that))) =>
        pure(BoolValue(number == that))

      case Message("<", Seq(IntValue(that))) =>
        pure(BoolValue(number < that))
      case Message("<", Seq(RealValue(that))) =>
        pure(BoolValue(number < that))

      case _ =>
        super.receive(msg)
  }

case class RealValue(number: Double) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        pure(RealValue(number + that))
      case Message("+", Seq(RealValue(that))) =>
        pure(RealValue(number + that))

      case Message("-", Seq(IntValue(that))) =>
        pure(RealValue(number - that))
      case Message("-", Seq(RealValue(that))) =>
        pure(RealValue(number - that))

      case Message("*", Seq(IntValue(that))) =>
        pure(RealValue(number * that))
      case Message("*", Seq(RealValue(that))) =>
        pure(RealValue(number * that))

      case Message("/", Seq(IntValue(that))) =>
        pure(RealValue(number / that))
      case Message("/", Seq(RealValue(that))) =>
        pure(RealValue(number / that))

      case Message("%", Seq(IntValue(that))) =>
        pure(RealValue(number % that))
      case Message("%", Seq(RealValue(that))) =>
        pure(RealValue(number % that))

      case Message("**", Seq(IntValue(that))) =>
        pure(RealValue(math.pow(number, that)))
      case Message("**", Seq(RealValue(that))) =>
        pure(RealValue(math.pow(number, that)))

      case Message("sqrt", Nil) =>
        pure(RealValue(math.sqrt(number)))

      case Message("asInteger", Nil) =>
        pure(IntValue(math.round(number).toInt))
      case Message("asFloat", Nil) =>
        pure(this)
      case Message("asString", Nil) =>
        pure(StrValue(number.toString))

      case Message("=", Seq(IntValue(that))) =>
        pure(BoolValue(number == that))
      case Message("=", Seq(RealValue(that))) =>
        pure(BoolValue(number == that))

      case Message("<", Seq(IntValue(that))) =>
        pure(BoolValue(number < that))
      case Message("<", Seq(RealValue(that))) =>
        pure(BoolValue(number < that))

      case _ =>
        super.receive(msg)
  }

case class CharValue(char: Char) extends Value:
  override def toString: String = char.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(StrValue(that))) =>
        pure(StrValue(char + that))

      case Message("*", Seq(IntValue(that))) =>
        pure(StrValue(char.toString * that))

      case Message("asInteger", Nil) =>
        pure(IntValue(char.toInt))
      case Message("asString", Nil) =>
        pure(StrValue(char.toString))

      case Message("=", Seq(CharValue(that))) =>
        pure(BoolValue(char == that))
      case Message("<", Seq(CharValue(that))) =>
        pure(BoolValue(char < that))

      case _ =>
        super.receive(msg)
  }

case class StrValue(string: String) extends Value:
  override def toString: String = string

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(CharValue(that))) =>
        pure(StrValue(string + that))
      case Message("+", Seq(StrValue(that))) =>
        pure(StrValue(string + that))

      case Message("*", Seq(IntValue(that))) =>
        pure(StrValue(string * that))

      case Message("charAt:", Seq(IntValue(that))) =>
        pure(CharValue(string(that)))
      case Message("size", Nil) =>
        pure(IntValue(string.length))

      case Message("asInteger", Nil) =>
        pure(IntValue(string.toDouble.toInt))
      case Message("asFloat", Nil) =>
        pure(RealValue(string.toDouble))
      case Message("asString", Nil) =>
        pure(this)

      case Message("=", Seq(StrValue(that))) =>
        pure(BoolValue(string == that))
      case Message("<", Seq(StrValue(that))) =>
        pure(BoolValue(string < that))

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

  override def receive(msg: Message): C[Value] = {
    msg match
      // "and": true & x is x, for any x (not just booleans)
      case Message("&", Seq(that)) =>
        pure(that)
      // "or": true | x is true, for any x
      case Message("|", Seq(_)) =>
        pure(this)
      case Message("not", Nil) =>
        pure(FalseValue)

      case Message("asInteger", Nil) =>
        pure(IntValue(1))
      case Message("asString", Nil) =>
        pure(StrValue("true"))

      case Message("=", Seq(TrueValue)) =>
        pure(TrueValue)
      case Message("=", Seq(FalseValue)) =>
        pure(FalseValue)
      // Note that we consider "false" to be less than "true"
      case Message("<", Seq(TrueValue)) =>
        pure(FalseValue)
      case Message("<", Seq(FalseValue)) =>
        pure(FalseValue)

      case Message("ifTrue:", Seq(trueBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifTrue:ifFalse:", Seq(trueBlock, falseBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifFalse:", Seq(falseBlock)) =>
        pure(NullValue)
      case Message("ifFalse:ifTrue:", Seq(falseBlock, trueBlock)) =>
        trueBlock.receive(Message("value", Nil))

      case _ =>
        super.receive(msg)
  }

case object FalseValue extends BoolValue:
  override def toString: String = "false"

  override def receive(msg: Message): C[Value] = {
    msg match
      // Removed for assignment 2
      
      case _ =>
        super.receive(msg)
  }

case class BlockValue(body: () => C[Value]) extends Value:
  override def toString: String = "<block>"

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("value", Nil) =>
        body()

      case _ =>
        super.receive(msg)
  }
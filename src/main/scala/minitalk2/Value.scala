package minitalk2

import C.*

import cats.*
import cats.implicits.*

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
        NullValue.pure
  }

case object NullValue extends Value:
  override def toString: String = "nil"

case class IntValue(number: Int) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        IntValue(number + that).pure
      case Message("+", Seq(RealValue(that))) =>
        RealValue(number + that).pure

      case Message("-", Seq(IntValue(that))) =>
        IntValue(number - that).pure
      case Message("-", Seq(RealValue(that))) =>
        RealValue(number - that).pure

      case Message("*", Seq(IntValue(that))) =>
        IntValue(number * that).pure
      case Message("*", Seq(RealValue(that))) =>
        RealValue(number * that).pure

      case Message("/", Seq(IntValue(that))) =>
        IntValue(number / that).pure
      case Message("/", Seq(RealValue(that))) =>
        RealValue(number / that).pure

      case Message("%", Seq(IntValue(that))) =>
        IntValue(number % that).pure
      case Message("%", Seq(RealValue(that))) =>
        RealValue(number % that).pure

      case Message("**", Seq(IntValue(that))) =>
        IntValue(math.round(math.pow(number, that)).toInt).pure
      case Message("**", Seq(RealValue(that))) =>
        RealValue(math.pow(number, that)).pure

      case Message("sqrt", Nil) =>
        RealValue(math.sqrt(number)).pure

      case Message("asInteger", Nil) =>
        this.pure
      case Message("asFloat", Nil) =>
        RealValue(number).pure
      case Message("asCharacter", Nil) =>
        CharValue(number.toChar).pure
      case Message("asString", Nil) =>
        StrValue(number.toString).pure

      case Message("=", Seq(IntValue(that))) =>
        BoolValue(number == that).pure
      case Message("=", Seq(RealValue(that))) =>
        BoolValue(number == that).pure

      case Message("<", Seq(IntValue(that))) =>
        BoolValue(number < that).pure
      case Message("<", Seq(RealValue(that))) =>
        BoolValue(number < that).pure

      case _ =>
        super.receive(msg)
  }

case class RealValue(number: Double) extends Value:
  override def toString: String = number.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(IntValue(that))) =>
        RealValue(number + that).pure
      case Message("+", Seq(RealValue(that))) =>
        RealValue(number + that).pure

      case Message("-", Seq(IntValue(that))) =>
        RealValue(number - that).pure
      case Message("-", Seq(RealValue(that))) =>
        RealValue(number - that).pure

      case Message("*", Seq(IntValue(that))) =>
        RealValue(number * that).pure
      case Message("*", Seq(RealValue(that))) =>
        RealValue(number * that).pure

      case Message("/", Seq(IntValue(that))) =>
        RealValue(number / that).pure
      case Message("/", Seq(RealValue(that))) =>
        RealValue(number / that).pure

      case Message("%", Seq(IntValue(that))) =>
        RealValue(number % that).pure
      case Message("%", Seq(RealValue(that))) =>
        RealValue(number % that).pure

      case Message("**", Seq(IntValue(that))) =>
        RealValue(math.pow(number, that)).pure
      case Message("**", Seq(RealValue(that))) =>
        RealValue(math.pow(number, that)).pure

      case Message("sqrt", Nil) =>
        RealValue(math.sqrt(number)).pure

      case Message("asInteger", Nil) =>
        IntValue(math.round(number).toInt).pure
      case Message("asFloat", Nil) =>
        this.pure
      case Message("asString", Nil) =>
        StrValue(number.toString).pure

      case Message("=", Seq(IntValue(that))) =>
        BoolValue(number == that).pure
      case Message("=", Seq(RealValue(that))) =>
        BoolValue(number == that).pure

      case Message("<", Seq(IntValue(that))) =>
        BoolValue(number < that).pure
      case Message("<", Seq(RealValue(that))) =>
        BoolValue(number < that).pure

      case _ =>
        super.receive(msg)
  }

case class CharValue(char: Char) extends Value:
  override def toString: String = char.toString

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(StrValue(that))) =>
        StrValue(char + that).pure

      case Message("*", Seq(IntValue(that))) =>
        StrValue(char.toString * that).pure

      case Message("asInteger", Nil) =>
        IntValue(char.toInt).pure
      case Message("asString", Nil) =>
        StrValue(char.toString).pure

      case Message("=", Seq(CharValue(that))) =>
        BoolValue(char == that).pure
      case Message("<", Seq(CharValue(that))) =>
        BoolValue(char < that).pure

      case _ =>
        super.receive(msg)
  }

case class StrValue(string: String) extends Value:
  override def toString: String = string

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("+", Seq(CharValue(that))) =>
        StrValue(string + that).pure
      case Message("+", Seq(StrValue(that))) =>
        StrValue(string + that).pure

      case Message("*", Seq(IntValue(that))) =>
        StrValue(string * that).pure

      case Message("charAt:", Seq(IntValue(that))) =>
        CharValue(string(that)).pure
      case Message("size", Nil) =>
        IntValue(string.length).pure

      case Message("asInteger", Nil) =>
        IntValue(string.toDouble.toInt).pure
      case Message("asFloat", Nil) =>
        RealValue(string.toDouble).pure
      case Message("asString", Nil) =>
        this.pure

      case Message("=", Seq(StrValue(that))) =>
        BoolValue(string == that).pure
      case Message("<", Seq(StrValue(that))) =>
        BoolValue(string < that).pure

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
        that.pure
      // "or": true | x is true, for any x
      case Message("|", Seq(_)) =>
        this.pure
      case Message("not", Nil) =>
        FalseValue.pure

      case Message("asInteger", Nil) =>
        IntValue(1).pure
      case Message("asString", Nil) =>
        StrValue("true").pure

      case Message("=", Seq(TrueValue)) =>
        TrueValue.pure
      case Message("=", Seq(FalseValue)) =>
        FalseValue.pure
      // Note that we consider "false" to be less than "true"
      case Message("<", Seq(TrueValue)) =>
        FalseValue.pure
      case Message("<", Seq(FalseValue)) =>
        FalseValue.pure

      case Message("ifTrue:", Seq(trueBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifTrue:ifFalse:", Seq(trueBlock, falseBlock)) =>
        trueBlock.receive(Message("value", Nil))
      case Message("ifFalse:", Seq(falseBlock)) =>
        NullValue.pure
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

case class BlockValue(body: Seq[Value] => C[Value]) extends Value:
  override def toString: String = "<block>"

  override def receive(msg: Message): C[Value] = {
    msg match
      case Message("value", args) =>
        body(args)
      case Message("value:", args) =>
        body(args)
      case Message("value:value:", args) =>
        body(args)
      case Message("value:value:value:", args) =>
        body(args)
      case Message("value:value:value:value:", args) =>
        body(args)
      // Assumes you will never have more than four arguments...

      case _ =>
        super.receive(msg)
  }
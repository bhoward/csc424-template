package dsl

class Dont(codeBlock: => Unit) {
  infix def unless(condition: => Boolean) = {
    if condition then codeBlock
  }

  infix def until(condition: => Boolean) = {
    while !condition do {}
    codeBlock
  }
}

object Dont {
  def dont(codeBlock: => Unit): Dont = Dont(codeBlock)
}

@main def dontDemo(): Unit = {
  import Dont.*

  // (1) this will never be executed
  dont {
    println("If you see this, something is wrong.")
  }

  // (2) this will only be executed if the condition is true
  dont {
    println("Yes, 2 is greater than 1")
  } unless (2 > 1)

  // (3a) a little helper function for the `until` example that follows
  var number = 0
  def nextNumber = {
    number += 1
    println(number)
    number
  }

  // (3b) no output will be printed until the condition is met
  dont {
    println("Done counting to 5.")
  } until (nextNumber == 5)
}

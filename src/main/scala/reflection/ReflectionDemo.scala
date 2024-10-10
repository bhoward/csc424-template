package reflection

import cats.instances.double

trait Thing:
    def id: Int

case class Demo(name: String, id: Int) extends Thing:
    val extendedName = s"$name (#$id)"

    private var fave: Double = math.Pi

    def greet(message: String): Unit = {
        println(s"$message from $extendedName")
        println(s"My current favorite number is $fave")
    }

@main
def tryit(): Unit = {
    val demo = Demo("Joe", 42)
    demo.greet("Hello World")

    val thing: Thing = demo

    val thingClass = thing.getClass()
    println(thingClass.getCanonicalName())
    println(thingClass.getPackageName())
    println(thingClass.getSimpleName())
    println
 
    println("Interfaces:")
    for i <- thingClass.getInterfaces() do
        println(i)
    println

    println("Superclasses:")
    var supe = thingClass.getSuperclass()
    while supe != null do
        println(supe)
        supe = supe.getSuperclass()
    println

    println("Constructors:")
    for c <- thingClass.getConstructors() do
        println(c)
    println

    val methods = thingClass.getMethods()
    println(s"Methods (${methods.length}):")
    for m <- methods do
        println(m)
    println

    val declaredMethods = thingClass.getDeclaredMethods()
    println(s"Declared Methods (${declaredMethods.length}):")
    for m <- declaredMethods do
        println(m)
    println

    println("Fields:")
    for f <- thingClass.getDeclaredFields() do
        println(f)
    println

    // These are compile-time errors: fave is public,
    // and thing is declared with type Thing, not Demo:
    // demo.fave = Math.TAU
    // thing.greet("Hello World")

    // Let's use reflection to call greet():
    val greet = thingClass.getMethod("greet", "".getClass())
    greet.invoke(thing, "Hi there")

    // Now we'll change the favorite number:
    val fave = thingClass.getDeclaredField("fave")
    fave.setAccessible(true)

    // Still not OK for the compiler:
    // demo.fave = Math.TAU

    fave.setDouble(thing, Math.TAU)
    fave.setAccessible(false)
    demo.greet("Hello again")
}
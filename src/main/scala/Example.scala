import doodle.core.*
import doodle.image.*
import doodle.image.syntax.all.*
import doodle.image.syntax.core.*
import doodle.core.font.*
import doodle.java2d.*
import doodle.reactor.*
import scala.concurrent.duration.*
import cats.effect.unsafe.implicits.global

// To use this example:
//
// 1. run `sbt`
// 2. run the `run` command within `sbt`
object Example {
  val image1 =
    Image.circle(100).fillColor(Color.red) `on`
    Image.circle(200).fillColor(Color.aquamarine) `on`
    Image.circle(300).fillColor(Color.steelBlue)
  
  def spiral(dot: Image, radius: Double, step: Angle, samples: Int): Image = {
    def aux(count: Int): Image = {
      val r = radius * count / samples
      val theta = step * count
      val color = Color.hsl(theta * 5, 1, 0.5)
      count match
        case 0 => Image.empty
        case n => dot.at(r, theta).fillColor(color) `on` aux(n - 1)
    }

    aux(samples)
  }

  val image2 = spiral(Image.star(5, 14, 5), 250, 42.degrees, 200)

  def sierpinski(count: Int): Image = {
    count match
      case 0 => Image.triangle(10, 10).strokeColor(Color.blue)
      case n =>
        val unit = sierpinski(n - 1)
        unit `above` (unit `beside` unit)
  }

  val image3 = sierpinski(6)

  def digitFlower(num: Int, radius: Double): Image = {
    def aux(count: Int, petal: Image, step: Angle): Image = {
      count match
        case 0 => Image.empty
        case n => {
          val point = Point(radius, step * n)
          petal.at(point) `on`
            Image.path(OpenPath.empty.lineTo(point)) `on`
            aux(count - 1, petal, step)
        }
    }

    num match
      case 0 => Image.empty
      case n => {
        val digit = n % 10
        val petal = digitFlower(n / 10, radius / 3)
        aux(digit, petal, 1.turns / digit)
      }
  }

  val image4 = digitFlower(34567, 200)

  def fibRect(num: Int): Image = {
    def aux(num: Int): (Image, Double, Double) = {
      val c = Color.hsl(num.turns / 9, 1, 0.5)
      num match
        case 0 => (Image.square(1).fillColor(c), 1, 1)
        case n =>
          val (r, w, h) = aux(n - 1)
          val w2 = w / 2
          val combined = r.rotate(90.degrees).size(h, w) `beside` (
            Image.path(OpenPath.empty
              .moveTo(-w2, w2)
              .curveTo(0, w2, w2, 0, w2, -w2)) `on`
            Image.square(w).fillColor(c))
          (combined, h + w, w)
    }

    val (result, _, _) = aux(num)
    result
  }

  val image5 = fibRect(14)

  val animation =
    Reactor
      .init(0.degrees)
      .withOnTick(a => a + 1.degrees)
      .withStop(a => a > 360.degrees)
      .withTickRate(20.millis)
      .withRender { a =>
        val location = Point(200, a)
        val planet = Image.circle(40).noStroke.fillColor(Color.seaGreen)
        val moon = Image.circle(10).noStroke.fillColor(Color.slateGray)
          .at(Point(60, a * 5))

        moon.on(planet).at(location)
      }

  val frame = Frame.default.withSize(600, 600).withCenterAtOrigin

  @main def go(): Unit = {
    image1.draw()
    // image2.draw()
    // image3.draw()
    // image4.draw()
    // image5.draw()

    // Comment out the above and uncomment the below to display the animation
    // animation.run(frame)
  }
}

scalaVersion := "3.4.2"

console / initialCommands := """
      |import doodle.core.*
      |import doodle.image.*
      |import doodle.image.syntax.all.*
      |import doodle.image.syntax.core.*
      |import doodle.core.font.*
      |import doodle.java2d.*
      |import cats.effect.unsafe.implicits.global
    """.trim.stripMargin

libraryDependencies ++= Seq(
  "org.creativescala" %% "doodle" % "0.23.0"
)

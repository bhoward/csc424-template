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
  "org.creativescala" %% "doodle" % "0.23.0",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  "org.typelevel" %% "cats-core" % "2.12.0"
)

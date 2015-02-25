
import sbt._

trait Dependencies { this:Build =>

  val mainDeps = Seq(
    "com.chuusai"   %% "shapeless"       % "2.0.0",
    "org.scalactic" %% "scalactic"       % "2.2.4",
    "io.github.jto" %% "validation-core" % "1.0-1c770f4",
    "io.github.jto" %% "validation-json" % "1.0-1c770f4"
  )

  val testDeps = Seq(
    "org.scalatest" %% "scalatest"        % "2.2.1" % "test"
  )

  val allDeps = mainDeps ++ testDeps

}

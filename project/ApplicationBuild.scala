
import sbt._


object ApplicationBuild extends Build
with BuildSettings
with Dependencies
 {

  val main = Project(
    id = "objectvalidation",
    base = file("."),
    settings = projectSettings(dependencies = allDeps)
  )

}

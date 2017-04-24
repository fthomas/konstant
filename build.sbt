name := "konstant"

resolvers += Resolver.bintrayRepo("scalameta", "maven")

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "1.8.0-526-cb5dd112",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

initialCommands := """
  import scala.meta._
"""

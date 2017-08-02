name := "cwf"

version := "1.0"
//
scalaVersion := "2.12.2"

resolvers += Resolver.bintrayRepo("rmgk", "maven")
resolvers += Resolver.bintrayRepo("pweisenburger", "maven")

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.12" % "7.2.8",
  "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "de.tuda.stg" %% "rescala" % "0.19.0"
)
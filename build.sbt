name := "adventofcode2017"

version := "0.1"

scalaVersion := "2.13.1"

val fs2Version = "2.3.0"

libraryDependencies += "co.fs2" %% "fs2-core" % fs2Version
libraryDependencies += "co.fs2" %% "fs2-io" % fs2Version

val CatsVersion = "2.0.0"
val CatsEffectVersion = "2.1.3"

libraryDependencies += "org.typelevel" %% "cats-core" % CatsVersion

libraryDependencies += "org.typelevel" %% "cats-effect" % CatsEffectVersion

libraryDependencies += "net.debasishg" %% "redisclient" % "3.20"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % Runtime

libraryDependencies += "com.lihaoyi" % "ammonite" % "2.1.1" % "test" cross CrossVersion.full

libraryDependencies += "io.monix" %% "monix" % "3.2.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

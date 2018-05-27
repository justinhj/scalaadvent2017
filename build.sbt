name := "adventofcode2017"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.4"
libraryDependencies += "co.fs2" %% "fs2-io" % "0.10.4"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.1.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.10"

libraryDependencies += "net.debasishg" %% "redisclient" % "3.5"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3" % Runtime

libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.5" % "test" cross CrossVersion.full

libraryDependencies += "io.monix" %% "monix" % "3.0.0-RC1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.23"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

libraryDependencies += "de.sciss" %% "fingertree" % "1.5.2"

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

name := "adventofcode2017"

version := "0.1"

scalaVersion := "2.12.2"

libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.2"

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.10"

libraryDependencies += "com.lihaoyi" % "ammonite" % "1.0.5" % "test" cross CrossVersion.full

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main().run() }""")
  Seq(file)
}.taskValue

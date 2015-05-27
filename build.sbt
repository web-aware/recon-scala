name         := "recon-scala"
version      := "0.1.0-SNAPSHOT"
organization := "com.webaware"

description  := "Record Notation (RECON) Scala Implementation"
homepage     := Some(url("https://github.com/web-aware/recon-scala"))
licenses     := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.11.6"
scalacOptions ++= Seq(
  "-language:_",
  "-Yno-predef",
  "-optimise",
  "-deprecation",
  "-unchecked",
  "-Xfuture",
  "-Ywarn-adapted-args",
  "-Ywarn-inaccessible",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-unused",
  "-Ywarn-unused-import")

scalacOptions in (Compile, console) := Seq(
  "-language:_",
  "-Yno-predef",
  "-optimise",
  "-deprecation")

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies ++= Seq(
  "it.reify" %% "basis-core" % "0.2.0-SNAPSHOT",
  "it.reify" %% "basis-data" % "0.2.0-SNAPSHOT",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test")

scalacOptions in (Compile, doc) ++= {
  val tagOrBranch = if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value
  val docSourceUrl = "https://github.com/web-aware/recon-scala/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
  Seq("-groups",
      "-implicits",
      "-diagrams",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-source-url", docSourceUrl,
      "-Ymacro-expand:none")
}

pomIncludeRepository := (_ => false)
publishMavenStyle := true
pomExtra := {
  <scm>
    <url>git@github.com:web-aware/recon-scala.git</url>
    <connection>scm:git:git@github.com:web-aware/recon-scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>c9r</id>
      <name>Chris Sachs</name>
      <email>chris@webaware.com</email>
    </developer>
  </developers>
}

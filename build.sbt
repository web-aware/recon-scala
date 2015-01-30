name         := "recon-scala"
version      := "0.0.0-SNAPSHOT"
organization := "net.coeffect"

description  := "Record Notation (RECON) Scala Implementation"
homepage     := Some(url("http://recon.coeffect.net/scala/"))
licenses     := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

scalaVersion := "2.11.5"
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

apiURL := Some(url("http://recon.coeffect.net/scala/api/"))
scalacOptions in (Compile, doc) ++= {
  val tagOrBranch = if (version.value.endsWith("-SNAPSHOT")) "master" else "v" + version.value
  val docSourceUrl = "https://github.com/coeffect/recon-scala/tree/" + tagOrBranch + "€{FILE_PATH}.scala"
  Seq("-groups",
      "-implicits",
      "-diagrams",
      "-sourcepath", baseDirectory.value.getAbsolutePath,
      "-doc-source-url", docSourceUrl,
      "-Ymacro-expand:none")
}

pomIncludeRepository := (_ => false)
publishMavenStyle := true
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (version.value.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
pomExtra := {
  <scm>
    <url>git@github.com:coeffect/recon-scala.git</url>
    <connection>scm:git:git@github.com:coeffect/recon-scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>c9r</id>
      <name>Chris Sachs</name>
      <email>chris@coeffect.net</email>
    </developer>
  </developers>
}

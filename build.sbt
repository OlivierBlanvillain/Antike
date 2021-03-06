val scalaVersion = "2.11.5"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Yno-adapted-args",
  "-Xfuture",
  "-Xlint")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %% "monocle-core" % "1.0.1",
  "com.github.julien-truffaut" %% "monocle-macro" % "1.0.1",
  "org.scalacheck" %% "scalacheck" % "1.12.2" % "test")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)

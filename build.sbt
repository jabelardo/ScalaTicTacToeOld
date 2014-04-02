name := "Tic Tac Toe"

version := "1.0"

scalaVersion := "2.10.3"

sbtVersion := "0.13.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.10" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

// Read here for optional dependencies:
// http://etorreborre.github.io/specs2/guide/org.specs2.guide.Runners.html#Dependencies

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

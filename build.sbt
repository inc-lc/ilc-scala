scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ )

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )

// SCALA METER BEGINS
// http://axel22.github.io/scalameter//2013/06/14/release_0_4_M2.html

// resolver for ScalaMeter
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

// ScalaMeter
libraryDependencies += "com.github.axel22" %% "scalameter" % "0.3"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

logBuffered := false

lazy val ilc = project in file(".")

lazy val clients = project in file("clients") dependsOn (ilc % "test->test;compile->test")

lazy val bigClients = project in file("bigClients") dependsOn (clients % "test->test;compile->test")

scalaVersion in ThisBuild := "2.10.4"

scalacOptions in ThisBuild := Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

scalacOptions in ThisBuild ++= Seq("-optimize")

//Link to the Scala standard library. See http://stackoverflow.com/a/18747266/53974.
//For Scaladoc, requires SBT 0.13.
autoAPIMappings in ThisBuild := true
// However, this might not work for the standard library due to stale caches.
// To fix, remove ~/.ivy2/cache/org.scala-lang/scala-library/ivy-2.10.2.xml

//Add a manual mapping for the standard library,
apiMappings in ThisBuild += (scalaInstance.value.libraryJar -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
//More explicit variant:
//scalacOptions in doc += s"-doc-external-doc:${scalaInstance.value.libraryJar}#http://www.scala-lang.org/api/${scalaVersion.value}/"

scalacOptions in doc in ThisBuild ++= Seq("-implicits",
  "-diagrams",
  "-doc-title", "ILC",
  "-doc-source-url", "file:€{FILE_PATH}.scala")

//0.13 syntax:
scalacOptions in (Compile, doc) in ThisBuild := (scalacOptions in doc).value

scalacOptions in (Test, doc) in ThisBuild := (scalacOptions in doc).value

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

// SCALA METER BEGINS
// http://axel22.github.io/scalameter//2013/06/14/release_0_4_M2.html

// resolver for ScalaMeter
resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

// ScalaMeter
libraryDependencies += "com.github.axel22" %% "scalameter" % "0.5-M2" % "test"

logBuffered in ThisBuild := false

val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

testFrameworks in ThisBuild += scalaMeterFramework

// -preJDK7 allows ScalaMeter to run on JDK 6 ─ see
// http://axel22.github.io/scalameter/2013/02/14/release_0_3.html
testOptions in ThisBuild += Tests.Argument(scalaMeterFramework, "-preJDK7 -CresultDir testOutput -silent")

parallelExecution in Test in ThisBuild := false

//SCALA METER ENDS

////Also generate HTML during compilation. We might want to move this later to a
////separate task.
//resolvers += Resolver.url("Typesafe Releases", url("http://repo.typesafe.com/typesafe/ivy-releases"))(Resolver.ivyStylePatterns)
//
//addCompilerPlugin("org.scala-sbt.sxr" %% "sxr" % "0.3.0")
//
//scalacOptions in (Compile, compile) in ThisBuild <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }
//
//scalacOptions in (Test, compile) in ThisBuild <+= scalaSource in Test map { "-P:sxr:base-directory:" + _.getAbsolutePath }
//
//scalacOptions in (Test, compile) in ThisBuild <+= baseDirectory map { base =>
//  val linkFile = base / "sxr.links"
//  "-P:sxr:link-file:" + linkFile.getAbsolutePath
//}

// Also enable access to source files for navigation.
EclipseKeys.withSource in ThisBuild := true

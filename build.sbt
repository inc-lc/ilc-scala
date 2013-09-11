scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-feature", "-Xlint")

//Link to the Scala standard library. See http://stackoverflow.com/a/18747266/53974.
//For Scaladoc, requires SBT 0.13. However, automatic mapping does not work for the standard library.
//autoAPIMappings := true

//Add a manual mapping for the standard library,
//0.13 syntax:
//apiMappings += (scalaInstance.value.libraryJar -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
//More explicit variant:
//scalacOptions in doc += s"-doc-external-doc:${scalaInstance.value.libraryJar}#http://www.scala-lang.org/api/${scalaVersion.value}/"
//0.12 desugaring of the above:
scalacOptions in doc <+= (scalaVersion, scalaInstance) map { (scalaVer, scalaIn) =>
    "-doc-external-doc:" + scalaIn.libraryJar + "#http://www.scala-lang.org/api/" + scalaVer + "/"}

scalacOptions in doc ++= Seq("-implicits",
  "-diagrams",
  "-doc-title", "ILC",
  "-doc-source-url", "file:€{FILE_PATH}.scala")

//0.13 syntax:
//scalacOptions in (Compile, doc) := (scalacOptions in doc).value
//0.12:
scalacOptions in (Compile, doc) <<= scalacOptions in doc

scalacOptions in (Test, doc) <<= scalacOptions in doc

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

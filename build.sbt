scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-feature", "-Xlint")

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

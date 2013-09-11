scalaVersion := "2.10.2"

scalacOptions := Seq("-deprecation", "-feature", "-Xlint")

scalacOptions in (Compile, doc) ++= Seq("-implicits",
  "-diagrams",
  "-doc-title", "ILC",
  "-doc-source-url", "file:€{FILE_PATH}.scala")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"

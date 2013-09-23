package ilc
package examples

import java.io.File
import ilc.feature.base.Syntax

trait Generator
extends Archive
   with Dummy
{
  def main(args: Array[String]) {
    // base is the directory to generate in
    val base = new File(args.head)
    base.mkdirs()
    exportDummy(base)
    exportExamples(base)
  }

  // stdout is exported as paths
  def export(path: String): Unit =
    Console.out.println(path)

  // stderr is echoed as [info] in sbt console
  def info(message: String): Unit =
    Console.err.println(message)

  def exportExamples(base: File) {
    for {
      (name, example) <- archive
    } {
      exportSource(base, name, example)
    }
  }

  // dummy code
  def exportDummy(base: File) {
    val path = new File(base, "DummyGenerated.scala").getCanonicalPath
    import java.io.FileWriter
    val file = new FileWriter(path)
    file.write(scalaMeterDummyCode)
    file.close
    export(path)
  }

  def rebuildNeeded(base: File, name: String, example: Example, outFile: File) = {
    val exampleFileName = example.getClass.
      getName stripSuffix "$" replaceAll ("\\.", java.io.File.separator)

    val fsep = File.separator
    //XXX hardcodes the Scala version.
    val exampleOutput = new File(s"target${fsep}scala-2.10${fsep}classes${fsep}${exampleFileName}.class")

    //XXX very simplified dependency checking, does not account for all dependencies.
    exampleOutput.lastModified > outFile.lastModified

    //Alternative, for robustness. Comment this out for faster rebuilds.
    true
  }

  def exportSource(base: File, name: String, example: Example) {
    val outFile = new File(base, Archive.toGenName(name) ++ ".scala")

    if (rebuildNeeded(base, name, example, outFile)) {
      Console.err.println(s"Generating ${name}")
      val source = example.toSource(name)
      import java.io.FileWriter
      val writer = new FileWriter(outFile)
      writer.write(source.code)
      writer.close
    }

    //Ensure this file is tracked by SBT anyway.
    export(outFile.getCanonicalPath)
  }
}

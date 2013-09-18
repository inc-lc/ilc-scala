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
    archive foreach exportExample(base)
  }

  def exportExample(base: File)(namedExample: (String, Example)) {
    val (name, example) = namedExample
    exportSource(base, example.toSource(name))
  }

  // dummy code
  def exportDummy(base: File) {
    val path = new File(base, "DummyBinary.scala").getCanonicalPath
    import java.io.FileWriter
    val file = new FileWriter(path)
    file.write(scalaMeterDummyCode)
    file.close
    export(path)
  }

  def exportSource(base: File, source: Source) {
    import java.io.FileWriter
    val file = new File(base, source.objectName ++ ".scala")
    val writer = new FileWriter(file)
    writer.write(source.code)
    writer.close
    export(file.getCanonicalPath)
  }
}

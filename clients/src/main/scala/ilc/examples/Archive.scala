package ilc
package examples

import java.io.{ File, FileWriter }

// the storage aspect of an examples generator
trait Archive {
  import scala.collection.mutable
  val archive: mutable.Map[String, Example] = mutable.LinkedHashMap.empty

  def get(exampleWithName: String): Example =
    archive(exampleWithName)

  // if name is "CoolExample", then the generated class will
  // be "CoolExampleGenerated"
  def addExample(example: Example) {
    val name = example.name
    archive.get(name) match {
      case Some(example) =>
        sys error s"example name clash: $name"

      case None =>
        archive.update(name, example)
    }
  }
}

object Archive {
  def toGenName(name: String) = name ++ "Generated"
}

/** Base class for implementations of examples.
  * When actually writing examples,
  * this must be mixed in with versions of ToScala and Derivation
  * corresponding to the used language features, and with the appropriate
  * syntax.
  */
abstract class Example
extends feature.functions.Pretty
   with feature.let.BetaReduction
   with feature.let.ProgramSize
{
  this: feature.base.ToScala
   with feature.base.Derivation =>

  def name =
    this.getClass().getSimpleName().stripSuffix("Example")

  def program: Term
  lazy val derivative: Term = derive(program)
  lazy val normalizedProgram = normalize(program)
  lazy val normalizedDerivative = normalize(derivative)

  def toSource(base: File) = {
    Seq(Source(this, new File(base, Archive.toGenName(name) + ".scala"), () => {
      assert(indentDiff == 2)
      setIndentDepth(2)

      val programCode = toScala(program)
      val derivativeCode = toScala(derivative)
      val normalizedDerivCode = toScala(normalizedDerivative)
      val inputType =>: outputType = program.getType
      val updateInputCode = toScala(updateTerm(inputType))
      val updateOutputCode = toScala(updateTerm(outputType))
      val inputTypeCode = toScala(inputType)
      val outputTypeCode = toScala(outputType)
      val deltaInputTypeCode = toScala(deltaType(inputType))
      val deltaOutputTypeCode = toScala(deltaType(outputType))

      //The output template in toSource relies on this value.
      setIndentDepth(4)

      s"""|package ilc.examples
          |
          |$imports
          |
          |object ${Archive.toGenName(name)} extends ExampleGenerated {
          |  val programSize = ${termSize(program)}
          |  val derivativeSize = ${termSize(derivative)}
          |  val normalizedProgramSize = ${termSize(normalizedProgram)}
          |  val normalizedDerivativeSize = ${termSize(normalizedDerivative)}
          |
          |  override val program = $programCode
          |  override val derivative = $derivativeCode
          |  override val normDerivative = $normalizedDerivCode
          |  override val updateInput = $updateInputCode
          |  override val updateOutput = $updateOutputCode
          |
          |  type InputType = $inputTypeCode
          |  type OutputType = $outputTypeCode
          |  type DeltaInputType = $deltaInputTypeCode
          |  type DeltaOutputType = $deltaOutputTypeCode
          |}
          |""".stripMargin
    }), Source(this, new File(base, Archive.toGenName(name) + ".scala"), () => {
      val programForHuman: String = pretty(program)
      val derivativeForHuman: String = pretty(derivative)
      val normalizedProgrForHuman: String = pretty(normalizedProgram)
      val normalizedDerivForHuman: String = pretty(normalizedDerivative)
      s"""|  /*
          |  programForHuman:
          |$programForHuman
          |
          |  derivativeForHuman:
          |$derivativeForHuman
          |
          |  normalizedProgrForHuman:
          |$normalizedProgrForHuman
          |
          |  normalizedDerivForHuman:
          |$normalizedDerivForHuman
          |  */
          |""".stripMargin
    }))
  }
}

case class Source(example: Example, outFile: File, codeGen: () => String) {
  import example.name
  //Careful with inlining this, it might avoid lots of dup. work!
  lazy val code = codeGen()

  def save(): Unit = {
    val writer = new FileWriter(outFile)
    writer.write(code)
    writer.close
  }

  def saveIfNeeded(): File = {
    if (rebuildNeeded()) {
      Console.err.println(s"Generating ${name}")
      save()
    } else {
      Console.err.println(s"Skipping ${name}, it *seems* to be up-to-date.")
    }

    outFile
  }

  def rebuildNeeded(): Boolean = {
    val exampleFileName = example.getClass.
      getName stripSuffix "$" replaceAll ("\\.", java.io.File.separator)

    val fsep = File.separator
    //XXX hardcodes the Scala version.
    val exampleOutput = new File(s"target${fsep}scala-2.10${fsep}classes${fsep}${exampleFileName}.class")

    //In debug mode, use a very simplified dependency checking. But this does not account for all dependencies.
    QuickAndDirty choose (exampleOutput.lastModified > outFile.lastModified, true)
  }
}

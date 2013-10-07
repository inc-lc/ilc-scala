package ilc
package examples

// the storage aspect of an examples generator
trait Archive {
  import scala.collection.mutable
  val archive: mutable.Map[String, Example] = mutable.LinkedHashMap.empty

  def get(exampleWithName: String): Example =
    archive(exampleWithName)

  // if name is "CoolExample", then the generated class will
  // be "CoolExampleGenerated"
  def addExample(name: String, example: Example) {
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
abstract class Example extends feature.functions.Pretty with feature.functions.BetaReduction
{
  this: feature.base.ToScala
   with feature.base.Derivation =>

  def program: Term
  def derivative: Term = derive(program)

  def toSource(name: String): Source = {
    val objectName = Archive.toGenName(name)
    val programCode = toScala(program)
    val derivativeCode = toScala(derivative)
    val inputType =>: outputType = program.getType
    val updateInputCode = toScala(updateTerm(inputType))
    val updateOutputCode = toScala(updateTerm(outputType))
    val inputTypeCode = toScala(inputType)
    val outputTypeCode = toScala(outputType)
    val deltaInputTypeCode = toScala(deltaType(inputType))
    val deltaOutputTypeCode = toScala(deltaType(outputType))

    val programForHuman: String = pretty(program)
    val derivativeForHuman: String = pretty(derivative)
    val normalizedProgrForHuman: String = pretty(betaNorm(program))
    val normalizedDerivForHuman: String = pretty(betaNorm(derivative))

    Source(objectName,
      s"""|package ilc.examples
          |
          |$imports
          |
          |object $objectName extends ExampleGenerated {
          |  override val programForHuman = "$programForHuman"
          |  override val derivativeForHuman = "$derivativeForHuman"
          |  override val normalizedProgrForHuman = "$normalizedProgrForHuman"
          |  override val normalizedDerivForHuman = "$normalizedDerivForHuman"
          |
          |  override val program = $programCode
          |  override val derivative = $derivativeCode
          |  override val updateInput = $updateInputCode
          |  override val updateOutput = $updateOutputCode
          |
          |  type InputType = $inputTypeCode
          |  type OutputType = $outputTypeCode
          |  type DeltaInputType = $deltaInputTypeCode
          |  type DeltaOutputType = $deltaOutputTypeCode
          |}
          |""".stripMargin)
  }
}

case class Source(objectName: String, code: String)

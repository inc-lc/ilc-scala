package ilc
package examples

// the storage aspect of an examples generator
trait Archive {
  import scala.collection.mutable
  val archive: mutable.Map[String, Example] = mutable.Map.empty

  def get(exampleWithName: String): Example =
    archive(exampleWithName)

  abstract class Example
  extends feature.base.ToScala
     with feature.base.Derivation
  {
    def program: Term
    def derivative: Term = derive(program)

    def toSource(name: String): Source = {
      val objectName = name ++ "Binary"
      val programCode = toScala(program)
      val derivativeCode = toScala(derivative)
      val inputType =>: outputType = program.getType
      val updateInputCode = toScala(updateTerm(inputType))
      val updateOutputCode = toScala(updateTerm(outputType))
      val inputTypeCode = toScala(inputType)
      val outputTypeCode = toScala(outputType)
      val deltaInputTypeCode = toScala(deltaType(inputType))
      val deltaOutputTypeCode = toScala(deltaType(outputType))
      Source(objectName, s"""
        package ilc.examples
        object $objectName extends ExampleBinary {
          override val program = $programCode
          override val derivative = $derivativeCode
          override val updateInput = $updateInputCode
          override val updateOutput = $updateOutputCode

          type InputType = $inputTypeCode
          type OutputType = $outputTypeCode
          type DeltaInputType = $deltaInputTypeCode
          type DeltaOutputType = $deltaOutputTypeCode
        }
      """)
    }
  }

  case class Source(objectName: String, code: String)

  // if name is "CoolExample", then the generated class will
  // be "CoolExampleBinary"
  def addExample(name: String, example: Example) {
    archive.get(name) match {
      case Some(example) =>
        sys error s"example name clash: $name"

      case None =>
        archive.update(name, example)
    }
  }
}

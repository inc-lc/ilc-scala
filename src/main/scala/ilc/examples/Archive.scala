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
      Source(objectName, s"""
        package ilc.examples
        object $objectName {
          val program      = $programCode
          val derivative   = $derivativeCode
          val updateInput  = $updateInputCode
          val updateOutput = $updateOutputCode
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

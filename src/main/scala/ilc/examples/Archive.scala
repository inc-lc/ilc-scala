package ilc
package examples

// the storage aspect of an examples generator
trait Archive {
  import scala.collection.mutable
  val archive: mutable.Map[String, Example] = mutable.Map.empty

  def get(exampleWithName: String): Example =
    archive(exampleWithName)

  abstract class Example extends feature.base.ToScala {
    def program: Term
    def derivative: Term

    def toSource(name: String): Source = {
      val objectName = name ++ "Binary"
      val programCode = toScala(program)
      val derivativeCode = toScala(derivative)
      Source(objectName, s"""
        package ilc.examples
        object $objectName {
          val program = $programCode
          val derivative = $derivativeCode
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

package ilc.examples

/**
  * Interface for generated example programs.
  */
trait ExampleGenerated extends Serializable {
  val programForHuman: String
  val derivativeForHuman: String

  type InputType
  type DeltaInputType
  type OutputType
  type DeltaOutputType

  val program: (=>InputType) => OutputType
  val derivative: (=>InputType) => (=>DeltaInputType) => DeltaOutputType
  val updateInput: (=>DeltaInputType) => (=>InputType) => InputType
  val updateOutput: (=>DeltaOutputType) => (=>OutputType) => OutputType
}

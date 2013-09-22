package ilc.examples

trait ExampleGenerated {
  type InputType
  type DeltaInputType
  type OutputType
  type DeltaOutputType

  val program: InputType => OutputType
  val derivative: InputType => DeltaInputType => DeltaOutputType
  val updateInput: DeltaInputType => InputType => InputType
  val updateOutput: DeltaOutputType => OutputType => OutputType
}

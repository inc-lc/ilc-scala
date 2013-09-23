import sbt._
import Keys._
import sbt.Defaults._

object BuildUnit extends Build {
  //Remove remaining files in target directory.
  def descendants(p: PathFinder) = p * AllPassFilter ***

  private val generatorMainClass = "ilc.Examples"

  private val generationSettings = Seq(
      // code to generate examples at stage `test`
      // usage described in ./src/main/scala/Examples.scala
      sourceGenerators in Test <+=
        (sourceManaged in Test,
          fullClasspath in Compile,
          thisProject in Compile,
          taskTemporaryDirectory in Compile,
          scalaInstance in Compile,
          baseDirectory in Compile,
          javaOptions in Compile,
          outputStrategy in Compile,
          javaHome in Compile,
          connectInput in Compile
            in Compile) map {
          (genSrcDir, lib,
            tp, tmp, si, base, options, strategy, javaHomeDir, connectIn
          ) =>
          val genFiles = generateExamples(genSrcDir, new ExamplesRunner(
            tp.id,
            lib.files,
            generatorMainClass,
            ForkOptions(
              scalaJars = si.jars,
              javaHome = javaHomeDir,
              connectInput = connectIn,
              outputStrategy = strategy,
              runJVMOptions = options,
              workingDirectory = Some(base))))

          //Note: this removes stale generated files, assuming no other
          //generator targets the same directory.
          for (staleFile <- (descendants(PathFinder(genSrcDir)) --- PathFinder(genFiles)).get) {
            console.warn("Removing stale file " + staleFile)
            console.warn("Look at project/Build.scala if this is not intended")
            staleFile.delete()
          }

          genFiles
        }
    )

  val scalaMeterFramework = new TestFramework("org.scalameter.ScalaMeterFramework")

  //XXX The type annotation is needed with SBT 0.12 to workaround a compiler
  //crash, probably due to the Scala compiler. This cannot be reproduced with
  //SBT 0.13, probably because Scala 2.10 does not suffer from this compiler
  //crash; hence, remove it when switching to 0.13.
  private val scalaMeterSettings: Seq[Setting[_]] = Seq(
      testFrameworks += scalaMeterFramework
    )

  private val extraSettings = generationSettings ++ scalaMeterSettings

  // dummy project with default settings + extraSettings.
  // This has the same effect as putting extraSettings in build.sbt.
  lazy val ilcProject = Project (
    "ilc",
    file("."),
    settings = defaultSettings ++ extraSettings
  )

  // Generate .class files from ilc.Examples during test:compile
  //
  // Architecture:
  //
  // 1. compile ilc.Examples
  // 2. execute ilc.Examples.main, giving the base dir as argument
  // 3. read stdout of ilc.Examples.main fork, convert lines to a list of dirs
  // 4. return those dirs as files
  //
  def generateExamples(base: File, generator: ExamplesRunner): Seq[File] =
  {
    console.info("Generating examples into:")
    console.info(base.toString)
    generator.start(base.getCanonicalPath).map(path => {
      val file = new File(path)
      console.info("- " ++ file.getName)
      file
    })
  }

  val console = ConsoleLogger()

  // Adapted from:
  // http://stackoverflow.com/a/10286201
  class ExamplesRunner(
    subproject: String,
    classpath: Seq[File],
    generatorMainClass: String,
    config: ForkScalaRun)
  extends sbt.ScalaRun
  {
    def start(base: String): Seq[String] = {
      val acc = new Accumulogger
      run(generatorMainClass, classpath, Seq(base), acc) match {
        case Some(error) =>
          sys.error("found some error: " ++ error)

        case None =>
          ()
      }
      acc.lines
    }

    override def run(mainClass: String, classpath: Seq[File],
      options: Seq[String], log: Logger): Option[String] =
    {
      val javaOptions = classpathOption(classpath) :::
        mainClass :: options.toList
      val strategy = config.outputStrategy getOrElse LoggedOutput(log)
      val process =  Fork.java.fork(config.javaHome,
        config.runJVMOptions ++ javaOptions,
        config.workingDirectory,
        Map.empty,
        config.connectInput,
        strategy)
      def cancel() = {
        log.warn("Run canceled.")
        process.destroy()
        1
      }
      val exitCode =
        try process.exitValue()
        catch { case e: InterruptedException => cancel() }
      processExitCode(exitCode, "runner")
    }
    private def classpathOption(classpath: Seq[File]) =
      "-classpath" :: Path.makeString(classpath) :: Nil
    private def processExitCode(exitCode: Int, label: String) = {
      if(exitCode == 0) None
      else Some("Nonzero exit code returned from " +
        label + ": " + exitCode)
    }
  }

  // accumulator+logger
  class Accumulogger extends Logger {
    protected[this] var accumulator: List[String] = Nil

    def log(level: Level.Value, message: => String): Unit = level match {
      // we echo warnings
      case Level.Warn =>
        console.warn(message)

      // we echo things printed to stderr as info
      case Level.Error =>
        console.info(message)

      case _ =>
        accumulator = message :: accumulator
    }

    def lines: List[String] = accumulator.reverse

    // fill interface with dummy methods, hope they don't get called
    // in order that Accumulogger be concrete
    def success(goodJob: => String): Unit = sys error goodJob
    def trace(t: => Throwable): Unit = sys error t.toString
  }
}

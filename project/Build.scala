import sbt._
import Keys._
import sbt.Defaults._
import scala.language.postfixOps

object BuildUnit extends Build {
  //Remove remaining files in target directory.
  def descendants(p: PathFinder) = p * AllPassFilter ***

  private val generatorMainClass = "ilc.Examples"

  //Exposed to build.sbt
  val generationSettings = Seq(
      // code to generate examples at stage `test`
      // usage described in ./src/main/scala/Examples.scala
      sourceGenerators in Test <+=
        (sourceManaged in Test,
          classDirectory in Compile,
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
          (genSrcDir, classDir, lib,
            tp, tmp, si, base, options, strategy, javaHomeDir, connectIn
          ) =>
          console.info("Generating examples into:")
          console.info(genSrcDir.toString)
          val genFiles = generateExamples(new ExamplesRunner(
            tp.id,
            lib.files,
            generatorMainClass,
            Seq(genSrcDir, classDir) map (_.getCanonicalPath),
            ForkOptions(
              bootJars = si.jars,
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

  // Generate .class files from ilc.Examples during test:compile
  //
  // Architecture:
  //
  // 1. compile ilc.Examples
  // 2. execute ilc.Examples.main, giving the base dir as argument
  // 3. read stdout of ilc.Examples.main fork, convert lines to a list of dirs
  // 4. return those dirs as files
  //
  def generateExamples(generator: ExamplesRunner): Seq[File] =
  {
    generator.start().map(path => {
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
    args: Seq[String],
    config: ForkOptions)
  extends sbt.ScalaRun
  {
    def start(): Seq[String] = {
      val acc = new Accumulogger
      run(generatorMainClass, classpath, args, acc) match {
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
      val javaOptions = classpathOption(classpath)

      val strategy = config.outputStrategy getOrElse LoggedOutput(log)
      val updConfig = config copy (runJVMOptions = config.runJVMOptions ++ javaOptions, envVars = Map.empty, outputStrategy = Some(strategy))
      val process =  Fork.java.fork(updConfig, mainClass :: options.toList)
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

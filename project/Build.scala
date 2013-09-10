import sbt._
import Keys._
import sbt.Defaults._

object BuildUnit extends Build {
  // dummy project with default settings
  // as if this file does not exist
  lazy val ilcProject = Project (
    "ilc",
    file("."),
    settings = defaultSettings
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
    config: ForkScalaRun)
  extends sbt.ScalaRun
  {
    val myMainClass: String = "ilc.Examples"

    // delete me, wenn es geht.
    val options: Seq[String] = Nil

    def start(base: String): Seq[String] = {
      val acc = new Accumulogger
      run(acc, base) match {
        case Some(error) =>
          sys.error("found some error: " ++ error)

        case None =>
          ()
      }
      acc.lines
    }

    def run(log: Logger, args: String*): Option[String] =
      run(myMainClass, classpath, args, log)

    def run(mainClass: String, classpath: Seq[File],
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

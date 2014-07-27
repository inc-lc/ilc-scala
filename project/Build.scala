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
      sourceGenerators in Test += Def.task {
          val genSrcDir = sourceManaged in Test value
          val tmp = (taskTemporaryDirectory in Compile value)
          consoleLogger.info("Generating examples into:")
          consoleLogger.info(genSrcDir.toString)
          val genFiles = generateExamples(new ExamplesRunner(
            (thisProject in Compile value) id,
            (fullClasspath in Compile value) files,
            generatorMainClass,
            Seq(genSrcDir, classDirectory in Compile value) map (_ getCanonicalPath),
            ForkOptions(
              bootJars = (scalaInstance in Compile value) jars,
              javaHome = javaHome in Compile value,
              connectInput = connectInput in Compile value,
              outputStrategy = outputStrategy in Compile value,
              runJVMOptions = javaOptions in Compile value,
              workingDirectory = Some(baseDirectory in Compile value))))

          //Note: this removes stale generated files, assuming no other
          //generator targets the same directory.
          for (staleFile <- (descendants(PathFinder(genSrcDir)) --- PathFinder(genFiles)).get) {
            consoleLogger.warn("Removing stale file " + staleFile)
            consoleLogger.warn("Look at project/Build.scala if this is not intended")
            staleFile.delete()
          }

          genFiles
      }.taskValue
    )

  val runInBrowser = TaskKey[Unit]("runInBrowser", "Extracts the source code")
  
  val browserTask = runInBrowser := {
    
    val maps = (resourceDirectory in Compile).value / "maps"
    val ghosts = (resourceDirectory in Compile).value / "ghosts"
    
    val outBase = (classDirectory in Compile).value
    
    val Main = (testLoader in Test).value.loadClass("ilc.language.gcc.ProgramBase")
    val main = Main.newInstance()
    
    val lambdaMan = Main.getMethod("results").invoke(main)
    
    val map = IO.read(maps / "map1.txt")
    val ghost1 = IO.read(ghosts / "ghost1.ghc")
    val ghost2 = ""
    val ghost3 = ""
    val ghost4 = ""
    
    val template = 
      <html>
        <head>
          <link href="stylesheet2.css" rel="stylesheet" type="text/css" />
          <script language="javascript" src="jquery.js"></script>
          <script language="javascript" src="game.js"></script>
          <script language="javascript" src="loader.js"></script>
        </head>
        <body>
          <header>
            <div class="container"><h1>ICFP Programming Contest 2014</h1></div>
          </header>
          <div class="container"><section id="main_content">
            <h2> Input </h2>
            <div id="inputs">
              <textarea class="twrap" id="lambda" placeholder="Lambda-Man program">{ lambdaMan }</textarea>
              <textarea class="twrap" id="map" placeholder="Map">{ map }</textarea>
              <div id="ghosts">
                <textarea class="twrap g" placeholder = "Ghost program 1">{ ghost1 }</textarea>
                <textarea class="twrap g" placeholder = "Ghost program 2">{ ghost2 }</textarea>
                <textarea class="twrap g" placeholder = "Ghost program 3">{ ghost3 }</textarea>
                <textarea class="twrap g" placeholder = "Ghost program 4">{ ghost4 }</textarea>
              </div>
            </div>
            <div id="buttons">
              <button id="load" type="button"> Load </button>
              <button class = "run" id="step" type="button"> Step </button>
              <button class = "run" id="runbreak" type="button"> Run  </button>
            </div>
            <section id="sim">
              <h2> Game </h2>
              <canvas id="maze" width="200" height="200"></canvas>
              <img src="images/alltiles.png" id="alltiles"/>
              <div id = "status"> No Program Loaded </div>
              <h5> State </h5>
              <div id = "state"> 
                Score: <span id="score">0</span> 
                Lives: <span id="lives">0</span> 
                Ticks: <span id="ticks">0</span> 
              </div>
              <h5> Trace output </h5>
              <div id = "trace"> </div>
            </section>
          </section></div>
        </body>
      </html>

      IO.write(outBase / "index.html", template.toString) 
                
  }
    
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
      consoleLogger.info("- " ++ file.getName)
      file
    })
  }

  val consoleLogger = ConsoleLogger()

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
        consoleLogger.warn(message)

      // we echo things printed to stderr as info
      case Level.Error =>
        consoleLogger.info(message)

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

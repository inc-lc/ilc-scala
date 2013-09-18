package ilc
package util
package process

/**
 * FunProcess (functional process) is viewing a system
 * command as a function from String to String
 */

import scala.io.Source
import scala.sys.process._

class ProcessFailureError(cmd: Seq[String], code: Int, msg: String)
extends java.lang.Error(
  "Process `" ++ cmd.mkString(" ") ++
    "`\n    failed with exit code " ++ code.toString ++
    "\n    and the following error message:\n" ++
    msg)

case class FunProcess(cmd: String*)
extends Function[String, String]
// throws ProcessFailureError, java.io.IOException
{
  val proc = Process(cmd)   // process object
  var output: String = null // log of stdout
  var error: String = null  // log of stderr

  def apply(input: String): String = {
    proc.run(new ProcessIO(
      procInput => {
        val writer = new java.io.PrintWriter(procInput)
        writer.write(input)
        writer.close()
      },
      procOutput => {
        output = readToEnd(procOutput)
        procOutput.close()
      },
      procError => {
        error = readToEnd(procError)
        procError.close()
      })).exitValue() match {

      case 0 =>
        output

      case abnormalExitCode =>
        throw new ProcessFailureError(cmd, abnormalExitCode, error)
    }
  }

  // convert java.io.InputStream to a String
  // http://stackoverflow.com/a/5221595
  def readToEnd(stream: java.io.InputStream): String = {
    Source.fromInputStream(stream).getLines().mkString("\n")
  }
}

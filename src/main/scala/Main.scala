package firrtl2digitaljs

import firrtl.Parser.{parseFile}
import logger.Logger.setOutput
import firrtl.Parser.UseInfo
import firrtl.LowFirrtlCompiler
import firrtl.ChirrtlForm
import firrtl.CircuitState
import scala.collection.immutable.Stream.Cons
import java.io.PrintStream
import java.io.ByteArrayOutputStream
import firrtl2digitaljs.digitaljs.DigitalJs
import firrtl.options.Dependency

object Main {

  def main(args: Array[String]) : Unit = {
    if (args.length < 1) {
      println("No file passed");
      return ()
    }

    if (args.length >= 1)
      try {
        // Redirects firrtl compiler log to firrtl.log
        setOutput("firrtl.log");
        val firrtl = parseFile(args.head, UseInfo);
        val djs = convert(firrtl, true);
        println(djs)
      }
      catch {
        case ex : java.nio.file.NoSuchFileException =>
          println(s"File not found ${ex.getMessage()}");
        case ex : Throwable =>
          println(s"Failed to convert. Exception message ${ex}")
      }

    if (args.length > 1)
      println(s"Ignoring files: ${args.tail mkString ", "}");
  }

  def convert(circuit : firrtl.ir.Circuit, transform_io: Boolean) : String = {
    val low_firrtl = lowerFirrtl(circuit);
    val digitaljs = (new Converter).convertWithOpts(low_firrtl, transform_io);
    digitaljs.toJson();
  }

  def lowerFirrtl(circuit : firrtl.ir.Circuit) : firrtl.ir.Circuit = {
    val compiler = new firrtl.stage.transforms.Compiler(
        targets = Dependency[RemoveSinksUsedAsSources] +: firrtl.stage.Forms.LowForm
      )
    val lowered = compiler.execute(CircuitState(circuit, Seq.empty))
    System.err.println(lowered.circuit.serialize)
    lowered.circuit
  }
}

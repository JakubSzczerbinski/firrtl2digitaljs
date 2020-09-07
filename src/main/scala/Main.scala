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
        val djs = convert(args.head);
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

  def convert(file : String) : String = {
    val firrtl = parseFile(file, UseInfo);
    val low_firrtl = lowerFirrtl(firrtl);
    println(low_firrtl.serialize);
    val digitaljs = (new Converter).convert(low_firrtl);
    digitaljs.toJson();
  }

  def lowerFirrtl(circuit : firrtl.ir.Circuit) : firrtl.ir.Circuit = {
    val lowfirrtlC = new LowFirrtlCompiler()
    lowfirrtlC.compileAndEmit(CircuitState(circuit, ChirrtlForm)).circuit
  }
}

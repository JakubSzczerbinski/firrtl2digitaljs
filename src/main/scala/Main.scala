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

  val usage = """
  Usage: ./firrtl2digitaljs [-o <output_filename>] [--log <log_filename>] input_filename

  Options:
    --log <log_filename>    -- set log filename (default: firrtl.log)
    -o <output_filename>    -- set output filename (default: input_filename with .json extension)
  """

  type OptionMap = Map[Symbol, String]
  def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
    list match {
      case "--log" :: filename :: opts =>
        nextOption(map ++ Map('log_filename -> filename), opts)
      case "-o" :: filename :: opts =>
        nextOption(map ++ Map('output_filename -> filename), opts)
      case opt :: opts if opt(0) == '-' =>
        throw new RuntimeException("Unkown option: " + opt);
      case filename :: opts =>
        nextOption(map ++ Map('input_filename -> filename), opts)
      case Nil =>
        map
    }
  }

  def changeExtension(str : String, ext : String) = {
    def segments = str.split('.')
    if (segments.length == 1)
      segments(0) + "." + ext
    else 
      segments.dropRight(1).mkString + "." + ext
  }

  def writeToFile(content: String, filename : String) : Unit = {
    new java.io.PrintWriter(filename) { write(content); close }
  }

  def main(args: Array[String]) : Unit = {
    val arglist = args.toList
    val options = nextOption(Map(), arglist)

    val input_filename = options.get('input_filename) match {
      case Some(value) => value
      case None => {
        println(usage)
        throw new RuntimeException("Missing input filename")
      }
    }
    val log_filename = options.getOrElse('log_filename, "firrtl.log")
    val output_filename = options.getOrElse('output_filename, changeExtension(input_filename, "json"))

    setOutput(log_filename);
    val firrtl = parseFile(input_filename, UseInfo);
    val (djs, scripts) = (new Converter).convert(firrtl, true);
    writeToFile(djs.toJson, output_filename);
  }
}

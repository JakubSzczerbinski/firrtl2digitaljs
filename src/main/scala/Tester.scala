
package firrtl2digitaljs

import firrtl2digitaljs.digitaljs.DigitalJs
import firrtl_interpreter.InterpretiveTester
import scala.sys.process._
import java.io.InputStream
import java.io.OutputStream
import java.{util => ju}
import java.nio.charset.StandardCharsets
import com.github.pathikrit.dijon._
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import firrtl.LowFirrtlCompiler
import firrtl.CircuitForm
import firrtl.ChirrtlForm
import firrtl.CircuitState
import firrtl_interpreter.InterpreterException
import logger.Logger.setOutput

trait Tester {
  def peek(output : String) : BigInt
  def poke(input : String, value: BigInt) : Unit
  def step(n : Int) : Unit
}

class FirrtlTester(dut: String) extends Tester {
  val tester = new InterpretiveTester(dut);
  def peek(output : String) = tester.peek(output)
  def poke(input: String, value: BigInt): Unit = tester.poke(input, value)
  def step(n : Int) : Unit = tester.step(n)
}

class DigitalJsTester(dut: String) extends Tester {
  val testerController = new SubprocessController(Seq("node", "external/digitaljs_peek_poke_tester/index.js"));
  setOutput("test.log");
  val djs = convertCircuit(dut);
  init();

  def convertCircuit(dut : String) : String = {
    val circuit = firrtl.Parser.parse(dut);
    (new Converter).convert(circuit, false)._1.toJson
  }

  def init() = {
    val msg = json"""{
      "type": "load",
      "circuit": $djs
    }""".toString;
    testerController.sendMsg(msg);
    val response = parse(testerController.receive()).toMap
    if (response("type") != "ok") {
      throw new Exception(s"Expected ok response for ${msg}, received: ${response}.")
    }
    assert(response("type") == "ok")
  }

  def expect(output: String, expected: BigInt) : Unit = {
    val value = peek(output)
    if (value != expected) {
      throw InterpreterException(s"Error:expect($output, $expected) got ${value}.")
    }
  }

  def peek(output: String): BigInt = {
    testerController.sendMsg(json"""{
      "type": "peek",
      "name": "$output"
    }""".toString)
    val response = parse(testerController.receive()).toMap
    if (response("type") != "value") {
      throw new Exception(s"Expected value response, received: ${response}.")
    }
  
    response("value").asString match {
      case Some(value) if response("type") == "value" => BigInt(value, 10);
      case _ => throw new Exception(s"Expected value response, received ${response}")
    }
  }

  def poke(input: String, value: BigInt): Unit = {
    testerController.sendMsg(json"""{
      "type": "poke",
      "name": "$input",
      "value": "$value"
    }""".toString)
    val response = parse(testerController.receive()).toMap
    assert(response("type") == "ok")
  }

  def step(n: Int = 1): Unit = {
    testerController.sendMsg(json"""{
      "type": "step",
      "steps": $n
    }""".toString)
    val response = parse(testerController.receive()).toMap
    assert(response("type") == "ok")
  }

  def report() : Unit = ()
}

class SubprocessController(cmd : Seq[String]) {
  val sub = os.proc(cmd).spawn();

  def sendMsg(msg: String) = {
    val base64 = ju.Base64.getEncoder.encodeToString(msg.getBytes(StandardCharsets.UTF_8))
    sub.stdin.writeLine(base64)
    sub.stdin.flush()
  }

  def receive() : String = {
    val msg = sub.stdout.readLine();
    val decoded = ju.Base64.getDecoder().decode(msg.getBytes(StandardCharsets.UTF_8))
    val str = new String(decoded, StandardCharsets.UTF_8)
    str
  }
}



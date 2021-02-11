package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Naive fulladder implementation
class Fulladder extends Module {
  val io = IO(new Bundle {
    val a     = Input(UInt(1.W))
    val b     = Input(UInt(1.W))
    val cin   = Input(UInt(1.W))
    val s     = Output(UInt(1.W))
    val cout  = Output(UInt(1.W))
  })

  io.s := io.a ^ io.b ^ io.cin;
  io.cout := (io.a & io.b) | (io.b & io.cin) | (io.a & io.cin);
}

class Lsfr extends Module {
  val io = IO(new Bundle {
    val out   = Output(Vec(8, UInt(1.W)))
  })

  val reg = RegInit(VecInit(1.U(8.W).toBools));
  reg(0) := !(reg(3) ^ reg(7));
  for (i <- 1 to 7) {
    reg(i) := reg(i - 1);
  }
  io.out := reg;
}

class GenerateExamples extends AnyFlatSpec with Matchers {
  def writeToFile(filename : String, content : String) {
    new java.io.PrintWriter(filename) { write(content); close }
  }

  "Examples" should "be generated" in {
    val examples = Seq(
      ("fulladder", Driver.emit(() => new Fulladder())),
      ("lsfr", Driver.emit(() => new Lsfr())),
      ("gcd", Driver.emit(() => new GCD())),
    )

    examples foreach {
      case (name, firrtl) => {
        writeToFile("examples/" + name + ".firrtl", firrtl)
      }
    }
  }
}

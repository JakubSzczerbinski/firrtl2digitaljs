



package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

object ShiftCalculator {
  def computeShift(a: Int, b: Int): Int = (a << b) % 256;
}

class Shift extends Module {
  val io = IO(new Bundle {
    val a     = Input(UInt(8.W))
    val b     = Input(UInt(3.W))
    val v     = Output(UInt(8.W))
  })

  io.v := io.a << io.b;
}

class ShiftSpec extends AnyFlatSpec with Matchers {

  "Shift" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new Shift)
    val tester = new InterpretiveTester(s);

    for {
      i <- 0 to 255
      j <- 0 to 7
    } {
      tester.poke("io_a", i)
      tester.poke("io_b", j)
      assert(tester.peek("io_v") == ShiftCalculator.computeShift(i, j))
    }
  }
}


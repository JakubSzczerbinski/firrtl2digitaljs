package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

object ShiftCalculator {
  def computeShiftLeft(a: Int, b: Int): Int = (a << b) % 256;
  def computeShiftRight(a: Int, b: Int): Int = (a >> b);
}

class Shift extends Module {
  val io = IO(new Bundle {
    val a    = Input(UInt(8.W))
    val b    = Input(UInt(3.W))
    val dshr = Output(UInt(8.W))
    val dshl = Output(UInt(8.W))
    val shr2 = Output(UInt(8.W))
    val shl3 = Output(UInt(8.W))
  })

  io.dshr := io.a >> io.b;
  io.dshl := io.a << io.b;
  io.shr2 := io.a >> 2;
  io.shl3 := io.a << 3;
}

class ShiftSpec extends AnyFlatSpec with Matchers {

  "Shift" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new Shift);
    println(s);
    val tester = new DigitalJsTester(s);

    for {
      i <- 0 to 255
      j <- 0 to 7
    } {
      tester.poke("io_a", i)
      tester.poke("io_b", j)
      assert(tester.peek("io_dshr") == ShiftCalculator.computeShiftRight(i, j))
      assert(tester.peek("io_dshl") == ShiftCalculator.computeShiftLeft(i, j))
      assert(tester.peek("io_shr2") == ShiftCalculator.computeShiftRight(i, 2))
      assert(tester.peek("io_shl3") == ShiftCalculator.computeShiftLeft(i, 3))
    }
  }
}


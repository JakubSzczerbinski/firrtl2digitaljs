
package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

object GCDCalculator {
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while(y > 0 ) {
      if (x > y) {
        x -= y
      }
      else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }
}

class GCD extends Module {
  val io = IO(new Bundle {
    val a     = Input(UInt(16.W))
    val b     = Input(UInt(16.W))
    val e     = Input(Bool())
    val z     = Output(UInt(16.W))
    val v     = Output(Bool())
  })

  val x  = Reg(UInt())
  val y  = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.e) {
    x := io.a
    y := io.b
  }

  io.z := x
  io.v := y === 0.U
}


class InterpreterUsageSpec extends AnyFlatSpec with Matchers {

  "GCD" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new GCD)
    val tester = new DigitalJsTester(s);

    for {
      i <- 90 to 100
      j <- 90 to 100
    } {
      tester.poke("io_a", i)
      tester.poke("io_b", j)
      tester.poke("io_e", 1)
      tester.step(2)
      tester.poke("io_e", 0)

      var cycles = 0
      while (tester.peek("io_v") != BigInt(1)) {
        tester.step(1)
        cycles += 1
      }

      assert(tester.peek("io_z") == BigInt(GCDCalculator.computeGcd(i, j)._1))
      // uncomment the println to see a lot of output
      // println(f"GCD(${i}%3d, ${j}%3d) => ${tester.peek("io_z")}%3d in $cycles%3d cycles")
    }
    tester.report()
  }
}

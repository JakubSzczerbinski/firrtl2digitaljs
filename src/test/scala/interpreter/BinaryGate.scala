package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl_interpreter.InterpretiveTester

class Gates extends Module {
  val io = IO(new Bundle {
    val lhs = Input(UInt(1.W))
    val rhs = Input(UInt(1.W))
    val and = Output(UInt(1.W))
    val or  = Output(UInt(1.W))
    val xor = Output(UInt(1.W))
  })

  io.and := io.lhs & io.rhs;
  io.or  := io.lhs | io.rhs;
  io.xor := io.lhs ^ io.rhs;
}

class GatesSpec extends AnyFlatSpec with Matchers {
  "Gates" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new Gates)
    val tester = new DigitalJsTester(s);

    tester.poke("io_lhs", 0);
    tester.poke("io_rhs", 0);
    tester.expect("io_and", 0);
    tester.expect("io_or",  0);
    tester.expect("io_xor", 0);

    tester.poke("io_lhs", 0);
    tester.poke("io_rhs", 1);
    tester.expect("io_and", 0);
    tester.expect("io_or",  1);
    tester.expect("io_xor", 1);

    tester.poke("io_lhs", 1);
    tester.poke("io_rhs", 0);
    tester.expect("io_and", 0);
    tester.expect("io_or",  1);
    tester.expect("io_xor", 1);

    tester.poke("io_lhs", 1);
    tester.poke("io_rhs", 1);
    tester.expect("io_and", 1);
    tester.expect("io_or",  1);
    tester.expect("io_xor", 0);
  }
}

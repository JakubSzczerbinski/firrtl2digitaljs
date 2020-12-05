package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class UArith extends Module {
  val io = IO(new Bundle {
    val lhs = Input(UInt(6.W))
    val rhs = Input(UInt(6.W))
    val add = Output(UInt(7.W))
    val sub = Output(SInt(7.W))
    val mul = Output(UInt(12.W))
    val div = Output(UInt(6.W))
    val mod = Output(UInt(6.W))
  })

  io.add := io.lhs +& io.rhs;
  io.sub := (io.lhs -& io.rhs).asSInt;
  io.mul := io.lhs *  io.rhs;
  io.div := io.lhs /  io.rhs;
  io.mod := io.lhs %  io.rhs;
}


class SArith extends Module {
  val io = IO(new Bundle {
    val lhs = Input(SInt(6.W))
    val rhs = Input(SInt(6.W))
    val add = Output(SInt(7.W))
    val sub = Output(SInt(7.W))
    val mul = Output(SInt(12.W))
    val div = Output(SInt(7.W))
    val mod = Output(SInt(6.W))
  })

  io.add := io.lhs +& io.rhs;
  io.sub := io.lhs -& io.rhs;
  io.mul := io.lhs *  io.rhs;
  io.div := io.lhs /  io.rhs;
  io.mod := io.lhs %  io.rhs;
}



class BinaryArithSpec extends AnyFlatSpec with Matchers {

  "UArith" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new UArith)
    val tester = new DigitalJsTester(s);

    for {
      i <- 0 to 63
      j <- 1 to 63
    } {
      tester.poke("io_lhs", i)
      tester.poke("io_rhs", j)
      assert(tester.peek("io_add") == i + j);
      assert(tester.peek("io_sub") == i - j);
      assert(tester.peek("io_mul") == i * j);
      assert(tester.peek("io_div") == i / j);
      assert(tester.peek("io_mod") == i % j);
    }
  }

  
  "SArith" should "return correct values for a range of inputs" in {
    val s = Driver.emit(() => new SArith)
    val tester = new DigitalJsTester(s);

    for {
      i <- -32 to 31
      j <- (-32 to -1) ++ (1 to 31)
    } {
      tester.poke("io_lhs", i)
      tester.poke("io_rhs", j)
      assert(tester.peek("io_add") == i + j);
      assert(tester.peek("io_sub") == i - j);
      assert(tester.peek("io_mul") == i * j);
      assert(tester.peek("io_div") == i / j);
      assert(tester.peek("io_mod") == i % j);
    }
  }
}

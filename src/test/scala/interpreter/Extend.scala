
package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ZeroExtend extends Module {
    val io = IO(new Bundle {
        val data_in     = Input(UInt(8.W))
        val data_out    = Output(UInt(16.W))
    })

    io.data_out := io.data_in;
}

class SignExtend extends Module {
    val io = IO(new Bundle {
        val data_in     = Input(SInt(8.W))
        val data_out    = Output(SInt(16.W))
    })

    io.data_out := io.data_in;
}

class ExtendSpec extends AnyFlatSpec with Matchers {

    "ZeroExtend" should " zero return correct values for a range of inputs" in {
        val s = Driver.emit(() => new ZeroExtend)
        val tester = new InterpretiveTester(s);

        for {
            i <- 0 to 255
        } {
            tester.poke("io_data_in", i);
            assert(tester.peek("io_data_out") == i);
        }
    }

    "SignExtend" should " zero return correct values for a range of inputs" in {
        val s = Driver.emit(() => new SignExtend)
        val tester = new InterpretiveTester(s);

        for {
            i <- -128 to 127
        } {
            tester.poke("io_data_in", i);
            assert(tester.peek("io_data_out") == i);
        }
    }
}

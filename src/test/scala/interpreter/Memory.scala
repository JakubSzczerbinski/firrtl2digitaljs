package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SimpleMemory extends Module {
    val io = IO(new Bundle {
    val clk         = Input(Clock())
    val w           = Input(Bool())
    val addr        = Input(UInt(8.W))
    val data_in     = Input(UInt(8.W))
    val data_out    = Output(UInt(8.W))
    })

    val mem = Mem(256, UInt(8.W))
    when(io.w) {
        mem(io.addr) := io.data_in;
    }
    io.data_out := mem(io.addr);
}

class MemorySpec extends AnyFlatSpec with Matchers {

    "Memory" should "return correct values for a range of inputs" in {
        val s = Driver.emit(() => new SimpleMemory)
        val tester = new DigitalJsTester(s);

        tester.poke("io_w", 1)
        for {
            i <- 0 to 255
        } {
            tester.poke("io_addr", i);
            tester.poke("io_data_in", (i * i) % 256);
            tester.step();
        }

        tester.poke("io_w", 0)
        for {
            i <- 0 to 255
        } {
            tester.poke("io_addr", i);
            tester.step();
            assert(tester.peek("io_data_out") == (i * i) % 256);
        }
    }
}


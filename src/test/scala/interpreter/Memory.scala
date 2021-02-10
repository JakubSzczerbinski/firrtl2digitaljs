package firrtl2digitaljs

import chisel3._
import firrtl_interpreter.InterpretiveTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ChiselMemory extends Module {
    val io = IO(new Bundle {
        val w           = Input(Bool())
        val addr        = Input(UInt(8.W))
        val data_in     = Input(UInt(8.W))
        val data_out    = Output(UInt(8.W))
    })

    val mem = SyncReadMem(256, UInt(8.W))
    when(io.w) {
        mem(io.addr) := io.data_in;
    }
    io.data_out := mem(io.addr);
}


class MemorySpec extends AnyFlatSpec with Matchers {

    val memoryGenerator = (readLatency : Int, writeLatency : Int, readUnderWrite : String) =>
        s"""
        |circuit SimpleMemory : 
        |  module SimpleMemory : 
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip w : UInt<1>, flip addr : UInt<8>, flip data_in : UInt<8>, data_out : UInt<8>}
        |    
        |    mem ram: 
        |      data-type => UInt<8>
        |      depth => 256
        |      read-latency => ${readLatency}
        |      write-latency => ${writeLatency}
        |      reader => r
        |      writer => w
        |      read-under-write => undefined
        |
        |    ram.w.data <= io.data_in
        |    ram.w.mask <= io.w
        |    ram.w.addr <= io.addr
        |    ram.w.en <= io.w
        |    ram.w.clk <= clock
        |
        |    ram.r.addr <= io.addr
        |    ram.r.en <= UInt<1>("h1")
        |    ram.r.clk <= clock
        |    io.data_out <= ram.r.data
        """.stripMargin

    val rwMemoryGenerator = (readLatency : Int, writeLatency : Int, readUnderWrite : String) =>
        s"""
        |circuit SimpleMemory : 
        |  module SimpleMemory : 
        |    input clock : Clock
        |    input reset : UInt<1>
        |    output io : {flip w : UInt<1>, flip addr : UInt<8>, flip data_in : UInt<8>, data_out : UInt<8>}
        |    
        |    mem ram: 
        |      data-type => UInt<8>
        |      depth => 256
        |      read-latency => ${readLatency}
        |      write-latency => ${writeLatency}
        |      readwriter => rw
        |      read-under-write => undefined
        |
        |    ram.rw.wmode <= io.w
        |    io.data_out <= ram.rw.rdata
        |    ram.rw.wdata <= io.data_in
        |    ram.rw.wmask <= UInt<1>("h1")
        |    ram.rw.addr <= io.addr
        |    ram.rw.en <= UInt<1>("h1")
        |    ram.rw.clk <= clock
        """.stripMargin

    val asyncReadMemory = memoryGenerator(0, 1, "undefined")
    val simpleMemory = memoryGenerator(1, 1, "undefined")
    val simpleRw = rwMemoryGenerator(1, 1, "undefined")

    "simpleMemory" should "return correct values for a range of inputs" in {
        val tester = new DigitalJsTester(simpleMemory);
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
            tester.step()
            assert(tester.peek("io_data_out") == (i * i) % 256);
        }
    }

    "asyncReadMemory" should "return correct values for a range of inputs" in {
        val tester = new DigitalJsTester(asyncReadMemory);
        tester.poke("io_w", 1)
        for {
            i <- 0 to 255
        } {
            tester.poke("io_addr", i);
            tester.poke("io_data_in", (i * i) % 256);
            tester.step();
        }

        tester.poke("io_w", 0)
        tester.poke("io_addr", 0);
        tester.step();
        for {
            i <- 1 to 255
        } {
            tester.poke("io_addr", i);
            assert(tester.peek("io_data_out") == (i * i) % 256);
        }
    }

    "readWriteMemory" should "return correct values for a range of inputs" in {
        val tester = new DigitalJsTester(simpleRw);
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
            tester.step()
            assert(tester.peek("io_data_out") == (i * i) % 256);
        }
    }
}


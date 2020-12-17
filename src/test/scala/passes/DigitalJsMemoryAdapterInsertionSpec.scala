package firrtl2digitaljs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import firrtl.options.Dependency
import firrtl.CircuitState
import firrtl.ir.Circuit
import firrtl.ir.DefModule
import firrtl.ir.Statement
import firrtl.ir.Connect
import chisel3._
import firrtl.LowForm
import firrtl.ir.IntParam

//scalastyle:off magic.number

class MemCirc extends Module {
  val io = IO(new Bundle {
    val clk         = Input(Clock())
    val w           = Input(Bool())
    val addr        = Input(UInt(8.W))
    val data_in     = Input(SInt(8.W))
    val data_out    = Output(SInt(8.W))
  })

  val mem = Mem(256, SInt(8.W))
  when(io.w) {
      mem(io.addr) := io.data_in;
  }
  io.data_out := mem(io.addr);
}

class DigitalJsMemoryAdapterInsertionSpec extends AnyFlatSpec with Matchers {

  def transform(s : String) : Circuit = {
      val cc = TestUtils.parse(s);
      val compiler = new firrtl.stage.transforms.Compiler(
          targets = firrtl.stage.Forms.LowForm ++ Seq(Dependency[DigitaljsMemoryAdapterInsertion])
      )
      compiler.execute(CircuitState(cc, Seq.empty)).circuit
  }

  val mem_circ = """
  |circuit SimpleMemory : 
  |  module SimpleMemory : 
  |    input clock : Clock
  |    input reset : UInt<1>
  |    output io : {flip clk : Clock, flip w : UInt<1>, flip addr : UInt<8>, flip data_in : UInt<8>, data_out : UInt<8>}
  |    
  |    cmem mem : UInt<8>[256] @[Memory.scala 17:18]
  |    when io.w : @[Memory.scala 18:16]
  |      infer mport _T = mem[io.addr], clock @[Memory.scala 19:12]
  |      _T <= io.data_in @[Memory.scala 19:22]
  |      skip @[Memory.scala 18:16]
  |    infer mport _T_1 = mem[io.addr], clock @[Memory.scala 21:23]
  |    io.data_out <= _T_1 @[Memory.scala 21:17]
  """.stripMargin

  "DigitalJsMemoryAdapterInsertion" should "remove all memories in memory circuit" in {
    val result = transform(mem_circ);

    val extModulesOfCircuit = TestUtils.extModulesOfCircuit(result)
    extModulesOfCircuit.length should equal(1);

    val memModule = extModulesOfCircuit(0);
    memModule.params should contain theSameElementsAs Seq(
      IntParam("ADDR_WIDTH", 8),
      IntParam("SIZE", 8),
      IntParam("READERS", 1),
      IntParam("WRITERS", 1)
    )
    memModule.defname should equal("DIGITALJS_MEMORY")
  }
}

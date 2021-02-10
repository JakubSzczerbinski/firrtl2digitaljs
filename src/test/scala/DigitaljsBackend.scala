package firrtl2digitaljs

import chisel3.testers.TesterDriver
import chisel3.Module
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3.internal.firrtl.Circuit
import firrtl.CircuitState

class MuxExample extends Module {
    val io = IO(new Bundle {
        val sel =   Input(UInt(2.W))
        val in  =   Input(Vec(4, UInt(1.W)))
        val out =   Output(UInt(1.W))
    })

    io.out := io.in(io.sel)
}


class EmitMuxExample extends AnyFlatSpec with Matchers {
    "Example" should "be emitted" in {
        val x = Driver.emit(() => new MuxExample())
        val compiler = new firrtl.stage.transforms.Compiler(
            targets = firrtl.stage.Forms.LowForm
        )
        val y = TestUtils.parse(x);
    }
}



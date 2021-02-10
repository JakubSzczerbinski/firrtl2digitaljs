package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import firrtl_interpreter.InterpretiveTester
import firrtl.Parser.{parseFile}
import firrtl.Parser.UseInfo

class RiscvSpec extends AnyFlatSpec with Matchers {
  val s = parseFile("firrtl_src/Riscv.fir", UseInfo);
  "Riscv" should "be loaded" in {
    // val tester = new DigitalJsTester(s.serialize);
  }
}

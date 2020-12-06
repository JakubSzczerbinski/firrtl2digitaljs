package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication
import scala.collection.GenTraversable

class UnarySpec extends AnyFlatSpec with Matchers {

    "Unary" should " zero return correct values for a range of inputs" in {
        val s = s"""
            |circuit Unary : 
            |  module Unary : 
            |    input clock : Clock
            |    input reset : UInt<1>
            |    output io : {flip in : UInt<8>, neg : SInt<9>, not : UInt<8>}
            |    
            |    io.neg <= neg(io.in) @[neg]
            |    io.not <= not(io.in) @[not]
        """.stripMargin
        val tester = new DigitalJsTester(s);

        for {
            i <- 0 to 255
        } {
            tester.poke("io_in", i);
            assert(tester.peek("io_not") == 255 - i);
            assert(tester.peek("io_neg") == -i)
        }
    }
}

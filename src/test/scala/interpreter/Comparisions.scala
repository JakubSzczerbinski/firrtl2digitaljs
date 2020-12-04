
package firrtl2digitaljs

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.dsl.ResultOfAllElementsOfApplication
import scala.collection.GenTraversable

class UComparisions extends Module {
    val io = IO(new Bundle {
        val lhs     = Input(UInt(6.W))
        val rhs     = Input(UInt(6.W))
        val lt      = Output(Bool())
        val leq     = Output(Bool())
        val gt      = Output(Bool())
        val geq     = Output(Bool())
        val eq      = Output(Bool())
        val neq     = Output(Bool())
    })

    io.lt  := io.lhs <   io.rhs;
    io.leq := io.lhs <=  io.rhs;
    io.gt  := io.lhs >   io.rhs;
    io.geq := io.lhs >=  io.rhs;
    io.eq  := io.lhs === io.rhs;
    io.neq := io.lhs !=  io.rhs;
}

class SComparisions extends Module {
    val io = IO(new Bundle {
        val lhs     = Input(SInt(6.W))
        val rhs     = Input(SInt(6.W))
        val lt      = Output(Bool())
        val leq     = Output(Bool())
        val gt      = Output(Bool())
        val geq     = Output(Bool())
        val eq      = Output(Bool())
        val neq     = Output(Bool())
    })

    io.lt  := io.lhs <   io.rhs;
    io.leq := io.lhs <=  io.rhs;
    io.gt  := io.lhs >   io.rhs;
    io.geq := io.lhs >=  io.rhs;
    io.eq  := io.lhs === io.rhs;
    io.neq := io.lhs !=  io.rhs;
}

class ComparisionSpec extends AnyFlatSpec with Matchers {
    def intOfBool(b : Boolean) = 
        if (b)
            1
        else
            0

    "UComparision" should " zero return correct values for a range of inputs" in {
        val s = Driver.emit(() => new UComparisions)
        val tester = new DigitalJsTester(s);

        for {
            i <- 0 to 63
            j <- 0 to 63
        } {
            tester.poke("io_lhs", i);
            tester.poke("io_rhs", j);
            assert(tester.peek("io_lt")  == intOfBool(i <  j));
            assert(tester.peek("io_leq") == intOfBool(i <= j));
            assert(tester.peek("io_gt")  == intOfBool(i >  j));
            assert(tester.peek("io_geq") == intOfBool(i >= j));
            assert(tester.peek("io_eq")  == intOfBool(i == j));
            assert(tester.peek("io_neq") == intOfBool(i != j));
        }
    }

    "SComparison" should " zero return correct values for a range of inputs" in {
        val s = Driver.emit(() => new SComparisions)
        val tester = new DigitalJsTester(s);

        for {
            i <- -32 to 31
            j <- -32 to 31
        } {
            tester.poke("io_lhs", i);
            tester.poke("io_rhs", j);
            assert(tester.peek("io_lt")  == intOfBool(i <  j));
            assert(tester.peek("io_leq") == intOfBool(i <= j));
            assert(tester.peek("io_gt")  == intOfBool(i >  j));
            assert(tester.peek("io_geq") == intOfBool(i >= j));
            assert(tester.peek("io_eq")  == intOfBool(i == j));
            assert(tester.peek("io_neq") == intOfBool(i != j));
        }
    }
}


package firrtl2digitaljs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import chisel3.testers.BasicTester
import chisel3._

class CounterWithStop extends BasicTester {
    val acc = Reg(UInt(3.W))
    acc := acc + 1.U;
    when(acc === 7.U) {
        stop();
    }
}

class CounterWithAssert extends BasicTester {
    val acc = Reg(UInt(3.W))
    acc := acc + 1.U;
    chisel3.assert(acc < 7.U)
}

class CounterWithPrintf extends BasicTester {
    val acc = Reg(UInt(3.W))
    acc := acc + 1.U;
    chisel3.printf("Acc=%x", acc);
}

class DebugSpec extends AnyFlatSpec with Matchers {
    "Counter with stop" should "stop" in {
        val counter = Driver.emit(() => new CounterWithStop())
        val c = (new Converter).convert(TestUtils.parse(counter), false);
    }

    "Counter with assert" should "trigger assert" in {
        val counter = Driver.emit(() => new CounterWithAssert())
        val c = (new Converter).convert(TestUtils.parse(counter), false);
    }

    "Counter with printf" should "print" in {
        val counter = Driver.emit(() => new CounterWithPrintf())
        val c = (new Converter).convert(TestUtils.parse(counter), false);
    }
}

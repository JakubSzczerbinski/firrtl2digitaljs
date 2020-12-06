package firrtl2digitaljs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HeadSpec extends AnyFreeSpec with Matchers {
  "Head returns first bit" in {
    val input =
      """
        |circuit headModule :
        |  module headModule :
        |    input io_In : UInt<8>
        |    output io_Out : UInt<4>
        |
        |    node T_1 = head(io_In, 4)
        |    io_Out <= T_1
      """.stripMargin
    val tester = new DigitalJsTester(input);
    for {i <- 0 to 255} {

      tester.poke("io_In", i);
      assert(tester.peek("io_Out") == i / 16)
    }
  }
}
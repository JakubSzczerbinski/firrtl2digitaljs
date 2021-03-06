/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package firrtl2digitaljs

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OutputAsSourceSpec extends AnyFreeSpec with Matchers {
  "it must be possible for the interpreter to handle module outputs as rhs dependencies" in {
    val input =
      """
        |circuit UseOutput :
        |  module UseOutput :
        |    input reset : UInt<1>
        |    input in1 : UInt<2>
        |    output out1 : UInt<2>
        |    output out2 : UInt<2>
        |
        |    out1 <= in1
        |    node T_1 = add(out1, UInt<1>("h1"))
        |    out2 <= T_1
      """.stripMargin

    val tester = new DigitalJsTester(input)

    tester.poke("in1", 1)

    tester.expect("out1", 1)
    tester.expect("out2", 2)
  }
}

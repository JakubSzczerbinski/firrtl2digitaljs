package firrtl2digitaljs

import scala.io.Source
import firrtl.ir.Circuit
import firrtl.Parser

object TestUtils {
    def asSigned(value: BigInt, bits : BigInt) =
        if(value >= Math.pow(2, (bits - 1).toDouble).toInt)
            value - Math.pow(2, bits.toDouble).toInt
        else
            value
    
    def readFile(path : String) = {
        Source.fromFile(path).getLines.mkString
    }

    def parse(firrtl : String) : Circuit = {
        val circuit = Parser.parse(firrtl)
        Main.lowerFirrtl(circuit)
    }
}


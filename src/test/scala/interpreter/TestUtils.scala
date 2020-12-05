package firrtl2digitaljs

object TestUtils {
    def asSigned(value: BigInt, bits : BigInt) = 
        if(value >= Math.pow(2, (bits - 1).toDouble).toInt)
            value - Math.pow(2, bits.toDouble).toInt
        else
            value
}


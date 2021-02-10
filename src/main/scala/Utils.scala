package firrtl2digitaljs

import firrtl.ir._
import firrtl.PrimOps._
import digitaljs._
import firrtl.Dshlw

object Utils {}

object ConveterUtils {
  def maybeGetIntParam(params: Seq[Param], name: String): Option[BigInt] =
    (params filter {
      case IntParam(param_name, value) if name == param_name => true
      case _                                                 => false
    } map { case IntParam(name, value) =>
      value
    }).headOption
  
  def getIntParam(params: Seq[Param], name: String): BigInt =
    maybeGetIntParam(params, name).get

  def getBoolParam(params : Seq[Param], name : String) : Boolean =
    getIntParam(params, name).toInt match {
      case 0 => false
      case 1 => true
    }

  def isClockType(tpe: Type) =
    tpe match {
      case ClockType => true
      case _         => false
    }

  def reduceGateOfPrimOp(op: PrimOp): ReduceGateType =
    op match {
      case Andr => AndReduce
      case Orr  => OrReduce
      case Xorr => XorReduce
    }

  def binGateTypeOfPrimOp(op: PrimOp): BinaryGateType =
    op match {
      case firrtl.PrimOps.And => digitaljs.And
      case firrtl.PrimOps.Or  => digitaljs.Or
      case firrtl.PrimOps.Xor => digitaljs.Xor
    }

  def binTypeOfPrimOp(op: PrimOp): BinType =
    op match {
      case Add => Addition()
      case Sub => Subtraction()
      case Mul => Multiplication()
      case Div => Division()
      case Rem => Modulo()
    }

  def compTypeOfPrimOp(op: PrimOp): CompType =
    op match {
      case Leq               => Le()
      case firrtl.PrimOps.Lt => digitaljs.Lt()
      case Geq               => Ge()
      case firrtl.PrimOps.Gt => digitaljs.Gt()
      case firrtl.PrimOps.Eq => digitaljs.Eq()
      case Neq               => Ne()
    }

  def shiftTypeOfPrimOp(op: PrimOp): ShiftType =
    op match {
      case Shr | Dshr => ShiftRight()
      case Shl | Dshl | Dshlw => ShiftLeft()
    }

  def maybeIsSigned(tpe: Type): Option[Boolean] = tpe match {
    case SIntType(width) => Some(true)
    case UIntType(width) => Some(false)
    case _               => None
  }

  def isSigned(tpe : Type) : Boolean = maybeIsSigned(tpe).get

  def makeConstantString(
    value : BigInt,
    width : BigInt
  ) : String = {
    val bits = width.toInt
    var constant = value.toString(2);
    while (constant.length < bits) {
      constant = "0" + constant;
    }
    constant
  }
}

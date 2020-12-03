package firrtl2digitaljs

import firrtl2digitaljs.digitaljs._
import firrtl.ir.DefModule
import firrtl.Mappers._
import firrtl.ir._
import firrtl.LowFirrtlCompiler
import firrtl.CircuitState
import firrtl.ChirrtlForm
import firrtl.FirrtlEmitter
import firrtl.LowFirrtlEmitter
import firrtl.bitWidth
import firrtl.PrimOps._
import firrtl.WRef
import firrtl.WSubField
import firrtl.WDefInstance
import firrtl.getWidth
import scala.collection.immutable.ListMap
import scala.collection.immutable.Stream.cons
import Chisel.Bool
import firrtl.SourceFlow
import firrtl.SinkFlow

abstract class MemPortType() {}

case object Read extends MemPortType {}
case object Write extends MemPortType {}
case object ReadWrite extends MemPortType {}

object MemPort {
  def andGateName(mem_name : String, index : Int) : String =
    mem_name + "_wr_and" + index
}

class MemPort(val index : Int, val name : String, val tpe : MemPortType) {
  def makePlug(device_name : String, endpoint : String) : Plug =
    (tpe match {
      case Read => endpoint match {
        case "data" => new Plug(device_name, "rd" + index + "data")
        case "addr" => new Plug(device_name, "rd" + index + "addr")
        case "en" => new Plug(device_name, "rd" + index + "en")
        case "clk" => new Plug(device_name, "rd" + index + "clk")
      }
      case Write => endpoint match {
        case "data" => new Plug(device_name, "wr" + index + "data")
        case "mask" => new Plug(MemPort.andGateName(device_name, index), "in1")
        case "addr" => new Plug(device_name, "wr" + index + "addr")
        case "en" => new Plug(MemPort.andGateName(device_name, index), "in2")
        case "clk" => new Plug(device_name, "wr" + index + "clk")
      }
    })
}

class Converter {
  var genId : BigInt = BigInt.int2bigInt(0);
  var memory_ports : ListMap[String, List[MemPort]] = ListMap.empty;

  def addPort(name : String, mem_port : MemPort) = {
    val portList = memory_ports get name getOrElse Nil
    memory_ports = memory_ports + (name -> (mem_port :: portList))
  }

  def convert(circuit : firrtl.ir.Circuit) : DigitalJs = convertWithOpts(circuit, true)
  def convertWithOpts(circuit : firrtl.ir.Circuit, io_transformed : Boolean) : DigitalJs = {
    val circuits = circuit.modules map convertModule toMap 
    val maybeToplevel = circuits find {case (name, _) => name == circuit.main }
    maybeToplevel match {
      case Some((_, toplevel)) => 
        new DigitalJs(
          if (io_transformed)
            toplevel.devices mapValues transformIO
          else
            toplevel.devices,
          toplevel.connectors, 
          circuits filter { case (name, _) => name != circuit.main })
      case None => {
        println("No toplevel module")
        new DigitalJs(Map.empty, Nil, circuits)
      }
    }
  }

  def transformIO : Device => Device = { case device => 
    device match {
      case digitaljs.Input(label, net, order, bits, is_clock) => 
        if (is_clock)
          Clock(label)
        else if (bits == 1)
          Button(label)
        else
          NumEntry(label, bits, "hex")
      case digitaljs.Output(label, net, order, bits) => 
        if (bits == 1)
          Lamp(label)
        else
          NumDisplay(label, bits, "hex")
      case _ => device
    }
  }

  def isClockType(tpe : Type) =
    tpe match {
      case ClockType => true
      case _ => false
    }

  def convertPort(port: Port, portNumber: Int): (String, Device) = {
    port.direction match {
      case firrtl.ir.Input =>
        (port.name, new Input(port.info.toString(), port.name, portNumber, bitWidth(port.tpe).intValue, isClockType(port.tpe)))
      case firrtl.ir.Output =>
        (port.name, new Output(
          port.info.toString(),
          port.name,
          portNumber,
          bitWidth(port.tpe).intValue
        ))
    }
  }

  def correctInputPlugsOfOutputs(ports : Seq[Port], connectors_ : List[Connector]) = {
    var connectors = connectors_
    var changed = true
    while (changed) {
      changed = false
      val inputPlugsOfOutputPorts = ports flatMap (port => {
        port.direction match {
          case firrtl.ir.Input => Seq.empty
          case firrtl.ir.Output => 
            connectors
              .find(conn => conn.to.id == port.name)
              .map(conn => (port.name, conn.from))
              .toSeq
        }
      })
      connectors = connectors map (conn => {
        val maybeFrom = inputPlugsOfOutputPorts find { case (outputName, plug) => conn.from.id == outputName } map (_._2)
        maybeFrom match {
          case None => conn
          case Some(from) => {
            changed = true
            new Connector(from, conn.to)
          }
        }
      })
    }
    connectors
  }

  def convertModule(module: DefModule): (String, digitaljs.Circuit) = {
    val io_devices = module.ports.zipWithIndex.map {
      case (p, i) => convertPort(p, i)
    }.toMap;
    var devices: Map[String, Device] = ListMap.empty;
    var connectors: List[Connector] = Nil;
    module.foreachStmt((statement) => {
      val (d, c) = convertStatement(statement);
      devices = d ++ devices;
      connectors = c ++ connectors;
    })
    connectors = correctInputPlugsOfOutputs(module.ports, connectors)
    (module.name, new digitaljs.Circuit(module.name, io_devices ++ devices, connectors))
  }

  

  def getPlug(expr: Expression): Plug =
    expr match {
      case WRef(name, tpe, kind, flow) => new Plug(name, "in")
      case WSubField(WRef(name, tp1, kind, f1), port, tp0, f0) =>
        new Plug(name, port)
      case WSubField(WSubField(WRef(name, tp2, kind, f2), mem_port, tp1, f1), field, tp0, f0) =>
        memory_ports
          .get(name)
          .flatMap((_ find (_.name == mem_port)))
          .map(port => port.makePlug(name, field))
          .get
      case Reference(name, tpe) => 
        new Plug(name, "in")
      case _ => ???
    }

  def generateIntermediateName(name: Option[String]): String = {
    if (name.isDefined) {
      return name.get;
    }
    genId += 1;
    return "__INTERMEDIATE__" + genId.toString(16);
  }

  def reduceGateOfPrimOp(op: PrimOp) : ReduceGateType =
    op match {
      case Andr => AndReduce
      case Orr => OrReduce
      case Xorr => XorReduce
    }

  def binGateTypeOfPrimOp(op: PrimOp) : BinaryGateType =
    op match {
      case firrtl.PrimOps.And => digitaljs.And
      case firrtl.PrimOps.Or => digitaljs.Or
      case firrtl.PrimOps.Xor => digitaljs.Xor
    }    

  def binTypeOfPrimOp(op: PrimOp) : BinType =
    op match {
      case Add => Addition()
      case Sub => Subtraction()
      case Mul => Multiplication()
      case Div => Division()
      case Rem => Modulo()
    }

  def compTypeOfPrimOp(op: PrimOp) : CompType =
    op match {
      case Leq => Le()
      case firrtl.PrimOps.Lt => digitaljs.Lt()
      case Geq => Ge()
      case firrtl.PrimOps.Gt => digitaljs.Gt()
      case firrtl.PrimOps.Eq => digitaljs.Eq()
      case Neq => Ne()
    }

  def shiftTypeOfPrimOp(op: PrimOp) : ShiftType =
    op match {
      case Shr | Dshr => ShiftRight()
      case Shl | Dshl => ShiftLeft()
    }

  def isSigned(tpe : Type) : Boolean = tpe match {
    case SIntType(width) => true
    case UIntType(width) => false
  }

  def convertPrimitive(
    op: PrimOp,
    args: Seq[Expression],
    consts: Seq[BigInt],
    tpe: Type,
    default_name: Option[String],
    label: String,
  ): (Map[String, Device], List[Connector], Plug) = 
    op match {
      case Add | Sub | Mul | Div | Rem =>
        val name = generateIntermediateName(default_name);
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label);
        ( (lds ++ rds)
        + (name -> new BinaryArith(
            binTypeOfPrimOp(op), 
            label, 
            bitWidth(lhs.tpe).toInt, 
            bitWidth(rhs.tpe).toInt, 
            bitWidth(tpe).toInt, false, false))
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      case firrtl.PrimOps.Leq | firrtl.PrimOps.Lt | firrtl.PrimOps.Geq | 
        firrtl.PrimOps.Gt | firrtl.PrimOps.Eq | firrtl.PrimOps.Neq =>
        val name = generateIntermediateName(default_name);
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label);
        ( (lds ++ rds)
        + (name -> new Comparision(
            compTypeOfPrimOp(op), 
            label, 
            bitWidth(lhs.tpe).toInt, 
            bitWidth(rhs.tpe).toInt, 
            false, false))
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      case Dshl | Dshr =>
        val name = generateIntermediateName(default_name);
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label);
        ( (lds ++ rds)
        + (name -> new Shift(
            shiftTypeOfPrimOp(op), 
            label, 
            bitWidth(lhs.tpe).toInt, 
            bitWidth(rhs.tpe).toInt, 
            bitWidth(tpe).toInt,
            isSigned(lhs.tpe),
            isSigned(rhs.tpe),
            isSigned(tpe), false))
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )

      case AsSInt | AsUInt | AsClock => {
        val arg = args(0);
        convertExpression(arg, label);
      }
      case Pad => {
        val name = generateIntermediateName(default_name);
        val arg = args(0);
        val padDevice = tpe match {
          case UIntType(_) => new ZeroExtend(label, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt)
          case SIntType(_) => new SignExtend(label, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt)
        }
        val (ds, cs, plug) = convertExpression(args(0), label)
        ( ds + (name -> padDevice)
        , new Connector(plug, new Plug(name, "in")) :: cs
        , new Plug(name, "out")
        )
      }
      case Shl | Shr => {
        val arg = args(0);
        val const = consts(0);
        val const_name = generateIntermediateName(None);
        val name = generateIntermediateName(default_name);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds +
          (const_name -> new Constant(label, makeConstantString(const, bitWidth(tpe)))) +
          (name -> new Shift(
            shiftTypeOfPrimOp(op),
            label, 
            bitWidth(arg.tpe).toInt,
            const.toString(2).length, 
            bitWidth(tpe).toInt, false, false, false, true))
        , new Connector(plug, new Plug(name, "in1")) ::
          new Connector(new Plug(const_name, "out"), new Plug(name, "in2")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Cvt => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        val name = generateIntermediateName(default_name);
        ( ds +
          (name -> new ZeroExtend(label, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Neg => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        val name = generateIntermediateName(default_name);
        ( ds +
          (name -> new Unary(Negation, label, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt, false))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case firrtl.PrimOps.Not => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        val name = generateIntermediateName(default_name);
        ( ds +
          (name -> new UnaryGate(digitaljs.Not, label, bitWidth(arg.tpe).toInt))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case firrtl.PrimOps.And | firrtl.PrimOps.Or | firrtl.PrimOps.Xor => {
        val name = generateIntermediateName(default_name);
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label);
        ( (lds ++ rds)
        + (name -> new BinaryGate(
            binGateTypeOfPrimOp(op),
            label,
            bitWidth(tpe).toInt))
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      }
      case Andr | Orr | Xorr => {
        val name = generateIntermediateName(default_name);
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds
        + (name -> new ReduceGate(
            reduceGateOfPrimOp(op),
            label,
            bitWidth(arg.tpe).toInt))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Cat => {
        val name = generateIntermediateName(default_name);
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label);
        ( (lds ++ rds)
        + (name -> new BusGroup(
            label,
            Array[Group](
              new Group(bitWidth(lhs.tpe).toInt),
              new Group(bitWidth(rhs.tpe).toInt)
            )
          ))
        , new Connector(lhsPlug, new Plug(name, "in0")) ::
          new Connector(rhsPlug, new Plug(name, "in1")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      }
      case Bits => {
        val name = generateIntermediateName(default_name);
        val arg = args(0);
        val hi = consts(0);
        val lo = consts(1);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds
        + (name -> new BusSlice(
            label,
            lo.toInt,
            (hi - lo + 1).toInt,
            bitWidth(arg.tpe).toInt
          ))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Head => {
        val name = generateIntermediateName(default_name);
        val arg = args(0);
        val n = consts(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds
        + (name -> new BusSlice(
            label,
            0,
            n.toInt,
            bitWidth(arg.tpe).toInt
          ))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Tail => {
        val name = generateIntermediateName(default_name);
        val arg = args(0);
        val n = consts(0);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds
        + (name -> new BusSlice(
            label,
            0,
            (bitWidth(arg.tpe) - n).toInt,
            bitWidth(arg.tpe).toInt
          ))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case _ => println("Not handled ", op.toString()); (ListMap(), Nil, new Plug("XD", "out")); // TODO Cleanup
    }

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

  def convertExpression(
      expr: Expression,
      label : String
  ): (Map[String, Device], List[Connector], Plug) = {
    val intermediateName = generateIntermediateName(None);
    convertExpression(expr, intermediateName, label);
  }

  def x(y : firrtl.Kind) : firrtl.Kind = y

  def convertExpression(
      expr: Expression,
      toplevel: String,
      label: String,
  ): (Map[String, Device], List[Connector], Plug) = {
    expr match {
      case DoPrim(op, args, consts, tpe) => convertPrimitive(op, args, consts, tpe, Some(toplevel), label);
      case FixedLiteral(value, width, point) => {
        ( ListMap(toplevel -> new Constant(label, makeConstantString(value, bitWidth(expr.tpe))))
        , Nil
        , new Plug(toplevel, "out") )
      }
      case WRef(name, tpe, kind, flow) =>
        (ListMap(), Nil, new Plug(name, "out"))
      case WSubField(_, name, tpe, flow) => (ListMap(), Nil, getPlug(expr))
      case firrtl.ir.Mux(cond, tval, fval, tpe) => {
        val (cds, ccs, condPlug) = convertExpression(cond, label);
        val (tds, tcs, tvalPlug) = convertExpression(tval, label);
        val (fds, fcs, fvalPlug) = convertExpression(fval, label);
        ( (cds ++ tds ++ fds) + (toplevel -> new digitaljs.Mux(label, bitWidth(tpe).toInt, 1))
        , new Connector(fvalPlug, new Plug(toplevel, "in0")) ::
          new Connector(tvalPlug, new Plug(toplevel, "in1")) ::
          new Connector(condPlug, new Plug(toplevel, "sel")) ::
          ccs ++ tcs ++ fcs
        , new Plug(toplevel, "out")
        )
      }
      case Reference(name, tpe) => {
        (ListMap(), Nil, new Plug(name, "out"))
      }
      case SIntLiteral(value, width) =>
        ( ListMap(toplevel -> new Constant(label, makeConstantString(value, bitWidth(expr.tpe))))
        , Nil
        , new Plug(toplevel, "out") )
      case SubAccess(expr, index, tpe) => ??? // Propably implementation not needed (not availble in low form)
      case SubField(expr, name, tpe) => ??? // Same /\
      case SubIndex(expr, value, tpe) => ??? // Same /\
      case UIntLiteral(value, width) => 
        ( ListMap(toplevel -> new Constant(label, makeConstantString(value, bitWidth(expr.tpe))))
        , Nil
        , new Plug(toplevel, "out") )
      case ValidIf(cond, value, tpe) => convertExpression(value, label);
    }
  }


  def convertStatement(
      statetment: Statement
  ): (Map[String, Device], List[Connector]) = {
    statetment match {
      case Block(stmts) =>
        return stmts
          .map(convertStatement)
          .fold((ListMap[String, Device](), Nil))((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
      case Connect(info, loc, expr) =>
        val sinkPlug = getPlug(loc);
        val (ds, cs, sourcePlug) = convertExpression(expr, info.toString());
        (ds, new Connector(sourcePlug, sinkPlug) :: cs)
      case DefInstance(info, name, module) => (ListMap(name -> new Subcircuit(name, module)), Nil)
      case WDefInstance(info, name, module, tpe) => (ListMap(name -> new Subcircuit(name, module)), Nil)
      case DefMemory(
          info,
          name,
          dataType,
          depth,
          writeLatency,
          readLatency,
          readers,
          writers,
          readwriters,
          readUnderWrite
          ) =>

        val convertReader : ((String, Int)) => ReadPort = {
          case (portName, index) => {
            addPort(name, new MemPort(index, portName, Read))
            new ReadPort(true, true, false)
          }
        }

        val convertWriter : ((String, Int)) => WritePort = {
          case (portName, index) => {
            addPort(name, new MemPort(index, portName, Write))
            new WritePort(true, true)
          }
        }

        val andGateOfWriter : ((String, Int)) => (String, Device) = {
          case (portName, index) => 
            (name + "_wr_and" + index -> new BinaryGate(
              digitaljs.And,
              "",
              1
            ))
        }

        val connectToEnableOfWriter : ((String, Int)) => Connector = {
          case (portName, index) =>
            new Connector(
              new Plug(MemPort.andGateName(name, index), "out"),
              new Plug(name, "wr" + index + "en")
            )
        }

        ( 
          (writers.zipWithIndex map andGateOfWriter).toMap +
          (name -> new Memory(
            info.toString(),
            bitWidth(dataType).toInt,
            depth.toInt,
            Math.pow(2, bitWidth(dataType).toDouble).toInt,
            0.toInt,
            readers.zipWithIndex map convertReader,
            writers.zipWithIndex map convertWriter,
            "")
          )
        , 
          (writers.zipWithIndex map connectToEnableOfWriter).toList
        )
      case DefNode(info, name, value) => {
        val (ds, cs, plug) = convertExpression(value, name, info.toString());
        if (plug.id == name)
          (ds, cs)
        else {
          val bits = bitWidth(value.tpe).toInt
          ( ds +
            (name -> new UnaryGate(Repeater, info.toString(), bits))
          , new Connector(plug, new Plug(name, "in")) ::
            cs
          )

        }
      }
      case DefRegister(info, name, tpe, clock, reset, init) => {
        val (ds, cs, clkPlug) = convertExpression(clock, info.toString());
        val arst = reset match { // TODO Implement reset with weird initiation
          case UIntLiteral(value, width) => if (value == 0) None else ??? // Same as below
          case _ => ??? // Cannot handle reset, no dynamic init in digitaljs
        }
        return (
          ds + (name -> new Dff(info.toString(), bitWidth(tpe).toInt, new Polarity(Some(true), arst, None), "")),
          (new Connector(clkPlug, new Plug(name, "clk"))) :: cs
        )
      }
      case DefWire(info, name, tpe) => (ListMap(), Nil)
      case IsInvalid(info, expr)    => (ListMap(), Nil)
      case Stop(info, ret, clk, en) => (ListMap(), Nil) // TODO Find out why its ignored
      case EmptyStmt                => (ListMap(), Nil)
      case Print(info, string, args, clk, en) =>
        println("Ignoring print statement"); (ListMap(), Nil)
      case _ => println("Illegal statement", statetment); (ListMap(), Nil)
    }
  }
}

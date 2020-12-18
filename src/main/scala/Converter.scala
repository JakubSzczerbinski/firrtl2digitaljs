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
import firrtl.SourceFlow
import firrtl.SinkFlow
import os.stat
import firrtl.Emitter
import os.write

class Converter {
  var genId : BigInt = BigInt.int2bigInt(0);
  var memory_modules : Map[String, Memory] = Map.empty;

  def findMemoryModules (circuit : firrtl.ir.Circuit) : Map[String, Memory] =
    (circuit.modules flatMap {
      case ExtModule(info, name, ports, defname, params) if defname == "DIGITALJS_MEMORY" => {
        var addrWidth = -1;
        var size = -1;
        var readers = -1;
        var writers = -1;
        params foreach {
          case IntParam(name, value) if name == "ADDR_WIDTH" => addrWidth = value.toInt;
          case IntParam(name, value) if name == "SIZE" => size = value.toInt;
          case IntParam(name, value) if name == "READERS" => readers = value.toInt;
          case IntParam(name, value) if name == "WRITERS" => writers = value.toInt;
        }
        if (addrWidth == -1 || size == -1 || readers == -1 || writers == -1) {
          System.err.println("Invalid memory params")
        }
        val rdports : Seq[ReadPort] = 0 to readers map (i => new ReadPort(true, true, false))
        val wrports : Seq[WritePort] = 0 to writers map (i => new WritePort(true, true))
        val memory = new Memory(
          info.toString, 
          size,
          addrWidth,
          Math.pow(2, size).toInt,
          0,
          rdports,
          wrports,
          None
        )

        System.err.println("Added memory", name)
        Seq((name -> memory))
      }
      case _ => Seq.empty
    }).toMap

  def convert(circuit : firrtl.ir.Circuit) : DigitalJs = convertWithOpts(circuit, true)
  def convertWithOpts(circuit : firrtl.ir.Circuit, io_transformed : Boolean) : DigitalJs = {
    memory_modules = findMemoryModules(circuit)
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

  // TO DO: Remove - becomes obsolete with new digitaljs changes 
  def transformIO : Device => Device = { case device =>
    device match {
      case digitaljs.Input(label, net, order, bits, is_clock, signed) =>
        if (is_clock)
          Clock(label)
        else if (bits == 1)
          Button(label)
        else
          NumEntry(label, bits, "hex")
      case digitaljs.Output(label, net, order, bits, signed) =>
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
        ( port.name
        , new Input(
            port.info.toString(),
            port.name, portNumber,
            bitWidth(port.tpe).intValue,
            isClockType(port.tpe),
            maybeIsSigned(port.tpe)
          )
        )
      case firrtl.ir.Output =>
        ( port.name
        , new Output(
            port.info.toString(),
            port.name,
            portNumber,
            bitWidth(port.tpe).intValue,
            maybeIsSigned(port.tpe)
          )
        )
    }
  }

  def convertPorts(ports : Seq[Port]) : Map[String, Device] =
    ports.zipWithIndex.map {
      case (p, i) => convertPort(p, i)
    }.toMap

  def convertModule(module: DefModule): (String, digitaljs.Circuit) = {
    val io_devs = convertPorts(module.ports)
    var devices: Map[String, Device] = ListMap.empty;
    var connectors: List[Connector] = Nil;
    module.foreachStmt((statement) => {
      val (d, c) = convertStatement(statement);
      devices = d ++ devices;
      connectors = c ++ connectors;
    })
    (module.name, new digitaljs.Circuit(module.name, io_devs ++ devices, connectors))
  }

  def getPlug(expr: Expression): Plug =
    expr match {
      case WRef(name, tpe, kind, flow) => new Plug(name, "in")
      case WSubField(WRef(name, tp1, kind, f1), port, tp0, f0) =>
        new Plug(name, port)
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

  def maybeIsSigned(tpe: Type) : Option[Boolean] = tpe match {
    case SIntType(width) => Some(true)
    case UIntType(width) => Some(false)
    case _ => None
  }

  def isSigned(tpe : Type) : Boolean = maybeIsSigned(tpe).get

  def convertPrimitive(
    op: PrimOp,
    args: Seq[Expression],
    consts: Seq[BigInt],
    tpe: Type,
    default_name: Option[String],
    label: String,
  ): (Map[String, Device], List[Connector], Plug) = {
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
            bitWidth(tpe).toInt, isSigned(lhs.tpe), isSigned(rhs.tpe)))
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
            isSigned(lhs.tpe), isSigned(rhs.tpe)))
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
        // TODO Test this branch
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
        // TODO Test this branch
        val arg = args(0);
        val const = consts(0);
        val const_width = const.toString(2).length;
        val const_name = generateIntermediateName(None);
        val name = generateIntermediateName(default_name);
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds +
          (const_name -> new Constant(label, makeConstantString(const, const_width))) +
          (name -> new Shift(
            shiftTypeOfPrimOp(op),
            label, 
            bitWidth(arg.tpe).toInt,
            const_width,
            bitWidth(tpe).toInt, isSigned(arg.tpe), false, isSigned(tpe), false))
        , new Connector(plug, new Plug(name, "in1")) ::
          new Connector(new Plug(const_name, "out"), new Plug(name, "in2")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Cvt => {
        // TODO Test this branch
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
          (name -> new Unary(Negation, label, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt, isSigned(arg.tpe)))
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
        val (lds, lcs, lhsPlug) = convertExpression(lhs, label, tpe);
        val (rds, rcs, rhsPlug) = convertExpression(rhs, label, tpe);
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
        val width = bitWidth(arg.tpe).toInt;
        val n = consts(0).toInt;
        val (ds, cs, plug) = convertExpression(arg, label);
        ( ds
        + (name -> new BusSlice(
            label,
            width - n,
            n,
            width
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

  def convertExpression(
    expr : Expression,
    toplevel : String,
    label : String,
    tpe : Type,
  ) : (Map[String, Device], List[Connector], Plug) = {
    val (ds, cs, plug) = convertExpression(expr, toplevel, label);
    val (eds, ecs, eplug) = maybeExtend(tpe, expr.tpe, plug);
    (eds ++ ds, ecs ++ cs, eplug)
  }

  def convertExpression(
    expr : Expression,
    label : String,
    tpe : Type,
  ) : (Map[String, Device], List[Connector], Plug) = {
    val (ds, cs, plug) = convertExpression(expr, label);
    val (eds, ecs, eplug) = maybeExtend(tpe, expr.tpe, plug);
    (eds ++ ds, ecs ++ cs, eplug)
  }

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
        val (tds, tcs, tvalPlug) = convertExpression(tval, label, tpe);
        val (fds, fcs, fvalPlug) = convertExpression(fval, label, tpe);
        ( (cds ++ tds ++ fds) + (toplevel -> new digitaljs.Mux(label, bitWidth(tpe).toInt, 1))
        , new Connector(fvalPlug, new Plug(toplevel, "in0")) ::
          new Connector(tvalPlug, new Plug(toplevel, "in1")) ::
          new Connector(condPlug, new Plug(toplevel, "sel")) ::
          ccs ++ tcs ++ fcs
        , new Plug(toplevel, "out")
        )
      }
      case SIntLiteral(value, width) =>
        ( ListMap(toplevel -> new Constant(label, makeConstantString(value, bitWidth(expr.tpe))))
        , Nil
        , new Plug(toplevel, "out") )
      case UIntLiteral(value, width) => 
        ( ListMap(toplevel -> new Constant(label, makeConstantString(value, bitWidth(expr.tpe))))
        , Nil
        , new Plug(toplevel, "out") )
      case ValidIf(cond, value, tpe) => convertExpression(value, label);
    }
  }

  def maybeExtend(sinkTpe : Type, sourceTpe : Type, sourcePlug : Plug) = {
    val sourceWidth = bitWidth(sourceTpe);
    val sinkWidth = bitWidth(sinkTpe);
    sourceTpe match {
      case UIntType(_) | SIntType(_) if sinkWidth > sourceWidth => {
        val name = generateIntermediateName(None);
        val dev = {
          if(isSigned(sourceTpe))
            new SignExtend("", sourceWidth.toInt, sinkWidth.toInt)
          else
            new ZeroExtend("", sourceWidth.toInt, sinkWidth.toInt)
        }

        ( ListMap((name -> dev))
        , List(new Connector(sourcePlug, new Plug(name, "in")))
        , new Plug(name, "out")
        )
      }
      case _ => (ListMap.empty, List.empty, sourcePlug)
    }
  }
  def convertStatement(
      statetment: Statement
  ): (Map[String, Device], List[Connector]) = {
    statetment match {
      case Block(stmts) =>
        stmts
          .map(convertStatement)
          .fold((ListMap[String, Device](), Nil))((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
      case Connect(info, loc, expr) =>
        val sinkPlug = getPlug(loc);
        val (ds, cs, sourcePlug) = convertExpression(expr, info.toString);
        val (extendedDs, extendedCs, extendedPlug) = maybeExtend(loc.tpe, expr.tpe, sourcePlug)
        ((ds ++ extendedDs), new Connector(extendedPlug, sinkPlug) :: (cs ++ extendedCs))
      case WDefInstance(info, name, module, tpe) => {
        if (memory_modules contains module)
          (ListMap(name -> memory_modules(module)), Nil)
        else 
          (ListMap(name -> new Subcircuit(name, module)), Nil)
      }
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
      case IsInvalid(info, expr) => {
        (Map.empty, List.empty)
      }
      case _ => println("Illegal statement: " + statetment.serialize); (ListMap(), Nil)
    }
  }
}

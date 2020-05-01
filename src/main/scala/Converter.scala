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
import scala.collection.immutable.ListMap
import scala.collection.immutable.Stream.cons

object Converter {
  var genId : BigInt = BigInt.int2bigInt(0);
  def convert(circuit : firrtl.ir.Circuit) : DigitalJs = convertFirrtl(circuit);

  def convertFirrtl(circuit : firrtl.ir.Circuit) : DigitalJs = {
    val circuits = circuit.modules map convertModule toMap 
    val maybeToplevel = circuits find {case (name, _) => name == circuit.main }
    maybeToplevel match {
      case Some((_, toplevel)) => 
        new DigitalJs(
          toplevel.devices mapValues transformIO, 
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
      case digitaljs.Input(label, net, order, bits) => NumEntry(label, bits, "hex")
      case digitaljs.Output(label, net, order, bits) => NumDisplay(label, bits, "hex")
      case _ => device
    }
  }

  def convertPort(port: Port, portNumber: Int): (String, Device) = {
    port.direction match {
      case firrtl.ir.Input =>
        (port.name, new Input(port.name, port.name, portNumber, bitWidth(port.tpe).intValue))
      case firrtl.ir.Output =>
        (port.name, new Output(
          port.name,
          port.name,
          portNumber,
          bitWidth(port.tpe).intValue
        ))
    }
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
    (module.name, new digitaljs.Circuit(module.name, io_devices ++ devices, connectors))
  }

  def getPlug(expr: Expression): Plug =
    expr match {
      case WRef(name, tpe, kind, flow) => new Plug(name, "in")
      case WSubField(WRef(name, tp1, kind, f1), port, tp0, f0) =>
        new Plug(name, port)
      case Reference(name, tpe) => new Plug(name, "in")
      case _ => println(expr); ???
    }

  def generateIntermediateName(): String = {
    genId += 1;
    "__INTERMEDIATE__" + genId.toString(16)
  }

    

  def binTypeOfPrimOp(op: PrimOp) : BinType =
    op match {
      case Add => Addition()
      case Sub => Subtraction()
      case Mul => Multiplication()
      case Div => Division()
      case Rem => Modulo()
      case Leq => Le()
      case firrtl.PrimOps.Lt => digitaljs.Lt()
      case Geq => Ge()
      case firrtl.PrimOps.Gt => digitaljs.Gt()
      case firrtl.PrimOps.Eq => digitaljs.Eq()
      case Neq => Ne()
      case Shr | Dshr => ShiftRight()
      case Shl | Dshl => ShiftLeft()
    }

  def convertPrimitive(
    op: PrimOp,
    args: Seq[Expression],
    consts: Seq[BigInt],
    tpe: Type
  ): (Map[String, Device], List[Connector], Plug) = 
    op match {
      case Add | Sub | Mul | Div | Rem |
        firrtl.PrimOps.Leq | firrtl.PrimOps.Lt | firrtl.PrimOps.Geq | 
        firrtl.PrimOps.Gt | firrtl.PrimOps.Eq | firrtl.PrimOps.Neq |
        Dshl | Dshr =>
        val name = generateIntermediateName();
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs);
        val (rds, rcs, rhsPlug) = convertExpression(rhs);
        ( (lds ++ rds)
        + (name -> new Binary(
            binTypeOfPrimOp(op), 
            name, 
            bitWidth(lhs.tpe).toInt, 
            bitWidth(rhs.tpe).toInt, 
            bitWidth(tpe).toInt, false, false))
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      case AsSInt | AsUInt | AsClock => {
        val arg = args(0);
        convertExpression(arg);
      }
      case Pad => {
        val name = generateIntermediateName();
        val arg = args(0);
        val padDevice = tpe match {
          case UIntType(_) => new ZeroExtend(name, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt)
          case SIntType(_) => new SignExtend(name, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt)
        }
        val (ds, cs, plug) = convertExpression(args(0))
        ( ds + (name -> padDevice)
        , new Connector(plug, new Plug(name, "in")) :: cs
        , new Plug(name, "out")
        )
      }
      case Shl | Shr => {
        val arg = args(0);
        val const = consts(0);
        val const_name = generateIntermediateName();
        val name = generateIntermediateName();
        val (ds, cs, plug) = convertExpression(arg);
        ( ds +
          (const_name -> new Constant(const_name, const.toString(2))) +
          (name -> new Binary(
            binTypeOfPrimOp(op),
            name, 
            bitWidth(arg.tpe).toInt,
            const.toString(2).length, 
            bitWidth(tpe).toInt, false, false))
        , new Connector(plug, new Plug(name, "in1")) ::
          new Connector(new Plug(const_name, "out"), new Plug(name, "in2")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Cvt => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg);
        val name = generateIntermediateName();
        ( ds +
          (name -> new ZeroExtend(name, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case Neg => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg);
        val name = generateIntermediateName();
        ( ds +
          (name -> new Unary(Negation(), name, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt, false))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case firrtl.PrimOps.Not => {
        val arg = args(0);
        val (ds, cs, plug) = convertExpression(arg);
        val name = generateIntermediateName();
        ( ds +
          (name -> new Unary(digitaljs.Not(), name, bitWidth(arg.tpe).toInt, bitWidth(tpe).toInt, false))
        , new Connector(plug, new Plug(name, "in")) ::
          cs
        , new Plug(name, "out")
        )
      }
      case And | Or | Xor => ???
      case Andr | Orr | Xorr => ???
      case Cat => ??? 
      case Bits => ???
      case Head => ???
      case Tail => ???
      case _ => println("Not handled ", op.toString()); (ListMap(), Nil, new Plug("XD", "out"));
    }

  def convertExpression(
      expr: Expression
  ): (Map[String, Device], List[Connector], Plug) = {
    val intermediateName = generateIntermediateName();
    convertExpression(expr, intermediateName);
  }

  def convertExpression(
      expr: Expression,
      toplevel: String
  ): (Map[String, Device], List[Connector], Plug) = {
    expr match {
      case DoPrim(op, args, consts, tpe) => convertPrimitive(op, args, consts, tpe);
      case FixedLiteral(value, width, point) => {
        ( ListMap(toplevel -> new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      }
      case WRef(name, tpe, kind, flow) => (ListMap(), Nil, new Plug(name, "out"))
      case firrtl.ir.Mux(cond, tval, fval, tpe) => {
        val (cds, ccs, condPlug) = convertExpression(cond);
        val (tds, tcs, tvalPlug) = convertExpression(tval);
        val (fds, fcs, fvalPlug) = convertExpression(fval);
        ( (cds ++ tds ++ fds) + (toplevel -> new digitaljs.Mux(toplevel, bitWidth(tpe).toInt, 1))
        , new Connector(tvalPlug, new Plug(toplevel, "in0")) ::
          new Connector(fvalPlug, new Plug(toplevel, "in1")) ::
          new Connector(condPlug, new Plug(toplevel, "sel")) ::
          ccs ++ tcs ++ fcs
        , new Plug(toplevel, "out")
        )
      }
      case Reference(name, tpe) => (ListMap(), Nil, new Plug(name, "out"))
      case SIntLiteral(value, width) =>
        ( ListMap(toplevel -> new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      case SubAccess(expr, index, tpe) => ???
      case SubField(expr, name, tpe) => ???
      case SubIndex(expr, value, tpe) => ???
      case UIntLiteral(value, width) => 
        ( ListMap(toplevel -> new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      case ValidIf(cond, value, tpe) => convertExpression(value);
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
        val (ds, cs, sourcePlug) = convertExpression(expr);
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
        println("Ignoring memory declaration"); (ListMap(), Nil)
      case DefNode(info, name, value) => val (ds, cs, _) = convertExpression(value, name);(ds, cs)
      case DefRegister(info, name, tpe, clock, reset, init) => {
        val x = generateIntermediateName();
        val (ds, cs, _) = convertExpression(init);
        return (
          ds + (x -> new Dff(x, bitWidth(tpe).toInt, new Polarity(true, true, false), "")),
          cs
        )
      }
      case DefWire(info, name, tpe) => (ListMap(), Nil)
      case IsInvalid(info, expr)    => (ListMap(), Nil)
      case Stop(info, ret, clk, en) => (ListMap(), Nil)
      case EmptyStmt                => (ListMap(), Nil)
      case Print(info, string, args, clk, en) =>
        println("Ignoring print statement"); (ListMap(), Nil)
      case _ => println("Illegal statement", statetment); (ListMap(), Nil)
    }
  }
}

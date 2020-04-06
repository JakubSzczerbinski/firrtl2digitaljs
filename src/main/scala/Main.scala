package firrtl2digitaljs

import firrtl.Parser.{parseFile}
import firrtl.Parser.IgnoreInfo
import firrtl.Parser.UseInfo
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
import scala.collection.immutable.Stream.cons
import firrtl.WRef
import firrtl.WSubField
import firrtl.WDefInstance

trait JsonSerializable {
  def toJson(): String
}

abstract class Device() extends JsonSerializable {
  def toJson(): String = "null"
}

case class Subcircuit(label: String, celltype: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "Subcircuit",
  "label": "$label",
  "celltype": "$celltype"
}"""
}

case class Not() extends Device {}

case class Input(label: String, net: String, order: Int, bits: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "Input",
  "label": "$label",
  "net": "$net",
  "order": $order,
  "bits": $bits
}"""
}

class Polarity(clock: Boolean, arst: Boolean, enable: Boolean) extends JsonSerializable {
  override def toJson(): String = s"""
{
  "clock": $clock,
  "arst": $arst,
  "enable": $enable
}"""
}

case class Dff(label: String, bits: Int, polarity: Polarity, initial: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "Dff",
  "label": "$label",
  "bits": $bits,
  "polarity": ${JsonHelpers.indent(polarity.toJson())},
  "initial": "$initial"
}"""
}

case class Mux(label: String, bits_in: Int, bits_sel: Int) extends Device
{
  override def toJson(): String = s"""
{
  "type": "Mux",
  "label": "$label",
  "bits": {
    "in": $bits_in,
    "sel": $bits_sel
  }
}"""
}

case class NumEntry(label: String, bits: Int, numbase: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "NumEntry",
  "label": "$label",
  "bits": $bits,
  "numbase": "$numbase"
}"""
}

case class NumDisplay(label: String, bits: Int, numbase: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "NumDisplay",
  "label": "$label",
  "bits": $bits,
  "numbase": "$numbase"
}"""
}

case class Constant(label: String, constant: String) extends Device
{
  override def toJson(): String = s"""
{
  "type": "Constant",
  "label": "$label",
  "constant": "$constant"
}"""
}

abstract class BinType extends JsonSerializable{}

case class Addition() extends BinType {
  override def toJson(): String = s""""Addition""""
}

case class Subtraction() extends BinType {
  override def toJson(): String = s""""Subtraction""""
}

case class Multiplication() extends BinType {
  override def toJson(): String = s""""Multiplication""""
}

case class Division() extends BinType {
  override def toJson(): String = s""""Division""""
}

case class Modulo() extends BinType {
  override def toJson(): String = s""""Modulo""""
}

case class Binary(tpe: BinType, label: String, bits_in1: Int, bits_in2: Int, bits_out: Int, 
  signed_in1: Boolean, signed_in2: Boolean) extends Device
{
  override def toJson(): String = s"""
{
  "type": ${tpe.toJson()},
  "label": "$label",
  "bits": {
    "in1": $bits_in1,
    "in2": $bits_in2,
    "out": $bits_out
  },
  "signed": {
    "in1": $signed_in1,
    "in2": $signed_in2
  }
}"""
}

case class Output(label: String, net: String, order: Int, bits: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "Output",
  "label": "$label",
  "net": "$net",
  "order": $order,
  "bits": "$bits"
}"""
}

class Plug(id: String, port: String) extends JsonSerializable {
  def toJson(): String = s"""
{
  "id": "$id",
  "port": "$port"
}"""

}

class Connector(from: Plug, to: Plug) extends JsonSerializable {
  def toJson(): String = s"""
{
  "to": ${JsonHelpers.indent(to.toJson())},
  "from": ${JsonHelpers.indent(from.toJson())}
}"""
}

object JsonHelpers {
  def serialize_list[T <: JsonSerializable](seq: Seq[T]): String =
    "[" + (seq map (_.toJson) map indent mkString ",") + (if (seq.size == 0) ""
                                                          else "\n") + "]"

  def indent(json: String): String =
    json split "\n" mkString "\n  "
}

class Circuit(val name: String, val devices: List[Device], val connectors: List[Connector]) extends JsonSerializable {
  def toJson(): String = s"""
{
  "name": "$name",
  "devices": ${JsonHelpers.indent(JsonHelpers.serialize_list(devices))},
  "connectors": ${JsonHelpers.indent(JsonHelpers.serialize_list(connectors))}
}"""
}

class DigitalJs(
  val devices: Seq[Device],
  val connectors: Seq[Connector],
  val subcircuits: Seq[Circuit]
) extends JsonSerializable {
  override def toJson(): String = s"""
{
  "devices": ${JsonHelpers.indent(JsonHelpers.serialize_list(devices))},
  "connectors": ${JsonHelpers.indent(JsonHelpers.serialize_list(connectors))},
  "subcircuits": ${JsonHelpers.indent(JsonHelpers.serialize_list(subcircuits))}
}
"""

}

object Firrtl2Digitaljs {
  var genId : BigInt = BigInt.int2bigInt(0);
  def convert(circuit : firrtl.ir.Circuit) : DigitalJs = convertFirrtl(circuit);

  def convertFirrtl(circuit : firrtl.ir.Circuit) : DigitalJs = {
    val circuits = circuit.modules map convertModule
    val maybeToplevel = circuits.find(_.name == circuit.main)
    maybeToplevel match {
      case Some(toplevel) => 
        new DigitalJs(
          toplevel.devices map transformIO, 
          toplevel.connectors, 
          circuits filter (_.name != circuit.main))
      case None => {
        println("No toplevel module")
        new DigitalJs(Nil, Nil, circuits)
      }
    }
  }

  def transformIO(device : Device) : Device =
    device match {
      case Input(label, net, order, bits) => NumEntry(label, bits, "hex")
      case Output(label, net, order, bits) => NumDisplay(label, bits, "hex")
      case _ => device
    }

  def convertPort(port: Port, portNumber: Int): Device = {
    port.direction match {
      case firrtl.ir.Input =>
        new Input(port.name, port.name, portNumber, bitWidth(port.tpe).intValue)
      case firrtl.ir.Output =>
        new Output(
          port.name,
          port.name,
          portNumber,
          bitWidth(port.tpe).intValue
        )
    }
  }

  def convertModule(module: DefModule): Circuit = {
    val io_devices = module.ports.zipWithIndex.map {
      case (p, i) => convertPort(p, i)
    }.toList;
    var devices: List[Device] = Nil;
    var connectors: List[Connector] = Nil;
    module.foreachStmt((statement) => {
      val (d, c) = convertStatement(statement);
      devices = d ++ devices;
      connectors = c ++ connectors;
    })
    new Circuit(module.name, io_devices ++ devices, connectors);
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
    }

  def convertPrimitive(
    op: PrimOp,
    args: Seq[Expression],
    consts: Seq[BigInt],
    tpe: Type
  ): (List[Device], List[Connector], Plug) = 
    op match {
      case Add | Sub | Mul | Div | Rem =>
        val name = generateIntermediateName();
        val lhs = args(0);
        val rhs = args(1);
        val (lds, lcs, lhsPlug) = convertExpression(lhs);
        val (rds, rcs, rhsPlug) = convertExpression(rhs);
        ( new Binary(
            binTypeOfPrimOp(op), 
            name, 
            bitWidth(lhs.tpe).toInt, 
            bitWidth(rhs.tpe).toInt, 
            bitWidth(tpe).toInt, false, false) :: 
          lds ++ rds
        , new Connector(lhsPlug, new Plug(name, "in1")) ::
          new Connector(rhsPlug, new Plug(name, "in2")) ::
          lcs ++ rcs
        , new Plug(name, "out")
        )
      case AsSInt | AsUInt => {
        val arg = args(0);
        convertExpression(arg);
      }
      case _ => println("Not handled ", op.toString()); (Nil, Nil, new Plug("XD", "out"));
    }

  def convertExpression(
      expr: Expression
  ): (List[Device], List[Connector], Plug) = {
    val intermediateName = generateIntermediateName();
    convertExpression(expr, intermediateName);
  }

  def convertExpression(
      expr: Expression,
      toplevel: String
  ): (List[Device], List[Connector], Plug) = {
    expr match {
      case DoPrim(op, args, consts, tpe) => convertPrimitive(op, args, consts, tpe);
      case FixedLiteral(value, width, point) => {
        ( List(new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      }
      case WRef(name, tpe, kind, flow) => (Nil, Nil, new Plug(name, "out"))
      case firrtl.ir.Mux(cond, tval, fval, tpe) => {
        val (cds, ccs, condPlug) = convertExpression(cond);
        val (tds, tcs, tvalPlug) = convertExpression(tval);
        val (fds, fcs, fvalPlug) = convertExpression(fval);
        ( new Mux(toplevel, bitWidth(tpe).toInt, 1) :: (cds ++ tds ++ fds)
        , new Connector(tvalPlug, new Plug(toplevel, "in0")) ::
          new Connector(fvalPlug, new Plug(toplevel, "in1")) ::
          new Connector(condPlug, new Plug(toplevel, "sel")) ::
          ccs ++ tcs ++ fcs
        , new Plug(toplevel, "out")
        )
      }
      case Reference(name, tpe) => (Nil, Nil, new Plug(name, "out"))
      case SIntLiteral(value, width) =>
        ( List(new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      case SubAccess(expr, index, tpe) => ???
      case SubField(expr, name, tpe) => ???
      case SubIndex(expr, value, tpe) => ???
      case UIntLiteral(value, width) => 
        ( List(new Constant(toplevel, value.toString(2)))
        , Nil
        , new Plug(toplevel, "out") )
      case ValidIf(cond, value, tpe) => convertExpression(value);
    }
  }

  def convertStatement(
      statetment: Statement
  ): (List[Device], List[Connector]) = {
    statetment match {
      case Block(stmts) =>
        return stmts
          .map(convertStatement)
          .fold((Nil, Nil))((a, b) => (a._1 ++ b._1, a._2 ++ b._2))
      case Connect(info, loc, expr) =>
        val sinkPlug = getPlug(loc);
        val (ds, cs, sourcePlug) = convertExpression(expr);
        (ds, new Connector(sourcePlug, sinkPlug) :: cs)
      case DefInstance(info, name, module) => (List(new Subcircuit(name, module)), Nil)
      case WDefInstance(info, name, module, tpe) => (List(new Subcircuit(name, module)), Nil)
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
        println("Ignoring memory declaration"); (Nil, Nil)
      case DefNode(info, name, value) => val (ds, cs, _) = convertExpression(value, name);(ds, cs)
      case DefRegister(info, name, tpe, clock, reset, init) => {
        val x = generateIntermediateName();
        val (ds, cs, _) = convertExpression(init);
        return (
          new Dff(x, bitWidth(tpe).toInt, new Polarity(true, true, false), "") :: ds,
          cs
        )
      }
      case DefWire(info, name, tpe) => (Nil, Nil)
      case IsInvalid(info, expr)    => (Nil, Nil)
      case Stop(info, ret, clk, en) => (Nil, Nil)
      case EmptyStmt                => (Nil, Nil)
      case Print(info, string, args, clk, en) =>
        println("Ignoring print statement"); (Nil, Nil)
      case _ => println("Illegal statement", statetment); (Nil, Nil)
    }
  }
}

object Main extends App {
  val circuit = parseFile("test.fir", UseInfo);
  val loweredCircuit = lowerFirrtl(circuit);
  val digitalJsCircuit = Firrtl2Digitaljs.convert(loweredCircuit);
  exportAsHtml(digitalJsCircuit, "./index.html")

  def printDigitaljs(circuit : DigitalJs) : Unit = {
    println(circuit.toJson())
  }

  def writeDigitalJS(circuit : DigitalJs, path : String) = {
    import java.io.PrintWriter
    new PrintWriter(path) { 
      write(circuit.toJson); close 
    }
  }

  def exportAsHtml(circuit : DigitalJs, path : String) = {
    val result = s"""
<!doctype html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html;charset=UTF-8" />
    <script type="text/javascript" src="main.js"></script>
    <title></title>
  </head>
  <body>
    <div id="paper"></div>
    <script>
      const circuit = new digitaljs.Circuit(${circuit.toJson});
      const paper = circuit.displayOn('#paper');
      circuit.start();
    </script>
  </body>
</html>
"""
    import java.io.PrintWriter
    new PrintWriter(path) { 
      write(result); close 
    }
  }

  def lowerFirrtl(circuit : firrtl.ir.Circuit) : firrtl.ir.Circuit = {
    val lowfirrtlC = new LowFirrtlCompiler()
    lowfirrtlC.compileAndEmit(CircuitState(circuit, ChirrtlForm)).circuit
  }

}

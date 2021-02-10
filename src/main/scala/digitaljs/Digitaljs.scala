package firrtl2digitaljs.digitaljs

import org.json4s.native.Json

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

case class Input(label: String, net: String, order: Int, bits: Int, is_clock: Boolean, signed : Option[Boolean] = None) extends Device {
  override def toJson(): String = s"""
{
  "type": "Input",
  "label": "$label",
  "net": "$net",
  "order": $order,
  "bits": $bits${
    signed match {
      case None => ""
      case Some(signed) => ",\n" +
       "  \"signed\":" + signed.toString()
    }
  }
}"""
}

class Polarity(clock: Option[Boolean], arst: Option[Boolean], enable: Option[Boolean]) extends JsonSerializable {
  override def toJson(): String = {
    val values = (
      clock.map(x => s"""  "clock": $x""").toList ++
      arst.map(x => s"""  "arst": $x""").toList ++
      enable.map(x => s"""  "enable": $x""").toList
    )
s"""{
  ${values.mkString(",\n")}
}"""
  }
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

case class Clock(label: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "Clock",
  "label": "$label"
}"""
}

case class Button(label: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "Button",
  "label": "$label"
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

case class Lamp(label: String) extends Device {
  override def toJson(): String = s"""
{
  "type": "Lamp",
  "label": "$label"
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
  override def toJson(): String = "Addition"
}

case class Subtraction() extends BinType {
  override def toJson(): String = "Subtraction"
}

case class Multiplication() extends BinType {
  override def toJson(): String = "Multiplication"
}

case class Division() extends BinType {
  override def toJson(): String = "Division"
}

case class Modulo() extends BinType {
  override def toJson(): String = "Modulo"
}

abstract class CompType extends JsonSerializable{}

case class Eq() extends CompType {
  override def toJson(): String = "Eq"
}

case class Ne() extends CompType {
  override def toJson(): String = "Ne"
}

case class Lt() extends CompType {
  override def toJson(): String = "Lt"
}

case class Le() extends CompType {
  override def toJson(): String = "Le"
}

case class Gt() extends CompType {
  override def toJson(): String = "Gt"
}

case class Ge() extends CompType {
  override def toJson(): String = "Ge"
}

abstract class ShiftType extends JsonSerializable{}

case class ShiftLeft() extends ShiftType {
  override def toJson(): String = "ShiftLeft"
}

case class ShiftRight() extends ShiftType {
  override def toJson(): String = "ShiftRight"
}

case class Comparision(tpe : CompType, label: String, bits_in1: Int, bits_in2: Int,
  signed_in1 : Boolean, signed_in2: Boolean) extends Device
{
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": {
    "in1": $bits_in1,
    "in2": $bits_in2
  },
  "signed": {
    "in1": $signed_in1,
    "in2": $signed_in2
  }
}"""
}

case class BinaryArith(tpe: BinType, label: String, bits_in1: Int, bits_in2: Int, bits_out: Int, 
  signed_in1: Boolean, signed_in2: Boolean) extends Device
{
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
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

case class Shift(tpe: ShiftType, label: String, bits_in1: Int, bits_in2: Int, bits_out: Int,
  signed_in1: Boolean, signed_in2: Boolean, signed_out: Boolean, fillx: Boolean) extends Device
{
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": {
    "in1": $bits_in1,
    "in2": $bits_in2,
    "out": $bits_out
  },
  "signed": {
    "in1": $signed_in1,
    "in2": $signed_in2,
    "out": $signed_out
  },
  "fillx": $fillx
}"""
}

abstract class UnaryType extends JsonSerializable{}

case object Negation extends UnaryType {
  override def toJson(): String = "Negation"
}

case object UnaryPlus extends UnaryType {
  override def toJson(): String = "UnaryPlus"
}

case class Unary(tpe: UnaryType, label: String, bits_in: Int, bits_out: Int, signed: Boolean) extends Device {
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": {
    "in": $bits_in,
    "out": $bits_out
  },
  "signed": $signed
}"""
}

abstract class UnaryGateType extends JsonSerializable{}

case object Not extends UnaryGateType {
  override def toJson(): String = "Not"
}

case object Repeater extends UnaryGateType {
  override def toJson(): String = "Repeater"
}

case class UnaryGate(tpe: UnaryGateType, label: String, bits: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": $bits
}"""
}

abstract class BinaryGateType extends JsonSerializable {}

case object And extends BinaryGateType {
  override def toJson(): String = "And"
}

case object Nand extends BinaryGateType {
  override def toJson(): String = "Nand"
}

case object Or extends BinaryGateType {
  override def toJson(): String = "Or"
}

case object Nor extends BinaryGateType {
  override def toJson(): String = "Nor"
}

case object Xor extends BinaryGateType {
  override def toJson(): String = "Xor"
}

case object Xnor extends BinaryGateType {
  override def toJson(): String = "Xnor"
}

case class BinaryGate (tpe: BinaryGateType, label: String, bits: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": $bits
}"""
}

abstract class ReduceGateType extends JsonSerializable {}


case object AndReduce extends ReduceGateType {
  override def toJson(): String = "AndReduce"
}

case object NandReduce extends ReduceGateType {
  override def toJson(): String = "NandReduce"
}

case object OrReduce extends ReduceGateType {
  override def toJson(): String = "OrReduce"
}

case object NorReduce extends ReduceGateType {
  override def toJson(): String = "NorReduce"
}

case object XorReduce extends ReduceGateType {
  override def toJson(): String = "XorReduce"
}

case object XnorReduce extends ReduceGateType {
  override def toJson(): String = "XnorReduce"
}

case class ReduceGate(tpe: ReduceGateType, label: String, bits: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "${tpe.toJson()}",
  "label": "$label",
  "bits": $bits
}"""
}

case class ZeroExtend(label: String, extend_input : Int, extend_output : Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "ZeroExtend",
  "label": "$label",
  "extend": {
    "input": $extend_input,
    "output": $extend_output
  }
}"""
}

case class SignExtend(label: String, extend_input : Int, extend_output : Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "SignExtend",
  "label": "$label",
  "extend": {
    "input": $extend_input,
    "output": $extend_output
  }
}"""
}

case class Output(label: String, net: String, order: Int, bits: Int, signed : Option[Boolean] = None) extends Device {
  override def toJson(): String = s"""
{
  "type": "Output",
  "label": "$label",
  "net": "$net",
  "order": $order,
  "bits": $bits${
    signed match {
      case None => ""
      case Some(signed) => ",\n" +
        "  \"signed\":" + signed.toString()
    }
  }
}"""
}

case class Group(bits : Int) extends JsonSerializable {
  override def toJson(): String = bits.toString
}

case class BusGroup(label: String, groups : Array[Group]) extends Device {
  override def toJson(): String = s"""
{
  "type": "BusGroup",
  "label": "$label",
  "groups": ${JsonHelpers.serialize_list(groups)}
}"""
}

case class BusUngroup(label: String, groups : Array[Group]) extends Device {
  override def toJson(): String = s"""
{
  "type": "BusUngroup",
  "label": "$label",
  "groups": ${JsonHelpers.serialize_list(groups)}
}"""
}

case class BusSlice(label: String, first: Int, count : Int, total: Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "BusSlice",
  "label": "$label",
  "slice": {
    "first": $first,
    "count": $count,
    "total": $total
  }
}"""
}

class ReadPort(
  enable_polarity: Boolean,
  clock_polarity: Option[Boolean],
  transparent: Boolean
) extends JsonSerializable {
  override def toJson(): String = s"""
{
  "enable_polarity": $enable_polarity,
  ${if (clock_polarity.isDefined) s""""clock_polarity": ${clock_polarity.get.toString},""" else ""}
  "transparent": $transparent
}"""
}

class WritePort(
  enable_polarity: Boolean,
  clock_polarity: Boolean
) extends JsonSerializable {
  override def toJson(): String = s"""
{
  "enable_polarity": $enable_polarity,
  "clock_polarity": $clock_polarity
}"""
}


case class Memory(
  label: String,
  bits: Int,
  abits: Int,
  words: Int,
  offset: Int,
  rdports: Seq[ReadPort],
  wrports: Seq[WritePort], 
  memdata: Option[Seq[String]]
) extends Device {
  override def toJson(): String = s"""
  {
    "type": "Memory",
    "label": "$label",
    "bits": $bits,
    "abits": $abits,
    "words": $words,
    "offset": $offset,
    "rdports": ${JsonHelpers.serialize_list(rdports)},
    "wrports": ${JsonHelpers.serialize_list(wrports)}
    ${memdata match {
      case Some(data) => ", \"memdata\": " + JsonHelpers.serialize_str_list(data)
      case None => ""
    }}
  }"""
}


class Plug(val id: String, val port: String) extends JsonSerializable {
  def toJson(): String = s"""
{
  "id": "$id",
  "port": "$port"
}"""

}

class Connector(val from: Plug, val to: Plug) extends JsonSerializable {
  def toJson(): String = s"""
{
  "to": ${JsonHelpers.indent(to.toJson())},
  "from": ${JsonHelpers.indent(from.toJson())},
  "name": "${from.id}__${from.port}"
}"""
}

object JsonHelpers {
  def serialize_str_list(seq: Seq[String]): String =
    "[" + (seq map ('"' + _ + '"') mkString ",") + "]"
  def serialize_list[T <: JsonSerializable](seq: Seq[T]): String =
    "[" + (seq map (_.toJson) map indent mkString ",") + (if (seq.size == 0) ""
                                                          else "\n") + "]"
  def serialize_map [T <: JsonSerializable](m: Map[String, T]) : String =
    "{" + indent("\n" + (m map {case (key, value) => s""""$key": ${value.toJson()}"""} mkString ",\n")) + "\n}"

  def indent(json: String): String =
    json split "\n" mkString "\n  "
}

class Circuit(val name: String, val devices: Map[String, Device], val connectors: List[Connector]) extends JsonSerializable {
  def toJson(): String = s"""
{
  "name": "$name",
  "devices": ${JsonHelpers.indent(JsonHelpers.serialize_map(devices))},
  "connectors": ${JsonHelpers.indent(JsonHelpers.serialize_list(connectors))}
}"""
}

class DigitalJs(
  val devices: Map[String, Device],
  val connectors: Seq[Connector],
  val subcircuits: Map[String, Circuit]
) extends JsonSerializable {
  override def toJson(): String = s"""
{
  "devices": ${JsonHelpers.indent(JsonHelpers.serialize_map(devices))},
  "connectors": ${JsonHelpers.indent(JsonHelpers.serialize_list(connectors))},
  "subcircuits": ${JsonHelpers.indent(JsonHelpers.serialize_map(subcircuits))}
}
"""

}



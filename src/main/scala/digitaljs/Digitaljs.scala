package firrtl2digitaljs.digitaljs

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

case class Eq() extends BinType {
  override def toJson(): String = "Eq"
}

case class Ne() extends BinType {
  override def toJson(): String = "Ne"
}

case class Lt() extends BinType {
  override def toJson(): String = "Lt"
}

case class Le() extends BinType {
  override def toJson(): String = "Le"
}

case class Gt() extends BinType {
  override def toJson(): String = "Gt"
}

case class Ge() extends BinType {
  override def toJson(): String = "Ge"
}

case class ShiftLeft() extends BinType {
  override def toJson(): String = "ShiftLeft"
}

case class ShiftRight() extends BinType {
  override def toJson(): String = "ShiftRight"
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

abstract class UnaryType extends JsonSerializable{}

case class Negation() extends UnaryType {
  override def toJson(): String = "Negation"
}

case class UnaryPlus() extends UnaryType {
  override def toJson(): String = "UnaryPlus"
}

case class Not() extends UnaryType {
  override def toJson(): String = "Not"
}

case class Repeater() extends UnaryType {
  override def toJson(): String = "Repeater"
}

case class Unary(tpe: UnaryType, label: String, bits_in: Int, bits_out: Int, signed: Boolean) extends Device {
  override def toJson(): String = s"""
{
  "type": ${tpe.toJson()},
  "label": "$label",
  "bits": {
    "in": $bits_in,
    "out": $bits_out
  },
  "signed": $signed
}"""
}

case class ZeroExtend(label: String, extend_input : Int, extend_output : Int) extends Device {
  override def toJson(): String = s"""
{
  "type": "ZeroExtend",
  "label": "$label",
  "extend": {
    "input": $extend_input,
    "output": $extend_output,
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
    "output": $extend_output,
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


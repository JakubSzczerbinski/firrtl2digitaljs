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
import firrtl.InstanceKind
import firrtl.Dshlw

class Converter {
  import ConveterUtils._
  var memory_modules : Map[String, Memory] = Map.empty;

  def findMemoryModules (circuit : firrtl.ir.Circuit) : Map[String, Memory] =
    (circuit.modules flatMap {
      case ExtModule(info, name, ports, defname, params) if defname == "DIGITALJS_MEMORY" => {
        var addrWidth = getIntParam(params, "ADDR_WIDTH").toInt
        var size = getIntParam(params, "SIZE").toInt
        var readers = getIntParam(params, "READERS").toInt
        var writers = getIntParam(params, "WRITERS").toInt
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
        Seq((name -> memory))
      }
      case _ => Seq.empty
    }).toMap

  def digitalJsOfCircuits(circuits : Map[String, digitaljs.Circuit], main_circuit : String) =
    circuits.get(main_circuit) match {
      case Some(toplevel) => 
        new DigitalJs(
          toplevel.devices,
          toplevel.connectors, 
          circuits filter { case (name, _) => name != main_circuit })
      case None => {
        println("No toplevel module")
        new DigitalJs(Map.empty, Nil, circuits)
      }
    }
  
  def convert(circuit : firrtl.ir.Circuit) : DigitalJs = {
    memory_modules = findMemoryModules(circuit)
    val circuits = (circuit.modules map (mod => {
      val modConv = new ModuleConverter(mod, memory_modules);
      modConv.convert()
    })).toMap;
    digitalJsOfCircuits(circuits, circuit.main)
  }

  def transformToplevelIO(djs : DigitalJs) : DigitalJs = {
    val devices = djs.devices mapValues {
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
      case device => device
    }
    new DigitalJs(devices, djs.connectors, djs.subcircuits);
  }

  def convertWithOpts(circuit : firrtl.ir.Circuit, transformIO : Boolean) : DigitalJs = {
    val djs = convert(circuit);
    if (transformIO)
      transformToplevelIO(djs)
    else
      djs
  }
}

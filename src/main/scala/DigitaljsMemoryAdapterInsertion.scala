package firrtl2digitaljs

import firrtl.passes.Pass
import firrtl.ir._
import firrtl._
import scala.collection._
import firrtl.stage.TransformManager
import firrtl.passes.InferTypes
import os.stat
import firrtl.passes.memlib.ReadPort
import chisel3.Bundle
import Chisel.UInt
import os.write
import firrtl2digitaljs.digitaljs.DigitalJs
import firrtl.PrimOps.And
import chisel3.Name
import firrtl.passes.LowerTypes
import firrtl.FirrtlProtos.Firrtl.Statement.Instance
import firrtl.passes.memlib.Source

object DigitaljsMemoryAdapterInsertion {
  def maskTypeOfType(tpe: Type) : Type = tpe match {
    case BundleType(fields) => BundleType(fields map
      (field => Field(field.name, field.flip, maskTypeOfType(tpe))))
    case UIntType(width) => UIntType(IntWidth(1))
    case SIntType(width) => UIntType(IntWidth(1))
    case VectorType(tpe, size) => VectorType(maskTypeOfType(tpe), size)
  }
}

class DigitalJsMemory(val moduleName : String, val addr_width : Int, val tpe : Type, val readers : Int, val writers: Int) {
  val signed : Boolean = tpe match {
    case UIntType(width) => false
    case SIntType(width) => true
  }
  val size : Int = bitWidth(tpe).toInt;
  def getInstance(name : String) : WDefInstance =
    WDefInstance(NoInfo, name, moduleName, UnknownType)

  def module() = {
    val ports : Seq[Port] =
      (0 to (readers - 1) flatMap readerPorts) ++
      (0 to (writers - 1) flatMap writerPorts)
    ExtModule(NoInfo, moduleName, ports, "DIGITALJS_MEMORY", Seq(
      IntParam("ADDR_WIDTH", addr_width),
      IntParam("SIZE", size),
      IntParam("READERS", readers),
      IntParam("WRITERS", writers)
    ))
  }

  def readerPorts (rindex : Int) : Seq[Port] = Seq(
    Port(NoInfo, s"rd${rindex}data", Output, tpe),
    Port(NoInfo, s"rd${rindex}addr", Input, UIntType(IntWidth(addr_width))),
    Port(NoInfo, s"rd${rindex}en", Input, UIntType(IntWidth(1))),
    Port(NoInfo, s"rd${rindex}clk", Input, ClockType)
  )

  def writerPorts (windex : Int) : Seq[Port] = Seq(
    Port(NoInfo, s"wr${windex}data", Input, tpe),
    Port(NoInfo, s"wr${windex}addr", Input, UIntType(IntWidth(addr_width))),
    Port(NoInfo, s"wr${windex}en", Input, UIntType(IntWidth(1))),
    Port(NoInfo, s"wr${windex}clk", Input, ClockType)
  )

  def readerConnections(instance_name : String) : ((String, Int)) => Seq[Statement] = {
    case (rname, rindex) =>
    Seq(
      Connect(NoInfo, 
        WSubField(WRef(rname, UnknownType, PortKind, SinkFlow), "data", UnknownType),
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SourceFlow), s"rd${rindex}data", UnknownType)
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"rd${rindex}addr", UnknownType),
        WSubField(WRef(rname, UnknownType, PortKind, SourceFlow), "addr", UnknownType)
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"rd${rindex}en", UnknownType),
        WSubField(WRef(rname, UnknownType, PortKind, SourceFlow), "en", UnknownType)
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"rd${rindex}clk", UnknownType),
        WSubField(WRef(rname, UnknownType, PortKind, SourceFlow), "clk", UnknownType)
      )
    )
  }

  def writerConnections(instance_name : String) : ((String, Int)) => Seq[Statement] = {
    case (wname, windex) =>
    Seq(
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"wr${windex}data", UnknownType),
        WSubField(WRef(wname, UnknownType, PortKind, SourceFlow), "data", UnknownType)
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"wr${windex}addr", UnknownType),
        WSubField(WRef(wname, UnknownType, PortKind, SourceFlow), "addr", UnknownType),
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"wr${windex}en", UnknownType), 
        DoPrim(And, Seq(
          WSubField(WRef(wname, UnknownType, PortKind, SourceFlow), "en", UnknownType),
          WSubField(WRef(wname, UnknownType, PortKind, SourceFlow), "mask", UnknownType),
        ), Seq(), UnknownType)
      ),
      Connect(NoInfo, 
        WSubField(WRef(instance_name, UnknownType, InstanceKind, SinkFlow), s"wr${windex}clk", UnknownType),
        WSubField(WRef(wname, UnknownType, PortKind, SourceFlow), "clk", UnknownType),
      )
    )
  }
}

class DigitaljsMemoryAdapterInsertion() extends Pass {
  import DigitaljsMemoryAdapterInsertion._

  var digitalJsMemories : Set[DigitalJsMemory] = Set.empty;
  var memoryAdapterModules : List[Module] = List.empty;

  override def name: String = "DigitaljsMemoryAdapterInsertion";

  override def prerequisites: Seq[TransformManager.TransformDependency] =
    firrtl.stage.Forms.LowForm

  override def invalidates(a: Transform): Boolean = a match {
    case InferTypes => true
    case LowerTypes => true
    case _ => false
  }

  def readerPort(tpe : Type, addressWidth : BigInt) (reader : String) : Port = {
    val readerTpe = new BundleType(Seq(
      Field("data", Flip, tpe),
      Field("addr", Default, UIntType(IntWidth(addressWidth))),
      Field("en", Default, UIntType(IntWidth(1))),
      Field("clk", Default, ClockType)
    ))
    Port(info = NoInfo, reader, Input, readerTpe)
  }

  def writerPort(tpe : Type, addressWidth : BigInt) (writer : String) : Port = {
    val maskTpe : Type = maskTypeOfType(tpe);
    val writerTpe = new BundleType(Seq(
      Field("data", Default, tpe),
      Field("mask", Default, maskTpe),
      Field("addr", Default, UIntType(IntWidth(addressWidth))),
      Field("en", Default, UIntType(IntWidth(1))),
      Field("clk", Default, ClockType)
    ))
    Port(info = NoInfo, writer, Input, writerTpe)
  } 

  def readWriterPort(tpe : Type, addressWidth : BigInt) (readWriter : String) : Port = {
    assert(false, "Not yet supported")
    val maskTpe : Type = maskTypeOfType(tpe);
    val readWriterTpe = new BundleType(Seq(
      Field("wmode", Default, UIntType(IntWidth(1))),
      Field("rdata", Flip, tpe),
      Field("wdata", Default, tpe),
      Field("wmask", Default, maskTpe),
      Field("addr", Default, UIntType(IntWidth(addressWidth))),
      Field("en", Default, UIntType(IntWidth(1))),
      Field("clk", Default, ClockType)
    ))
    Port(NoInfo, readWriter, Input, readWriterTpe)
  }

  def swapMemoryForAdapterE(e : Expression) : Expression = {
    e match {
      case WRef(memory, tpe, MemKind, flow) => WRef(memory, tpe, InstanceKind, flow)
      case _ => e mapExpr swapMemoryForAdapterE
    }
  }

  def swapMemoryForAdapterS(c : Circuit) (s : Statement) : Statement = {
    s match {
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
        ) => {
          // TODO simulate read and write latency
          // TODO support memories with complex dataType
          // TODO suppport read/write ports
          val nm = Namespace(c);

          val addrWidth = Math.ceil(Math.log(depth.toInt)/Math.log(2)).toInt
          val djsMemoryInstanceName = nm.newTemp;
          val djsMemory = new DigitalJsMemory(nm.newTemp, addrWidth, dataType, readers.length, writers.length)
          digitalJsMemories = digitalJsMemories + djsMemory;

          val ports : Seq[Port] =
            (readers map readerPort(dataType, addrWidth)) ++
            (writers map writerPort(dataType, addrWidth)) ++
            (readwriters map readWriterPort(dataType, addrWidth))
          val connections : Seq[Statement] = 
            (readers.zipWithIndex flatMap djsMemory.readerConnections(djsMemoryInstanceName)) ++
            (writers.zipWithIndex flatMap djsMemory.writerConnections(djsMemoryInstanceName))
          val body : Statement = Block(Seq(djsMemory.getInstance(djsMemoryInstanceName)) ++ connections)
          val moduleName = nm.newTemp;
          val instanceName = name;
          val memModule = Module(info, moduleName, ports, body)

          memoryAdapterModules = memModule :: memoryAdapterModules;
          WDefInstance(info, instanceName, moduleName, UnknownType)
        }
        case _ => s mapStmt swapMemoryForAdapterS(c) mapExpr swapMemoryForAdapterE
    }
  }

  def swapMemoryForAdapterM (c : Circuit) (m : DefModule) : DefModule = {
    m mapStmt swapMemoryForAdapterS(c)
  }

  override def run(c: Circuit): Circuit = {
    val modulesx = 
      (c.modules map swapMemoryForAdapterM(c)) ++ 
      memoryAdapterModules ++ 
      digitalJsMemories.map(_.module)
    Circuit(c.info, modulesx, c.main)
  }
}

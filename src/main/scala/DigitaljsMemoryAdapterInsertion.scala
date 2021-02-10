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
import Chisel.Bool
import firrtl.PrimOps.Not
import os.read

object DigitaljsMemoryAdapterInsertion {
  def maskTypeOfType(tpe: Type): Type = tpe match {
    case BundleType(fields) =>
      BundleType(
        fields map
          (field => Field(field.name, field.flip, maskTypeOfType(tpe)))
      )
    case UIntType(width)       => UIntType(IntWidth(1))
    case SIntType(width)       => UIntType(IntWidth(1))
    case VectorType(tpe, size) => VectorType(maskTypeOfType(tpe), size)
  }
}

class DigitalJsMemory(
    val moduleName: String,
    val addr_width: Int,
    val tpe: Type,
    val readers: Int,
    val writers: Int,
    val readwriters : Int,
    val writeLatency: Int,
    val readLatency: Int,
) {
  val transparent_read = if (readLatency == 0) 1 else 0
  val nm = Namespace();

  class MemPort(
      val delay: Int,
      val name: String,
      val in: Boolean,
      val rw: String
  ) {
    def abstractionPort(rwname: String): Expression = {
      val flow = if (in) SourceFlow else SinkFlow
      WSubField(WRef(rwname, UnknownType, PortKind, flow), name, UnknownType)
    }
    def memoryPort(instance_name: String, rwindex: Int): Expression = {
      val flow = if (in) SinkFlow else SourceFlow
      WSubField(
        WRef(instance_name, UnknownType, InstanceKind, SinkFlow),
        s"${rw}${rwindex}" + name,
        UnknownType
      ),
    }
  }

  case object RData extends MemPort(0, "data", false, "rd") {}
  case class RAddr(readLatency: Int)
      extends MemPort(readLatency, "addr", true, "rd") {}
  case class REn(readLatency: Int)
      extends MemPort(readLatency, "en", true, "rd") {}
  case object RClk extends MemPort(0, "clk", true, "rd") {}

  case class WData(writeLatency: Int)
      extends MemPort(writeLatency, "data", true, "wr") {}
  case class WAddr(writeLatency: Int)
      extends MemPort(writeLatency, "addr", true, "wr") {}
  case class WEn(writeLatency: Int)
      extends MemPort(writeLatency, "en", true, "wr") {
    override def abstractionPort(wname: String): Expression = {
      DoPrim(
        And,
        Seq(
          WSubField(
            WRef(wname, UnknownType, PortKind, SourceFlow),
            "en",
            UnknownType
          ),
          WSubField(
            WRef(wname, UnknownType, PortKind, SourceFlow),
            "mask",
            UnknownType
          )
        ),
        Seq(),
        UnknownType
      )
    }
  }
  case object WClk extends MemPort(0, "clk", true, "wr") {}

  case object WrRdata extends MemPort(0, "rdata", false, "rd") {
    override def memoryPort(instance_name: String, rwindex: Int): Expression = {
      val flow = if (in) SinkFlow else SourceFlow
      WSubField(
        WRef(instance_name, UnknownType, InstanceKind, SinkFlow),
        s"${rw}${rwindex}" + "data",
        UnknownType
      ),
    }
  }
  case object WrWdata extends MemPort(0, "wdata", true, "wr") {
    override def memoryPort(instance_name: String, rwindex: Int): Expression = {
      val flow = if (in) SinkFlow else SourceFlow
      WSubField(
        WRef(instance_name, UnknownType, InstanceKind, SinkFlow),
        s"${rw}${rwindex}" + "data",
        UnknownType
      ),
    }
  }

  case object WrRen extends MemPort(0, "en", true, "rd") {
    override def abstractionPort(rwname: String): Expression =
      DoPrim(
        And,
        Seq(
          WSubField(
            WRef(rwname, UnknownType, PortKind, SourceFlow),
            "en",
            UnknownType
          ),
          DoPrim(
            Not,
            Seq(
              WSubField(
                WRef(rwname, UnknownType, PortKind, SourceFlow),
                "wmode"
              )
            ),
            Seq(),
            UnknownType
          )
        ),
        Seq(),
        UnknownType
      )
  }
  case object WrWen extends MemPort(0, "en", true, "wr") {
    override def abstractionPort(rwname: String): Expression =
      DoPrim(
        And,
        Seq(
          WSubField(
            WRef(rwname, UnknownType, PortKind, SourceFlow),
            "en",
            UnknownType
          ),
          DoPrim(
            And,
            Seq(
              WSubField(
                WRef(rwname, UnknownType, PortKind, SourceFlow),
                "wmode"
              ),
              WSubField(
                WRef(rwname, UnknownType, PortKind, SourceFlow),
                "wmask"
              )
            ),
            Seq(),
            UnknownType
          )
        ),
        Seq(),
        UnknownType
      )
  }

  val signed: Boolean = tpe match {
    case UIntType(width) => false
    case SIntType(width) => true
  }
  val size: Int = bitWidth(tpe).toInt;
  def getInstance(name: String): WDefInstance =
    WDefInstance(NoInfo, name, moduleName, UnknownType)

  def module() = {
    val ports: Seq[Port] =
      (0 to (readers + readwriters - 1) flatMap readerPorts) ++
      (0 to (writers + readwriters - 1) flatMap writerPorts)
    ExtModule(
      NoInfo,
      moduleName,
      ports,
      "DIGITALJS_MEMORY",
      Seq(
        IntParam("ADDR_WIDTH", addr_width),
        IntParam("SIZE", size),
        IntParam("READERS", readers + readwriters),
        IntParam("WRITERS", writers + readwriters),
        IntParam("TRANSPARENT_READ", transparent_read)
      )
    )
  }

  def readerPorts(rindex: Int): Seq[Port] = Seq(
    Port(NoInfo, s"rd${rindex}data", Output, tpe),
    Port(NoInfo, s"rd${rindex}addr", Input, UIntType(IntWidth(addr_width))),
    Port(NoInfo, s"rd${rindex}en", Input, UIntType(IntWidth(1))),
    Port(NoInfo, s"rd${rindex}clk", Input, ClockType)
  )

  def writerPorts(windex: Int): Seq[Port] = Seq(
    Port(NoInfo, s"wr${windex}data", Input, tpe),
    Port(NoInfo, s"wr${windex}addr", Input, UIntType(IntWidth(addr_width))),
    Port(NoInfo, s"wr${windex}en", Input, UIntType(IntWidth(1))),
    Port(NoInfo, s"wr${windex}clk", Input, ClockType)
  )

  def delayed(expr: Expression, delay: Int): (Seq[Statement], Expression) =
    if (delay <= 0)
      (Seq(), expr)
    else {
      val (stmts, exps) = delayed(expr, delay - 1)
      val register_name = nm.newTemp;
      val reg = DefRegister(NoInfo, register_name, UnknownType, ???, ???, ???)
      val conn = Connect(
        NoInfo,
        WRef(register_name, UnknownType, RegKind, SinkFlow),
        expr
      )
      val expr_ = WRef(register_name, UnknownType, RegKind, SourceFlow)
      (Seq(reg, conn), expr_)
    }

  def conn(instance_name: String, rname: String, rindex: Int)(
      port_type: MemPort
  ): Seq[Statement] = {
    val (stmts, rhs, lhs) =
      if (port_type.in) {
        val (stmts, rhs) =
          delayed(port_type.abstractionPort(rname), port_type.delay)
        (stmts, rhs, port_type.memoryPort(instance_name, rindex))
      } else {
        val (stmts, rhs) =
          delayed(port_type.memoryPort(instance_name, rindex), port_type.delay)
        (stmts, rhs, port_type.abstractionPort(rname))
      }

    return stmts ++ Seq(
      Connect(NoInfo, lhs, rhs)
    )
  }

  val readDelay = if (readLatency == 0) 0 else readLatency - 1;
  val writeDelay = writeLatency - 1;

  def maybeClockPort = 
    if (transparent_read == 0)
      Seq(RClk)
    else
      Seq()

  def readerConnections(
      instance_name: String
  ): ((String, Int)) => Seq[Statement] = {
    case (rname, rindex) => {
      val ports = Seq(RData, RAddr(readDelay), REn(readDelay)) ++ maybeClockPort
      ports flatMap conn(instance_name, rname, rindex)
    }
  }

  def writerConnections(
      instance_name: String
  ): ((String, Int)) => Seq[Statement] = {
    case (wname, windex) => {
      val delay = writeLatency - 1;
      val ports = Seq(WData(writeDelay), WAddr(writeDelay), WEn(writeDelay), WClk)
      ports flatMap conn(instance_name, wname, windex)
    }
  }

  def readwriterConnections(
    instance_name: String
  ) : ((String, Int)) => Seq[Statement] = {
    case (rwname, rwindex) => {
      val windex = rwindex + writers;
      val rindex = rwindex + readers;
      val wports = Seq(
        WrWdata, 
        WAddr(writeDelay), 
        WrWen, 
        WClk)
      val rports = Seq(
        WrRdata,
        RAddr(readDelay),
        WrRen) ++ maybeClockPort
      (wports flatMap conn(instance_name, rwname, windex)) ++
      (rports flatMap conn(instance_name, rwname, rindex))
    }
  }
}

class DigitaljsMemoryAdapterInsertion() extends Pass {
  import DigitaljsMemoryAdapterInsertion._

  var digitalJsMemories: Set[DigitalJsMemory] = Set.empty;
  var memoryAdapterModules: List[Module] = List.empty;

  override def name: String = "DigitaljsMemoryAdapterInsertion";

  override def prerequisites: Seq[TransformManager.TransformDependency] =
    firrtl.stage.Forms.LowForm

  override def invalidates(a: Transform): Boolean = a match {
    case InferTypes => true
    case LowerTypes => true
    case _          => false
  }

  def readerPort(tpe: Type, addressWidth: BigInt)(reader: String): Port = {
    val readerTpe = new BundleType(
      Seq(
        Field("data", Flip, tpe),
        Field("addr", Default, UIntType(IntWidth(addressWidth))),
        Field("en", Default, UIntType(IntWidth(1))),
        Field("clk", Default, ClockType)
      )
    )
    Port(info = NoInfo, reader, Input, readerTpe)
  }

  def writerPort(tpe: Type, addressWidth: BigInt)(writer: String): Port = {
    val maskTpe: Type = maskTypeOfType(tpe);
    val writerTpe = new BundleType(
      Seq(
        Field("data", Default, tpe),
        Field("mask", Default, maskTpe),
        Field("addr", Default, UIntType(IntWidth(addressWidth))),
        Field("en", Default, UIntType(IntWidth(1))),
        Field("clk", Default, ClockType)
      )
    )
    Port(info = NoInfo, writer, Input, writerTpe)
  }

  def readWriterPort(tpe: Type, addressWidth: BigInt)(
      readWriter: String
  ): Port = {
    val maskTpe: Type = maskTypeOfType(tpe);
    val readWriterTpe = new BundleType(
      Seq(
        Field("wmode", Default, UIntType(IntWidth(1))),
        Field("rdata", Flip, tpe),
        Field("wdata", Default, tpe),
        Field("wmask", Default, maskTpe),
        Field("addr", Default, UIntType(IntWidth(addressWidth))),
        Field("en", Default, UIntType(IntWidth(1))),
        Field("clk", Default, ClockType)
      )
    )
    Port(NoInfo, readWriter, Input, readWriterTpe)
  }

  def swapMemoryForAdapterE(e: Expression): Expression = {
    e match {
      case WRef(memory, tpe, MemKind, flow) =>
        WRef(memory, tpe, InstanceKind, flow)
      case _ => e mapExpr swapMemoryForAdapterE
    }
  }

  def swapMemoryForAdapterS(c: Circuit)(s: Statement): Statement = {
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
        // TODO suppport read/write ports
        val nm = Namespace(c);

        val addrWidth = Math.ceil(Math.log(depth.toInt) / Math.log(2)).toInt
        val djsMemoryInstanceName = nm.newTemp;
        val djsMemory = new DigitalJsMemory(
          nm.newTemp,
          addrWidth,
          dataType,
          readers.length,
          writers.length,
          readwriters.length,
          writeLatency,
          readLatency,
        )
        digitalJsMemories = digitalJsMemories + djsMemory;

        val ports: Seq[Port] =
          (readers map readerPort(dataType, addrWidth)) ++
            (writers map writerPort(dataType, addrWidth)) ++
            (readwriters map readWriterPort(dataType, addrWidth))
        val connections: Seq[Statement] =
          (readers.zipWithIndex flatMap djsMemory.readerConnections(
            djsMemoryInstanceName
          )) ++
          (writers.zipWithIndex flatMap djsMemory.writerConnections(
            djsMemoryInstanceName
          )) ++
          (readwriters.zipWithIndex flatMap djsMemory.readwriterConnections(
            djsMemoryInstanceName
          ))
        val body: Statement = Block(
          Seq(djsMemory.getInstance(djsMemoryInstanceName)) ++ connections
        )
        val moduleName = nm.newTemp;
        val instanceName = name;
        val memModule = Module(info, moduleName, ports, body)

        memoryAdapterModules = memModule :: memoryAdapterModules;
        WDefInstance(info, instanceName, moduleName, UnknownType)
      }
      case _ => s mapStmt swapMemoryForAdapterS(c) mapExpr swapMemoryForAdapterE
    }
  }

  def swapMemoryForAdapterM(c: Circuit)(m: DefModule): DefModule = {
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

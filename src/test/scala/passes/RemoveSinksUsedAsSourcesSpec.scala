package passes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import firrtl2digitaljs.TestUtils
import firrtl2digitaljs.RemoveSinksUsedAsSources
import firrtl.options.Dependency
import firrtl.CircuitState
import firrtl.ir.Circuit
import firrtl.ir.DefModule
import firrtl.ir.Statement
import firrtl.ir.Connect

//scalastyle:off magic.number
class RemoveSinksUsedAsSourcesSpec extends AnyFlatSpec with Matchers {

    def transform(s : String) : Circuit = {
        val cc = TestUtils.parse(s);
        val compiler = new firrtl.stage.transforms.Compiler(
            targets = Seq(Dependency[RemoveSinksUsedAsSources])
        )
        compiler.execute(CircuitState(cc, Seq.empty)).circuit
    }

    def getConnectsS(statement : Statement) : Seq[(String, String)] = statement match
    {
        case Connect(info, loc, expr) => Seq((loc.serialize, expr.serialize))
        case _: Statement => {
            var connects : Seq[(String, String)] = Seq.empty
            statement foreachStmt (stmt => {
                val conns = getConnectsS(stmt);
                connects = conns ++ connects;
            })
            connects
        }
    }

    def getConnects(module : DefModule) : Seq[(String, String)] = {
        var result : Seq[(String, String)] = Seq.empty;
        module foreachStmt (stmt => {
            result = getConnectsS(stmt) ++ result;
        })
        result
    }

    def moduleOfCircuit(c : Circuit, name : String) : DefModule = {
        var maybeCirc : Option[DefModule] = None;
        c foreachModule (module => {
            if (module.name == name)
                maybeCirc = Some(module)
        })
        maybeCirc get
    }

    val clone_circuit = """
    |circuit clone :
    |  module clone :
    |    input clock : Clock
    |    input io_in : UInt<1> 
    |    output io_out0 : UInt<1> 
    |    output io_out1 : UInt<1>
    |    output io_out2 : UInt<1>
    | 
    |    inst s1 of sub;
    |    s1.in <= io_in;
    |    io_out0 <= io_in;
    |    io_out1 <= io_out0;
    |    io_out2 <= s1.in;
    |  module sub :
    |    input in : UInt<1>
    |
    """.stripMargin;


    "Clone circuit" should "have sinks swapped for sources" in {
        val circuit = transform(clone_circuit);
        val cloneModule = moduleOfCircuit(circuit, "clone");
        val connects : Seq[(String, String)] = getConnects(cloneModule)
        connects should contain theSameElementsAs Seq(
            ("io_out0", "io_in"),
            ("io_out1", "io_in"),
            ("io_out2", "io_in"),
            ("s1.in", "io_in"),
        )
    }

    val multiple_source_connects = """
    |circuit mult :
    |  module mult :
    |    input io_in1 : UInt<1>
    |    input io_in2 : UInt<1>
    |    output io_out0 : UInt<1>
    |    output io_out1 : UInt<1>
    |    output io_out2 : UInt<1>
    |    output io_out3 : UInt<1>
    |    output io_out4 : UInt<1>
    |
    |    io_out0 <= io_in1;
    |    io_out1 <= io_out0;
    |    io_out2 <= io_out1;
    |    io_out3 <= io_out2;
    |    io_out4 <= io_in2;
    |
    """.stripMargin;

    "Multiple source connects circuit" should "have sinks swapped for sources" in {
        val circuit = transform(multiple_source_connects);
        val multModule = moduleOfCircuit(circuit, "mult");
        val connects = getConnects(multModule);

        connects should contain theSameElementsAs Seq(
            ("io_out0", "io_in1"),
            ("io_out1", "io_in1"),
            ("io_out2", "io_in1"),
            ("io_out3", "io_in1"),
            ("io_out4", "io_in2")
        )
    }
}

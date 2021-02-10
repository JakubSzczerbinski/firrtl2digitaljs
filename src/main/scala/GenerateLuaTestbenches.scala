package firrtl2digitaljs

import firrtl.passes.Pass
import firrtl.ir._
import os.stat
import firrtl_interpreter.Script
import firrtl.WRef
import firrtl.WSubField

object ScriptUtils {
    type Path = Seq[String]
    def luaArgsFromPath(path : Path) = {
        path map ('"' + _ + '"') mkString ", ";
    }

    def pathOfExpression(exp : Expression) : Path = exp match {
        case WRef(name, _, _, _) => Seq(name + "__out")
        case WSubField(WRef(name, _, _, _), name_sub, _, _) => Seq(name + "__" + name_sub)
        case exp => Seq()
    }
}

abstract class LuaScript {
    def extendPath(path : ScriptUtils.Path) : LuaScript
    def contents() : String
}

object PrintfScript {
    def apply(string : StringLit, args : Seq[Expression], clk : Expression, en : Expression) : PrintfScript = {
        PrintfScript(
            string,
            args map ScriptUtils.pathOfExpression,
            ScriptUtils.pathOfExpression(clk),
            ScriptUtils.pathOfExpression(en)
        )
    }
}

case class PrintfScript(string : StringLit, args : Seq[ScriptUtils.Path], clk : ScriptUtils.Path, en : ScriptUtils.Path) extends LuaScript {
    override def extendPath(path : ScriptUtils.Path) : PrintfScript = {
        PrintfScript(
            string,
            args map (arg => path ++ arg),
            path ++ clk,
            path ++ en
        )
    }
    override def contents(): String = 
    s"""
    |while true do
    |    sim.wait(sim.posedge(${ScriptUtils.luaArgsFromPath(clk)}));
    |    if sim.getvalue(${ScriptUtils.luaArgsFromPath(en)}) then
    |        print("PRINTF")
    |    end
    |end
    """.stripMargin
}

object StopScript {
    def apply(ret : Int, clk : Expression, en : Expression) = {
        new StopScript(ret, Seq.empty, Seq.empty)
    }
}

case class StopScript(ret : Int, clk : ScriptUtils.Path, en : ScriptUtils.Path) extends LuaScript {
    override def extendPath(path: ScriptUtils.Path): LuaScript = ???
    override def contents(): String =
    s"""
    |while true do
    |    sim.wait(sim.posedge(${ScriptUtils.luaArgsFromPath(clk)}))
    |    if sim.getvalue(${ScriptUtils.luaArgsFromPath(en)}) then
    |        print("STOP")
    |    end
    |end
    """.stripMargin
}

class GenerateLuaTestbenches() {
    type Path = ScriptUtils.Path
    var moduleScripts : Map[String, Seq[LuaScript]] = Map.empty
    var moduleInstances : Map[String, Seq[DefInstance]] = Map.empty

    def collectScripts(statement : Statement) : Seq[LuaScript] = {
        var scripts : Seq[LuaScript] = Seq.empty;
        statement foreachStmt {
            case Print(i, string, args, clk, en) =>
                scripts = PrintfScript(string, args, clk, en) +: scripts
            case Stop(info, ret, clk, en) =>
                scripts = StopScript(ret, clk, en) +: scripts
            case stmt =>
                scripts = scripts ++ collectScripts(stmt)
        }
        return scripts;
    }

    def collectDependencies(statement : Statement) : Seq[DefInstance] = {
        var dependencies : Seq[DefInstance] = Seq.empty;
        statement foreachStmt {
            case stmt : DefInstance =>
                dependencies = stmt +: dependencies;
            case stmt =>
                dependencies = dependencies ++ collectDependencies(stmt)
        }
        return dependencies;
    }

    def processStmt(mod : DefModule) (statement : Statement) : Statement =
        (statement mapStmt processStmt(mod)) mapStmt {
            case stmt : Print =>
                EmptyStmt
            case stmt : Stop =>
                EmptyStmt
            case stmt => stmt
        }

    def processModule(mod : DefModule) : DefModule = {
        val (scripts, dependencies) = mod match {
            case Module(info, name, ports, body) => 
                (collectScripts(body), collectDependencies(body))
            case _ => (Seq.empty, Seq.empty)
        }
        moduleScripts = moduleScripts + (mod.name -> scripts)
        moduleInstances = moduleInstances + (mod.name -> dependencies)
        mod mapStmt processStmt(mod)
    }

    def computeInstancePaths(module_name : String) : Seq[(Path, DefInstance)] = {
        val extendPath : (Path, DefInstance) => (Path, DefInstance) = {
            case (path, mod) => (module_name +: path, mod)
        }
        val instances : Seq[DefInstance] = moduleInstances(module_name);
        instances.flatMap({
            case DefInstance(i, name, module) =>
                val childInstnacePaths = computeInstancePaths(module) map {
                    case (path, mod) => (module_name +: path, mod)
                }
                (Seq.empty, DefInstance(i, name, module)) +: childInstnacePaths
            case _ =>
                Seq.empty
        })
    }

    def makeToplevelScripts(module_name : String) : Seq[LuaScript] = {
        val instancePaths = computeInstancePaths(module_name);
        val descendantScripts = instancePaths flatMap {
            case (path, DefInstance(_, _, mod)) => 
                moduleScripts(mod) map (script => script.extendPath(path))
        }
        descendantScripts ++ moduleScripts(module_name)
    }

    def run(c: Circuit): (Circuit, Seq[LuaScript]) = {
        val circ = c mapModule processModule
        val scripts = makeToplevelScripts(c.main)
        (circ, scripts)
    }
}
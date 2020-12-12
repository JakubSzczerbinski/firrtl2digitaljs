package firrtl2digitaljs

import firrtl.passes.Pass
import firrtl.ir._
import firrtl._
import scala.collection._
import firrtl.stage.TransformManager
import firrtl.passes.InferTypes
import os.stat

class RemoveSinksUsedAsSources() extends Pass {
    override def name = "RemoveSinksUsedAsSources"

    // Tune the prerequisites
    override def prerequisites: Seq[TransformManager.TransformDependency] =
        firrtl.stage.Forms.LowForm

    override def invalidates(a: Transform): Boolean = a match {
        case firrtl.passes.CommonSubexpressionElimination => true
        case _ => false
    }

    override def run(c: Circuit): Circuit = {
        c mapModule swapSinksforSources_M
    }

    type SinksToSources = mutable.Map[Expression, Expression]
    var instances : mutable.Set[String] = mutable.Set();
    var outputPorts : mutable.Set[String] = mutable.Set();
    var sourcesOfSinks : SinksToSources = mutable.Map()

    def findInstances_S(stmt : Statement) : Unit = 
        stmt match {
            case WDefInstance(info, name, module, tpe) => instances.add(name)
            case stmt => stmt foreachStmt findInstances_S
        }

    def findInstances_M(module : DefModule) : Unit = {
        instances = mutable.Set();
        module foreachStmt findInstances_S
    }

    def flipFlow_F(f : Flow) = 
        f match {
            case SinkFlow => SourceFlow
            case SourceFlow => SinkFlow
        }

    def flipFlow_E(expr : Expression) : Expression = 
        expr match {
            case WRef(name, tpe, kind, flow) =>
                WRef(name, tpe, kind, flipFlow_F(flow))
            case WSubField(expr, name, tpe, flow) => 
                WSubField(flipFlow_E(expr), name, tpe, flipFlow_F(flow))
        }

    def findOutputPorts(port : Port) : Unit =
        port.direction match {
            case Output => outputPorts.add(port.name)
            case _ => ()
        }

    def findSourcesOfSinks_S(statement : Statement) : Unit = 
        statement match {
            case Connect(info, loc, expr) => 
                loc match {
                    case WRef(name, _, _, _) if outputPorts contains name =>
                        sourcesOfSinks.put(flipFlow_E(loc), expr);
                    case WSubField(WRef(name, _, _, _), _, _, _) if instances contains name =>
                        sourcesOfSinks.put(flipFlow_E(loc), expr);
                    case _ => ()
                }
            case stmt => stmt foreachStmt findSourcesOfSinks_S
        }

    def findSourcesOfSinks_M(m : DefModule) : Unit = {
        sourcesOfSinks = mutable.Map();
        m foreachStmt findSourcesOfSinks_S
        sourcesOfSinks = sourcesOfSinks map {
            case (sink, source) => {
                var src = source;
                while (sourcesOfSinks contains src) {
                    src = sourcesOfSinks(src);
                }
                (sink, src)
            }
        }
    }

    def swapSinksforSources_E (expr : Expression) : Expression = {
        if (sourcesOfSinks contains expr)
            sourcesOfSinks(expr)
        else
            expr mapExpr swapSinksforSources_E
    }

    def swapSinksforSources_S (stmt : Statement) : Statement = 
        stmt match {
            case Connect(info, loc, expr) =>
                Connect(
                    info,
                    loc,
                    swapSinksforSources_E(expr)
                )
            case stmt => 
                stmt.mapStmt(swapSinksforSources_S)
                    .mapExpr(swapSinksforSources_E)
        }

    def swapSinksforSources_M (m : DefModule) : DefModule = {
        m foreachPort findOutputPorts;
        findInstances_M(m);
        findSourcesOfSinks_M(m);
        m mapStmt swapSinksforSources_S
    }

}
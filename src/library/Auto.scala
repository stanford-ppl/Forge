package ppl.dsl.forge
package library

import core.{ForgeApplication,ForgeApplicationRunner}
import templates.Utilities.nl

import scala.reflect.runtime.{universe => ru}

/**
 * This file enables auto-lifting classes via reflection.
 */
trait AutoOps {
  this: ForgeApplication =>
  
  def importAuto[T:ru.TypeTag] = {
    // TODO

    def ftpe(tp: ru.Type) = tpe(tp.typeSymbol.name.toString) // package names?

    val tt = ru.typeTag[T]
    val name = tt.tpe.typeSymbol.name.toString

    val Tpe = ftpe(tt.tpe)

    val Grp = grp(name)
    lift (Grp) (Tpe)

    //val Ops = withTpe(Grp)

    for (m <- tt.tpe.members if m.isMethod) {

        println(m + ": takesTypeArgs " + m.asTerm.typeSignature.takesTypeArgs)

        val mt0 = m.asTerm.typeSignature
        val ispoly = mt0.takesTypeArgs

        val mt = if (!ispoly) mt0.asInstanceOf[ru.MethodType] 
                  else mt0.asInstanceOf[ru.PolyType].resultType.asInstanceOf[ru.MethodType]

        val args = for (param <- mt.params) yield {
            val tp = param.typeSignature
            ftpe(tp)
        }

        val ret = ftpe(mt.resultType)

        infix (Grp) (m.name.toString, Nil, ((Tpe :: args)) :: ret, Nil, simple) implements 
            (codegen($cala, quotedArg(0) + "." + m.name))

        // overloading?? --> duplicate codegen warning

    }

  }
    
}

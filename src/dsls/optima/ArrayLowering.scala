package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait ArrayLowering { this: OptiMADSL =>

  def importArrayLowering() {
    val ArrayLowering = transformer("ArrayLowering")
    val ArrayND = lookupTpe("ArrayND")
    val Array1D = lookupTpe("Array1D")
    val Array2D = lookupTpe("Array2D")
    val Array3D = lookupTpe("Array3D")

    val LoweringRules = withTransformer(ArrayLowering)
    LoweringRules {
      /*lower (ArrayND, "ma_new") using rule ${ rank($0) match {
        case 1 =>
        case 1 =>
        case 2 =>
        case _ =>
      }}

      lower (ArrayND, "size") using rule ${

      }
      lower (ArrayND, "dim") using rule ${

      }
      lower (ArrayND, "ma_apply") using rule ${

      }*/

      // TODO: Referring to bound args should have simpler syntax than this
      val arg2_0 = "f_"+quotedArg(2)+"___arg0"

      lower (ArrayND, "ma_mkstring") using rule ${

        def stringify(elem: Rep[T]): Rep[String] = {
          val mirroredBody = withSubstScope(\$arg2_0 -> elem){ f($2) }
          getBlockResultFull(mirroredBody)
        }

        maimpl_mkstring($0, $1, {e: Rep[T] => stringify(e)})

        // TODO: Inlining blocks should have simpler syntax than this, should just expand to something like this
        // ideally:
        // ma_impl_mkstring($0, $1, {e: Rep[T] => inline[2](e) })
      }
    }

  }
}
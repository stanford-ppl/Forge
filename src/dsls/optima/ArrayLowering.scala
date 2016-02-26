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
      lower (ArrayND, "ma_new") using rule ${ layout(lhs) match {
        case l@MLayout(_,Flat,_) => maflat_new[T]($0, l)
      }}
      lower (ArrayND, "ma_new_immutable") using rule ${ layout(lhs) match {
        case l@MLayout(_,Flat,_) => maflat_new_immutable[T]($0, l)
      }}
      lower (ArrayND, "ma_view") using rule ${ layout(lhs) match {
        case l@MLayout(_,Flat,_) => maflat_view[T]($0, $1, $2, l)
      }}

      lower (ArrayND, "size") using rule ${ maimpl_size($0) }
      lower (ArrayND, "dim") using rule ${ maimpl_dim($0, $1) }
      lower (ArrayND, "ma_apply") using rule ${ maimpl_apply($0, $1) }

      lower (ArrayND, "ma_update") using rule ${ layout($0) match {
        case MLayout(_,Flat,_) => maflat_update($0, $1, $2)
      }}

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
        // ma_impl_mkstring($0, $1, {e: Rep[T] => $inline[2](e) })
      }
    }

  }
}
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
      /*lower (ArrayND, "multia_new") using rule ${ rank($0) match {
        case 1 =>
        case 1 =>
        case 2 =>
        case _ =>
      }}

      lower (ArrayND, "multia_size") using rule ${

      }
      lower (ArrayND, "multia_dim") using rule ${

      }
      lower (ArrayND, "multia_apply") using rule ${

      }*/
      lower (ArrayND, "multia_as_1d") using rule ${ $0 }
      lower (ArrayND, "multia_as_2d") using rule ${ $0 }
      lower (ArrayND, "multia_as_3d") using rule ${ $0 }
    }

  }
}
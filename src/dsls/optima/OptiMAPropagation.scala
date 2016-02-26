package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait OptiMAPropagation{ this: OptiMADSL =>

  def importOptiMAProps() {
    val ArrayND = lookupTpe("ArrayND")

    propagate (ArrayND, "ma_view") using rule ${ setChild(lhs, getChild($0)) }
    propagate (ArrayND, "ma_apply") using rule ${ setProps(lhs, getChild($self)) }
    propagate (ArrayND, "ma_update") using rule ${
      setChild($self, meet(UpdateAlias, getChild($self), getProps($2)))
    }
    propagate (ArrayND, "ma_mkstring") using rule ${
      //setChild($)
    }
  }

}
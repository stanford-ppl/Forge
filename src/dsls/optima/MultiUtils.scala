package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait MultiUtils { this: OptiMADSL =>

  def importMultiUtils() {
    val T = tpePar("T")

    // --- Utils
    // TODO: Move to a different file
    val Utils = grp("Utils")
    //internal (Utils) ("list_zeros", T, SInt :: SList(MInt)) implements composite ${ List.fill($0)(unit(0)) }
    internal (Utils) ("reductionTree", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    internal (Utils) ("productTree", Nil, SList(MInt) :: MInt) implements composite ${
      reductionTree($0, {(a: Rep[Int],b: Rep[Int]) => a * b}).head
    }
    internal (Utils) ("dimsToStrides", Nil, SList(MInt) :: SList(MInt)) implements composite ${
      List.tabulate($0.length){d =>
        if (d == $0.length - 1) unit(1)
        else productTree($0.drop(d + 1))
      }
    }
    internal (Utils) ("flattenIndices", Nil, (("indices", SList(MInt)), ("ofs", MInt), ("stride", SList(MInt))) :: MInt) implements composite ${
      List.tabulate($indices.length){i => $indices(i)*$stride(i) }.reduce{_+_} + $ofs
    }
  }
}
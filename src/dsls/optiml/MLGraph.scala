package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait MLGraphOps {
  this: OptiMLDSL =>

  def importAllGraphOps() {
    importCSROps() 
  }

  def importCSROps() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val CSRGraph = tpe("CSRGraph")
    val CSRNgbr = tpe("CSRNgbr")

    data(CSRGraph, ("_numNodes", MInt), ("_numEdges", MInt), ("_nodes", MArray(MInt)), ("_edges", MArray(MInt)))

    static (CSRGraph) ("apply", Nil, (("numNodes", MInt), ("numEdges", MInt), ("nodes", MArray(MInt)), ("edges", MArray(MInt))) :: CSRGraph) implements
      allocates(CSRGraph, ${$0}, ${$1}, ${$2}, ${$3})

    static (CSRGraph) ("apply", Nil, (("nodes", DenseVector(MInt)), ("edges", DenseVector(MInt))) :: CSRGraph) implements composite ${
      CSRGraph($0.length - unit(1), $1.length, densevector_raw_data($0), densevector_raw_data($1))
    }

    data(CSRNgbr, ("_edgeId", MInt), ("_nodeId", MInt))

    static (CSRNgbr) ("apply", Nil, (("edgeId", MInt), ("nodeId", MInt)) :: CSRNgbr) implements
      allocates (CSRNgbr, ${$0}, ${$1})

    val CSRNgbrOps = withTpe(CSRNgbr)
    CSRNgbrOps {
      infix ("edge") (Nil :: MInt) implements getter(0, "_edgeId")
      infix ("node") (Nil :: MInt) implements getter(0, "_nodeId")
    }

    val CSRGraphOps = withTpe(CSRGraph)
    CSRGraphOps {
      compiler ("csrgraph_get_numnodes") (Nil :: MInt) implements getter(0, "_numNodes")
      compiler ("csrgraph_get_numedges") (Nil :: MInt) implements getter(0, "_numEdges")
      compiler ("csrgraph_get_nodes") (Nil :: MArray(MInt)) implements getter(0, "_nodes")
      compiler ("csrgraph_get_edges") (Nil :: MArray(MInt)) implements getter(0, "_edges")

      infix ("numNodes") (Nil :: MInt) implements composite ${
        csrgraph_get_numnodes($self)
      }

      infix ("numEdges") (Nil :: MInt) implements composite ${
        csrgraph_get_numedges($self)
      }

      infix ("nodes") (Nil :: DenseVectorView(MInt)) implements composite ${
        DenseVectorView(csrgraph_get_nodes($self), unit(0), unit(1), csrgraph_get_numnodes($self) + unit(1), unit(true))
      }

      infix ("edges") (Nil :: DenseVectorView(MInt)) implements composite ${
        DenseVectorView(csrgraph_get_edges($self), unit(0), unit(1), csrgraph_get_numedges($self), unit(true))
      }

      infix ("ngbrNodes") (MInt :: DenseVectorView(MInt)) implements composite ${
        val eidx = array_apply(csrgraph_get_nodes($self), $1)
        val fidx = array_apply(csrgraph_get_nodes($self), $1 + unit(1))
        DenseVectorView(
          csrgraph_get_edges($self), 
          eidx,
          unit(1),
          fidx - eidx,
          unit(true)
        )
      }

      infix ("ngbrEdges") (MInt :: IndexVector) implements composite ${
        val eidx = array_apply(csrgraph_get_nodes($self), $1)
        val fidx = array_apply(csrgraph_get_nodes($self), $1 + unit(1))
        (eidx :: fidx)
      }

      infix ("ngbrs") (MInt :: DenseVector(CSRNgbr)) implements composite ${
        val eidx = array_apply(csrgraph_get_nodes($self), $1)
        val fidx = array_apply(csrgraph_get_nodes($self), $1 + unit(1))
        densevector_fromfunc(fidx - eidx, unit(true), i => CSRNgbr(eidx + i, array_apply(csrgraph_get_edges($self), eidx + i)) )
      }

      infix ("deepcopy") (Nil :: CSRGraph) implements composite ${
        CSRGraph(
          csrgraph_get_numnodes($self),
          csrgraph_get_numedges($self),
          array_clone(csrgraph_get_nodes($self)),
          array_clone(csrgraph_get_edges($self))
        )
      }
    }
  }
}

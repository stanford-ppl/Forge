/*//////////////////////////////////////////////////////////////
Author: Alex G B Jin

Description: Combination of graph and sparsematrix traits from
optigraph and optila. This implementation supports CSR and CSR+
CSC format.
*///////////////////////////////////////////////////////////////
package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait SparseOps {
  this: OptiLADSL =>

  def importSparseOps() {
    importSparseDirectedGraphOps()
    importSparseUndirectedGraphOps()
  }

  def importSparseDirectedGraphOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")
    val CSR = lookupTpe("CSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val SparseDirectedGraph = lookupTpe("SparseDirectedGraph")
    val DenseVector = lookupTpe("DenseVector")
    data(SparseDirectedGraph, ("_outNeighbors", CSR(T)), ("_inNeighbors", CSR(T)), ("_nodeprop", DenseVector(T)))
    compiler (SparseDirectedGraph) ("sparse_directed_graph_alloc_raw", T, MethodSignature(List(CSR(T), CSR(T), DenseVector(T)), SparseDirectedGraph(T))) implements allocates(SparseDirectedGraph, ${$0}, ${$1}, ${$2})

    val SparseDirectedGraphOps = withTpe (SparseDirectedGraph)
    SparseDirectedGraphOps {
      compiler ("get_outNeighbors" ) (Nil :: CSR(T)) implements getter(0, "_outNeighbors")
      compiler ("get_inNeighbors") (Nil :: CSR(T)) implements getter(0, "_inNeighbors")
      compiler ("get_nodeprop_directed" ) (Nil :: DenseVector(T)) implements getter(0, "_nodeprop")
      compiler ("set_outNeighbors" ) (CSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_outNeighbors", ${$1})
      compiler ("set_inNeighbors") (CSR(T) :: MUnit, effect = write(0)) implements setter(0, "_inNeighbors", ${$1})
      compiler ("set_nodeprop_directed") (DenseVector(T) :: MUnit, effect = write(0)) implements setter(0, "_nodeprop", ${$1})
      infix ("numRows") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numCols }
      infix ("numNodes") (Nil :: MInt) implements composite ${ get_outNeighbors($self).numRows }
      infix ("numEdges") (Nil :: MInt) implements composite ${ get_outNeighbors($self).nnz }
      infix ("isDirected") (Nil :: MBoolean) implements composite ${true}
      infix ("outNeighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_outNeighbors($self).getRowIndices($1) } 
      infix ("inNeighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_inNeighbors($self).getRowIndices($1) }
      infix ("nodeProp") (Nil :: DenseVector(T)) implements composite ${ get_nodeprop_directed($self) }
      infix ("outDegree") (MInt :: MInt) implements composite ${ $self.outNeighbors($1).length() }
      infix ("inDegree") (MInt :: MInt) implements composite ${ $self.inNeighbors($1).length() }
      infix ("sumOverInNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val in_neighbors = $self.inNeighbors($1)
        in_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      // (Node, Neighbor==>value, filter) :: sum
      infix ("sumOverInNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val in_neighbors = $self.inNeighbors($1)
        in_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Node, Neighbor==>value) :: sum
      infix ("sumOverOutNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val out_neighbors = $self.outNeighbors($1)
        out_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      infix ("sumOverOutNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val out_neighbors = $self.outNeighbors($1)
        out_neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Nodes Array, (Nodeprop, Node)==>value) :: sum
      infix ("sumOverNodes") ((MArray(MInt), (MInt, T) ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val nodes = densevector_fromarray($1, unit(true))
        val nodeprop = $self.nodeProp
        nodes.map[R]({n => $2(n, nodeprop(n))}).reduce({(a,b) => a+b})
      }
    }
  }

  def importSparseUndirectedGraphOps() {
    val T = tpePar("T")
    val R = tpePar("R")
    val B = tpePar("B")
    val CSR = lookupTpe("CSR")
    val SparseRowView = lookupTpe("SparseRowView")
    val SparseUndirectedGraph = lookupTpe("SparseUndirectedGraph")
    val DenseVector = lookupTpe("DenseVector")
    data(SparseUndirectedGraph, ("_neighbors", CSR(T)), ("_nodeprop", DenseVector(T)))
    compiler (SparseUndirectedGraph) ("sparse_undirected_graph_alloc_raw", T, MethodSignature(List(CSR(T), DenseVector(T)), SparseUndirectedGraph(T))) implements allocates(SparseUndirectedGraph, ${$0}, ${$1})

    val SparseUndirectedGraphOps = withTpe (SparseUndirectedGraph)
    SparseUndirectedGraphOps {
      compiler ("get_neighbors" ) (Nil :: CSR(T)) implements getter(0, "_neighbors")
      compiler ("get_nodeprop_undirected" ) (Nil :: DenseVector(T)) implements getter(0, "_nodeprop")
      compiler ("set_neighbors" ) (CSR(T) :: MUnit, effect = write(0)) implements setter(0,  "_neighbors", ${$1})
      compiler ("set_nodeprop_undirected") (DenseVector(T) :: MUnit, effect = write(0)) implements setter(0, "_nodeprop", ${$1})
      infix ("numRows") (Nil :: MInt) implements composite ${ get_neighbors($self).numRows }
      infix ("numCols") (Nil :: MInt) implements composite ${ get_neighbors($self).numCols }
      infix ("numNodes") (Nil :: MInt) implements composite ${ get_neighbors($self).numRows }
      infix ("numEdges") (Nil :: MInt) implements composite ${ get_neighbors($self).nnz }
      infix ("isDirected") (Nil :: MBoolean) implements composite ${false}
      infix ("neighbors") (MInt :: SparseRowView(MInt)) implements composite ${ get_neighbors($self).getRowIndices($1) } 
      infix ("nodeProp") (Nil :: DenseVector(T)) implements composite ${ get_nodeprop_undirected($self) }
      infix ("degree") (MInt :: MInt) implements composite ${ $self.neighbors($1).length() }
      infix ("sumOverNeighbors") ((MInt, MInt ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val neighbors = $self.neighbors($1)
        neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => true})
      }
      infix ("sumOverNeighborsCond") ((MInt, MInt ==> R, MInt ==> MBoolean) :: R, TArith(R), addTpePars = R) implements composite ${
        val neighbors = $self.neighbors($1)
        neighbors.mapreduce[R]({n => $2(n)},{(a,b) => a+b}, {n => $3(n)})
      }
      // (Nodes Array, (Nodeprop, Node)==>value) :: sum
      infix ("sumOverNodes") ((MArray(MInt), (MInt, T) ==> R) :: R, TArith(R), addTpePars = R) implements composite ${
        val nodes = densevector_fromarray($1, unit(true))
        val nodeprop = $self.nodeProp
        nodes.map[R]({n => $2(n, nodeprop(n))}).reduce({(a,b) => a+b})
      }
    }
  }
}

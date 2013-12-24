package ppl.dsl.forge
package dsls 
package optigraph

import optiql.OptiQLDSL
import core.{ForgeApplication,ForgeApplicationRunner}

// This object lets us build our DSL
object OptiGraphDSLRunner extends ForgeApplicationRunner with OptiGraphDSL

trait OptiGraphDSL extends ForgeApplication 
  with GraphOps with NodeDataOps with NodeOps with EdgeOps 
    with NodeIdViewOps with NodeDataViewOps 
    with AtomicIntArrayOps with AtomicBooleanOps with IOGraphOps{
  /**
   * The name of our DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  override def dslName = "OptiGraph"
    
  /**
   * The specification is the DSL definition (types, data structures, ops, code generators)
   */
  override def specification() = {
    /**
     * Include Scala ops
     */
    importScalaOps()
        
    /**
     * The main portion of our DSL
     */
    importNodeOps()
    importEdgeOps()
    importNodeDataOps()
    importAtomicBooleanOps()
    importAtomicIntArrayOps()
    importNodeDataViewOps()
    importNodeIdViewOps()
    importGraphOps()
    importIOGraphOps()
  }
} 

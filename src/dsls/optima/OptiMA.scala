package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL

trait OptiMADSL extends ForgeApplication with MultiArrayOps {

  def dslName = "OptiMA"

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    /*importPrimitives()
    importMisc()
    importCasts()
    importOrdering()
    importStrings()
    importMath()
    importTuples()
    importHashMap()
    importConcurrentHashMap()*/

    // OptiLA types
    // declare all tpes first, so that they are available to all ops (similar to Delite)
    val T = tpePar("T")
    val ma = abstractFamily("MultiArrayFamily")
    val ArrayND = abstractTpe("ArrayND", T, ma)
    val Array1D = abstractTpe("Array1D", T, ma) isA ArrayND
    val Array2D = abstractTpe("Array2D", T, ma) isA ArrayND
    val Array3D = abstractTpe("Array3D", T, ma) isA ArrayND

    // rewrites
    //extern(grp("Rewrite"), targets = Nil)
    //extern(grp("Distributed"), targets = List($cala))
  }
}

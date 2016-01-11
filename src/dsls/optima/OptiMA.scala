package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL

trait OptiMADSL extends ForgeApplication with MultiArrayOps {

  def dslName = "OptiMA"
  override val clearVisitors = true

  def specification() = {
    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    //importMisc()
    //importPrimitives()
    //importCasts()
    //importOrdering()
    //importStrings()
    //importMath()
    //importTuples()
    noInfixList :::= List("toInt", "toFloat", "toDouble", "toLong")

    /*
    importHashMap()
    importConcurrentHashMap()*/

    // MultiArray figment types (with subtyping)
    val T = tpePar("T")
    val ArrayND = figmentTpe("ArrayND", T)
    val Array1D = figmentTpe("Array1D", T) isA ArrayND
    val Array2D = figmentTpe("Array2D", T) isA ArrayND
    val Array3D = figmentTpe("Array3D", T) isA ArrayND





    importMultiArrayOps()

    // rewrites
    //extern(grp("Rewrite"), targets = Nil)
    //extern(grp("Distributed"), targets = List($cala))
  }
}

package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// TODO:
// - Autodoc
// - Forge errors (source context is useless right now)
// - Completeness checks for analyses
// - Convergence conditions for analyses
// - Pre- and Post-processing rules for analyses
// - figment should operate more like a redirect for library implementation
// - separate metadata meet, etc. functions into separate functions generated in Impls
// - generate atomic writes from Forge
// - Add Global precision to OptiMA extern
// - Error when creating static methods with same object group as Metadata (case class)
//   - workaround: Use different group for now
// - Need quick syntax for referring to arguments of lambdas in analysis/transformer functions
//   - bound args are named using f___arg#___arg#
//     where first # is argnum of lambda and second is argnum of lambda's arg
// - Propagation of metadata via syms
//   - Subproblem: can we define contain and extracts syms on DeliteArray or does this break things?
// - Optional identification of figment types as "array-like" or "struct-like" - can this be done automatically?

// TEST:

// DONE:
// - disable error with field shortcutting on figment types (new rule for unapply on structs for figments)
// - Fix type arguments in creating RefinedManifest
// - allow and generate inheritance for any type with no data structure definition
// - Move FigmentStruct to extern (Records)
// - Add option to disable struct unwrapping in LMS
// - Add new sugar for modules in DADL
// - Why are if-then-else statements being staged even when the condition is a Scala constant?
//   - Unable to reproduce (try using System.out.println instead?)
// - Library version of Blocks? How to handle transformation rules for these?

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication
  with OptiMAPropagation
  with MultiArrays with MultiArrayImpls with MultiArrayMetadata
  with ArrayLowering with MultiArrayAnalysis
  with RangeOps with Stringables {

  def dslName = "OptiMA"
  override def clearTraversals = true

  def specification() = {

    // our selection of Scala ops
    // we don't use Numeric or Fractional, since they are replaced by Arith
    importStrings()
    importMisc()
    importPrimitives()
    //importCasts()
    //importOrdering()
    //importMath()
    //importTuples()
    //noInfixList :::= List("toInt", "toFloat", "toDouble", "toLong")

    // MultiArray figment types (with subtyping)
    val T = tpePar("T")
    val ArrayND = tpe("ArrayND", T)
    val Array1D = tpe("Array1D", T) augments ArrayND
    val Array2D = tpe("Array2D", T) augments ArrayND
    val Array3D = tpe("Array3D", T) augments ArrayND

    val ImplND  = tpe("ImplND", T) augments ArrayND
    val FlatND  = figmentTpe("FlatND", T) augments ImplND

    val Indices = figmentTpe("Indices")
    val LoopIndices = figmentTpe("LoopIndices") augments Indices

    importRanges()
    importIndices()
    importStringables()

    importMultiArrayMetadata()
    importMultiArrays()
    importMultiArrayImpls()
    importMultiArrayAnalyses()
    importArrayLowering()
    importOptiMAProps()

    val RankAnalyzer = analyzer("Rank")
    //val RankChecker  = analyzer("RankCheck")
    //val ArrayWrapper = transformer("ArrayWrapper", isExtern=true)
    val LayoutAnalyzer = analyzer("Layout", isExtern=true)
    val ArrayLowering = transformer("ArrayLowering")

    schedule(IRPrinter)
    schedule(RankAnalyzer)
    //schedule(RankChecker)
    //schedule(ArrayWrapper)
    schedule(LayoutAnalyzer)
    schedule(ArrayLowering)
    schedule(IRPrinter)

    //schedule(MultiloopSoA)

    // rewrites
    //extern(grp("Rewrite"), targets = Nil)
    //extern(grp("Distributed"), targets = List($cala))
  }
}

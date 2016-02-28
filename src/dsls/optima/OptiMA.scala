package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

// TODO:
// - Autodoc
// - Forge errors (source context is useless right now)
// - Miscellaneous warnings and errors for improper use of forge API
// - Completeness checks for analyses
// - Convergence conditions for analyses
// - Pre- and Post-processing rules for analyses
// - figment should operate more like a redirect for library implementation?
// - generate atomic writes from Forge
// - Add Global precision to OptiMA extern
// - Error when creating static methods with same object group as Metadata (case class)
//   - workaround: Use different group for now
//   - separate metadata meet, etc. functions into separate functions generated in Impls, change case class to classes?
// - Propagation of metadata via syms
//   - Subproblem: can we define contain and extracts syms on DeliteArray or does this break things?
// - Optional identification of types as "array-like" or "struct-like" - can this be done automatically?
//   - types with data structure definition will always show up as struct-like, no change needed
//   - types which have ops implemented as records should be struct-like
//   - all other types need to be specified as arrayLike or scalarLike (scalarLike is default)
//   - use fig vs. figarray vs. figstruct?

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
// - Need quick syntax for referring to arguments of lambdas in analysis/transformer functions
//   - bound args are named using f___arg#___arg#
//     where first # is argnum of lambda and second is argnum of lambda's arg

object OptiMADSLRunner extends ForgeApplicationRunner with OptiMADSL
trait OptiMADSL extends ForgeApplication
  with OptiMAPropagation
  with MultiArrays with MultiArrayImpls with MultiArrayMetadata
  with ArrayLowering with MultiArrayAnalysis
  with RangeOps with Stringables {

  def dslName = "OptiMA"
  override def addREPLOverride = false
  override def clearTraversals = true

  val PROP_TEST = true

  def specification(): Rep[Unit] = {
    if (PROP_TEST) propagationTest()

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

  def propagationTest() {
    val T = tpePar("T")
    val R = tpePar("R")
    val Container = tpe("Container", (T,R))
    data(Container, ("_data", MArray(T)), ("_other", R))

    static (Container) ("apply", (T,R), (MArray(T), R) :: Container(T,R), effect = mutable) implements allocates(Container, ${$0}, ${$1})

    val ContainerOps = withTpe(Container)
    ContainerOps {
      infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
      infix ("other") (Nil :: R) implements getter(0, "_other")
      infix ("setData") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", ${$1})
      infix ("setOther") (R :: MUnit, effect = write(0)) implements setter(0, "_other", ${$1})

      infix ("apply") (MInt :: T) implements composite ${ $self.data.apply($1) }
      infix ("update") ((MInt, T) :: MUnit, effect = write(0)) implements composite ${ $self.data($1) = $2 }
    }

    val Tag = metadata("Tag", ("tag", SBoolean))
    onMeet (Tag) ${ this }
    direct (Tag) ("tag", Nil, MAny :: MUnit, effect = simple) implements composite ${ setMetadata($0,Tag(true)) }

    val Prop = analyzer("TestPropagation") // Default propagation rules
    schedule(Prop)
    schedule(IRPrinterPlus)
  }
}

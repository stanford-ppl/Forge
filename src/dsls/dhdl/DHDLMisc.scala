package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMisc {
  this: DHDLDSL =>

  def importDHDLMisc() {
    importDHDLHelper()
    importDHDLTestingOps()
  }

	def importDHDLHelper() {
    val Misc = grp("DHDLMisc")

    val T = tpePar("T")
    val Fix = lookupTpe("Fix")

    // Multi-dimensional addressing
    internal (Misc) ("calcAddress", Nil, (("indices", SList(Fix)), ("dims", SList(SInt))) :: Fix) implements composite ${
      if ($indices.length == 1) indices.apply(0)  // Flat indexing is always allowed
      else {
        if ($indices.length != $dims.length) {
          throw new Exception("Trying to address " + $dims.length + "D memory using " + $indices.length + "D addressing")
        }
        val strides = List.tabulate($dims.length){d =>
          if (d == $dims.length - 1) 1
          else $dims.drop(d + 1).reduce(_*_)
        }
        List.tabulate($indices.length){i => strides(i).toFixPt * $indices(i) }.reduce{_+_}
      }
    }

    // Scala's fold and reduce don't produce a binary tree - use these functions instead
    internal (Misc) ("reductionTree", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      if ($0.length == 1) $0
      else if ($0.length % 2 == 0) reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
      else reductionTree( List.tabulate($0.length/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
    }
    internal (Misc) ("productTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => a * b}).head
    }
    internal (Misc) ("sumTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reductionTree[T]($0, {(a,b) => a + b}).head
    }
  }

  // --- Unsynthesizable operations used for testing purposes only
  def importDHDLTestingOps() {
    val Tst = grp("Testing")
    val Bit = lookupTpe("Bit")

    val println  = direct (Tst) ("println", Nil, MAny :: MUnit, effect = simple)
    val println2 = direct (Tst) ("println", Nil, Nil :: MUnit, effect = simple)
		val assert   = direct (Tst) ("assert", Nil, Bit :: MUnit, effect = simple)

    impl (println)  (codegen($cala, ${ println($0) }))
    impl (println2) (codegen($cala, ${ println() }))
    impl (assert)   (codegen($cala, ${ assert($0) }))
	}
}

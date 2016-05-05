package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLVectors {
  this: DHDLDSL =>

  def importVectors() {
    val T = tpePar("T")
    val Vector = lookupTpe("Vector")

    // --- Nodes
    /** @nodoc **/
    val transparentVector = direct (Vector) ("transparentVector", T, SList(T) :: Vector(T))
    /** @nodoc **/
    val vectorSlice = direct (Vector) ("vectorSlice", T, (Vector(T), SInt, SInt) :: Vector(T))
    /** @nodoc **/
    val vectorApply = direct (Vector) ("vectorApply", T, (Vector(T), SInt) :: Vector(T))

    /** Creates a new Vector containing the given elements
     * @param elems
     **/
    static (Vector) ("apply", T, varArgs(T) :: Vector(T)) implements composite ${
      val elems = $0.toList
      if (elems.length < 1) stageError("Cannot create empty Vector")
      transparentVector(elems)
    }

    /** Creates a subvector of this vector with elements [start, end)
     * @param start: index of the first element to include in the subvector
     * @param end: end index of the slice, non-inclusive
     **/
    infix (Vector) ("slice", T, (Vector(T), SInt, SInt) :: Vector(T)) implements redirect ${ vectorSlice($0, $1, $2) }

    /** Extracts the element of this vector at the given index
     * @param i: the index of the element to extract
     **/
    infix (Vector) ("apply", T, (Vector(T), SInt) :: T) implements redirect ${ vectorApply($0, $1) }


    // --- Rewrite rules
    rewrite (vectorSlice) using pattern((${Def(EatReflect(TransparentVector(elems)))},${start},${end}) -> ${
      if (start >= end) stageError("Cannot create empty Vector")
      if (end >= elems.length) stageError("Vector slice exceeds length of original Vector")
      Vector(elems.slice(start, end))
    })
    rewrite (vectorApply) using pattern((${Def(EatReflect(TransparentVector(elems)))}, ${i}) -> ${
      if (i < 0 && i >= elems.length) stageError("Invalid Vector apply: " + i)
      elems(i)
    })


    // --- Scala Backend
    impl (transparentVector) (codegen($cala, ${ Array($0:_*) }))
    impl (vectorSlice) (codegen($cala, ${ $0.slice($1, $2) }))
    impl (vectorApply) (codegen($cala, ${ $0.apply($1) }))




  }
}

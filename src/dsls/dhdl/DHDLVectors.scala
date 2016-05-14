package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLVectors {
  this: DHDLDSL =>

  def importDHDLVectors() {
    val T = tpePar("T")
    val Vector = lookupTpe("Vector")

    // --- Nodes
    // vector_from_list, vector_new -- see extern
    val vector_slice = internal (Vector) ("vec_slice", T, (Vector(T), SInt, SInt) :: Vector(T))
    val vector_apply = internal (Vector) ("vec_apply", T, (Vector(T), SInt) :: T)

    // --- Internals
    internal (Vector) ("vector_list_create", T, SList(T) :: Vector(T)) implements composite ${
      val vec = vector_from_list($0)
      dimsOf(vec) = List($0.length.as[Index])
      vec
    }

    // Not yet supported
    /*val vector_create = internal (Vector) ("vector_create", T, MInt :: Vector(T)) implements composite ${
      val vec = vector_new[T]($0)
      dimsOf(vec) = List($0)
      vec
    }*/

    /** Creates a new Vector containing the given elements
     * @param elems
     **/
    static (Vector) ("apply", T, varArgs(T) :: Vector(T)) implements composite ${
      val elems = $0.toList
      if (elems.length < 1) stageError("Cannot create empty Vector")
      vector_list_create(elems)
    }

    /** Creates a subvector of this vector with elements [start, end)
     * @param start: index of the first element to include in the subvector
     * @param end: end index of the slice, non-inclusive
     **/
    infix (Vector) ("slice", T, (Vector(T), SInt, SInt) :: Vector(T)) implements composite ${ vec_slice($0, $1, $2) }

    /** Extracts the element of this vector at the given index
     * @param i: the index of the element to extract
     **/
    infix (Vector) ("apply", T, (Vector(T), SInt) :: T) implements composite ${ vec_apply($0, $1) }


    // --- Rewrite rules
    rewrite (vector_slice) using pattern((${Def(EatReflect(Vector_from_list(elems)))},${start},${end}) -> ${
      if (start >= end) stageError("Cannot create empty Vector")
      if (end >= elems.length) stageError("Vector slice exceeds length of original Vector")
      vector_create(elems.slice(start, end)).asInstanceOf[Rep[Vector[T]]]
    })
    rewrite (vector_apply) using pattern((${Def(EatReflect(Vector_from_list(elems)))}, ${i}) -> ${
      if (i < 0 && i >= elems.length) stageError("Invalid Vector apply: " + i)
      elems(i).asInstanceOf[Rep[T]]
    })

    /*rewrite(vector_create) using rule ${
      $0 match {
        case ConstFix(_) =>
        case ParamFix(_) =>
        case _ => stageError("Only constants and DSE parameters are allowed as sizes of Vectors")
      }
      super.vector_create[T]($0)
    }*/

    // --- Scala Backend
    impl (vector_slice) (codegen($cala, ${ $0.slice($1, $2) }))
    impl (vector_apply) (codegen($cala, ${ $0.apply($1) }))
  }
}

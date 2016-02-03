package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait VisibilityTestOps { this: OptiMADSL =>

  def importVisibilityTest() {
    // --- Ranges
    val Range = tpe("Range")
    data(Range, ("_start", MInt), ("_end", MInt), ("_stride", MInt))
    internal.infix (Range) ("start", Nil, Range :: MInt) implements getter(0, "_start")
    internal.infix (Range) ("end", Nil, Range :: MInt) implements getter(0, "_end")
    internal.infix (Range) ("stride", Nil, Range :: MInt) implements getter(0, "_stride")
    internal.infix (Range) ("len", Nil, Range :: MInt) implements composite ${ ($0.end - $0.start) / $0.stride }

    internal.static (Range) ("apply", Nil, (MInt,MInt,MInt) :: Range) implements allocates(Range, quotedArg(0), quotedArg(1), quotedArg(2))

    internal.infix (Range) ("to", Nil, (MInt, MInt) :: Range) implements composite ${ Range($0, $1+1, 1) }
    internal.infix (Range) ("until", Nil, (MInt,MInt) :: Range) implements composite ${ Range($0, $1, 1) }
    internal.infix (Range) ("by", Nil, (Range, MInt) :: Range) implements composite ${ Range($0.start, $0.end, $1) }
    internal.infix (Range) ("foreach", Nil, (Range, MInt ==> MUnit) :: MUnit) implements composite ${ seqloop($0.start,$0.end,$0.stride,$1) }

    internal (Range) ("seqloop", Nil, (("start",MInt),("end",MInt),("stride",MInt),("func",MInt ==> MUnit)) :: MUnit) implements codegen($cala, ${
      var i = $start
      while (i < $end) {
        $b[func](i)
        i += $stride
      }
    })

    //internal.infix (Range) ("::", Nil, (MInt, MInt) :: Range) implements composite ${ Range($0, $1, 1) }
    //noSourceContextList ::= "::"
    noInfixList ::= "foreach"

    // --- Vectors
    /*val Vector = tpe("Vector")
    data(Vector, ("_length",MInt), ("_data",MArray(MInt)), ("_isRow", MBoolean))

    infix (Vector) ("length", Nil, Vector :: MInt) implements getter(0, "_length")
    internal.infix (Vector) ("isRow", Nil, Vector :: MBoolean) implements getter(0, "_isRow")
    internal.infix (Vector) ("data", Nil, Vector :: MArray(MInt)) implements getter(0, "_data")

    // Allocators
    static (Vector) ("apply", Nil, MInt :: Vector) implements allocates(Vector, quotedArg(0), ${array_empty[Int]($0)}, ${unit(true)})
    internal (Vector) ("vector_from_array", Nil, MArray(MInt) :: Vector) implements allocates(Vector, ${array_length($0)}, quotedArg(0), ${unit(true)})
    internal (Vector) ("vector_allocate", Nil, (MInt, MArray(MInt), MBoolean) :: Vector) implements allocates(Vector, ${$0},${$1},${$2})

    infix (Vector) ("apply", Nil, (Vector,MInt) :: MInt) implements composite ${ array_apply($0.data, $1) }

    // Printing
    direct (Vector) ("print", Nil, MAny :: MUnit, effect=simple) implements codegen($cala, ${print($0)})
    direct (Vector) ("pprint", Nil, Vector :: MUnit, effect=simple) implements single ${
      for (i <- unit(0).until($0.length)) {
        print( $0(i) )
        if ($0.isRow) print(" ") else print("\\n")
      }
      print("\\n")
    }

    fimplicit (Range) ("range_to_vector", Nil, Range :: Vector) implements composite ${
      val data = array_fromfunction[Int]($0.len,i=>i*$0.stride + $0.start)
      vector_from_array(data)
    }

    // NOTE: Not a great way of writing this, just need to test internal implicit
    internal.fimplicit (Vector) ("vec_to_array", Nil, Vector :: MArray(MInt)) implements composite ${ $0.data }
    infix (Vector) ("t", Nil, Vector :: Vector) implements composite ${ vector_allocate($0.length, $0, !$0.isRow) }
  */
  }
}
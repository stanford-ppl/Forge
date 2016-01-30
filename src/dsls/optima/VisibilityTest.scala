package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait VisibilityTestOps { this: OptiMADSL =>

  def importVisibilityTest2() {
    val ImplForm = metadata("ImplForm", ("v", SInt))
    val Buffer = metadata("MBuffer", ("impl", ImplForm))
    val View = metadata("MView", ("impl", ImplForm))

    val Rank = metadata("MRank", ("rank", SInt))
    val MayUpdate = metadata("MayUpdate", ("mayUpdate", SBoolean))
    //val Precision = metadata("Precision", ("signed", SBoolean), ("intBits", SInt), ("decBits", SInt))

    //compiler (ImplForm) ("NoImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(0) }
    //compiler (ImplForm) ("PhysOnly", Nil, Nil :: ImplForm) implements composite ${ ImplForm(1) }
    //compiler (ImplForm) ("TrueImpl", Nil, Nil :: ImplForm) implements composite ${ ImplForm(2) }

    // Meet: proposed syntax version 1
    // Default for matching is case class equality (which is generated as field equality)
    // Default for MetaOverwrite meet case is keep the rhs

    meet (ImplForm) ${ if (this != that) ImplForm(1) else that }
    meet (Buffer) ${ MBuffer(meet(this.impl, that.impl)) }
    meet (View) ${ MView(meet(this.impl, that.impl)) }

    meet (Rank) ${ MRank(this.rank) }
    meet (MayUpdate) ${ MayUpdate(this.mayUpdate || that.mayUpdate) }

    //meet (Precision, alias = branch) ${ ... }

  }

  def importVisibilityTest() {
    // --- Primitives
    val Prim = grp("Primitive")

    lift (Prim) (MInt)
    lift (Prim) (MBoolean)
    lift (Prim) (MString)

    val not = infix (Prim) ("unary_!", Nil, MBoolean :: MBoolean) implements codegen($cala, "!" + quotedArg(0))
    rewrite (not) using forwarding ${ delite_boolean_negate($0) }

    val int_plus = direct (Prim) ("forge_int_plus", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 + $1})
    val int_minus = direct (Prim) ("forge_int_minus", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 - $1})
    val int_times = direct (Prim) ("forge_int_times", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 * $1})
    val int_divide = direct (Prim) ("forge_int_divide", Nil, (MInt,MInt) :: MInt) implements codegen($cala, ${$0 / $1})
    // Forward integer operations used for index calc to Delite internal implementations
    rewrite (int_plus) using forwarding ${ delite_int_plus($0, $1) }
    rewrite (int_minus) using forwarding ${ delite_int_minus($0, $1) }
    rewrite (int_times) using forwarding ${ delite_int_times($0, $1) }

    infix (Prim) ("+", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_plus($0, $1) }
    infix (Prim) ("-", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_minus($0, $1) }
    infix (Prim) ("*", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_times($0, $1) }
    infix (Prim) ("/", Nil, (MInt,MInt) :: MInt) implements redirect ${ forge_int_divide($0, $1) }

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
    noInfixList ::= "t"
    noInfixList ::= "isRow"

    // --- Vectors
    val Vector = tpe("Vector")
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
  }
}
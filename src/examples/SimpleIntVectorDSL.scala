package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * This DSL is here to make sure that things still work without type parameters.
 * It's a sad replacement for actual unit tests, which are still a big TODO.
 */
object SimpleIntVectorDSLRunner extends ForgeApplicationRunner with SimpleIntVectorDSL

trait SimpleIntVectorDSL extends ForgeApplication {
  def dslName = "SimpleIntVector"

  def specification() = {
    importScalaOps()
    importSimpleVectorOps()
  }

  def importSimpleVectorOps() {
    val Vector = tpe("Vector")
    data(Vector, ("_length", MInt), ("_data", MArray(MInt)))
    static (Vector) ("apply", Nil, MInt :: Vector, effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[Int]($0) })

    val VectorOps = withTpe (Vector)
    VectorOps {
      compiler ("vector_raw_data") (Nil :: MArray(MInt)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(MInt) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      infix ("length") (Nil :: MInt) implements getter(0, "_length")
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))

      infix ("apply") (MInt :: MInt) implements composite ${ array_apply(vector_raw_data($self), $1) }
      infix ("update") ((("i",MInt),("e",MInt)) :: MUnit, effect = write(0)) implements composite ${
        array_update(vector_raw_data($self), $i, $e)
      }

      infix ("slice") (MethodSignature(List(("start",MInt,"unit(0)"),("end",MInt)), Vector)) implements single ${
        val out = Vector($end - $start)
        var i = $start
        while (i < $end) {
          out(i-$start) = $self(i)
          i += 1
        }
        out
      }

      infix ("insert") ((MInt,MInt) :: MUnit, effect = write(0)) implements single ${
        vector_insertspace($self,$1,1)
        $self($1) = $2
      }

      infix ("append") ((MInt,MInt) :: MUnit, effect = write(0)) implements single ${
        $self.insert($self.length, $2)
      }

      compiler ("vector_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        vector_ensureextra($self,$len)
        val data = vector_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,$self.length-$pos)
        vector_set_length($self,$self.length+$len)
      }

      compiler ("vector_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        if (array_length(data) - $self.length < $extra) {
          vector_realloc($self, $self.length+$extra)
        }
      }

      compiler ("vector_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = vector_raw_data($self)
        var n = Math.max(unit(4), array_length(data)*2).toInt
        while (n < $minLen) n = n*2
        val d = array_empty[Int](n)
        array_copy(data, 0, d, 0, $self.length)
        vector_set_raw_data($self, d.unsafeImmutable)
      }


      // math
      infix ("+") (Vector :: Vector) implements zip((MInt,MInt,MInt), (0,1), ${ (a,b) => forge_int_plus(a,b) }) // unfortunate conflict with Delite PrimitiveOps unless we specify (TODO: need more disambiguations)
      infix ("*") (MInt :: Vector) implements map((MInt,MInt), 0, "e => e*"+quotedArg(1))
      infix ("sum") (Nil :: MInt) implements reduce(MInt, 0, ${0}, ${ (a,b) => forge_int_plus(a,b) })

      // bulk
      infix ("map") ((MInt ==> MInt) :: Vector) implements map((MInt,MInt), 0, ${ e => $1(e) })

      infix ("reduce") (((MInt,MInt) ==> MInt) :: MInt) implements reduce(MInt, 0, ${0}, ${
        (a,b) => $1(a,b)
      })

      infix ("filter") ((MInt ==> MBoolean) :: Vector) implements filter((MInt,MInt), 0, ${e => $1(e)}, ${e => e})

      infix ("mapreduce") ((MInt ==> MInt,(MInt,MInt) ==> MInt) :: MInt) implements composite ${
        $self.map($1).reduce($2)
      }

      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(MInt, 0, ${a => println(a)})


      // parallel collectionification
      compiler ("vector_raw_alloc") (MInt :: Vector) implements single ${
        Vector($1)
      }
      compiler ("vector_appendable") ((MInt,MInt) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector,MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = vector_raw_data($self)
        val dest = vector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(MInt, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))
    }

    ()
  }
}


package ppl.dsl.forge
package dsls
package optiql

import core.{ForgeApplication,ForgeApplicationRunner}

trait TableOps {
  this: OptiQLDSL =>

  def importTableOps() {
    val A = tpePar("A")
    val K = tpePar("K")
    val V = tpePar("V")
    val R = tpePar("R")

    val Table = tpe("Table", A)

    // data
    data(Table, "size" -> MInt, "data" -> MArray(A))
    static (Table) ("apply", A, MInt :: Table(A), effect = mutable) implements allocates(Table, ${$0}, ${array_empty[A]($0) })
    static (Table) ("apply", A, MArray(A) :: Table(A)) implements allocates(Table, ${array_length($0)}, ${$0})
    static (Table) ("apply", A, varArgs(A) :: Table(A)) implements allocates(Table, ${unit($0.length)}, ${array_fromseq($0)})

    static (Table) ("fromFile", A, (MString, MString ==> A) :: Table(A)) implements single ${
      val a = ForgeFileReader.readLines($0)($1)
      Table[A](a)
    }

    //(K,V) pairs for Tables
    val Tuple2 = lookupTpe("Tup2")
    fimplicit (Tuple2) ("pair_to_value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) } //TODO: this isn't kicking in
    infix (Tuple2) ("key", (K,V), Tuple2(K,Table(V)) :: K) implements composite ${ tup2__1($0) }
    infix (Tuple2) ("value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) }

    val TableOps = withTpe (Table)
    TableOps {

      // bulk collect ops
      infix ("Select") ("selector" -> (A ==> R) :: Table(R), addTpePars = R) implements map((A,R), 0, ${$selector})
      infix ("Where") ("predicate" -> (A ==> MBoolean) :: Table(A)) implements filter((A,A), 0, ${$predicate}, ${e => e})

      //bulk bucket ops
      infix ("GroupBy") ("keySelector" -> (A ==> K) :: Table(Tuple2(K,Table(A))), addTpePars = K) implements single ${
        groupByHackImpl($self, $keySelector)
      }

      // infix ("Distinct")

      //bulk reduce ops
      infix ("Sum") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric) implements mapReduce((A,R), 0, ${$selector}, ${zeroType[R]}, ${(a,b) => a + b})

      infix ("Average") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric withBound TFractional) implements single ${
        $self.Sum($selector) / upgradeInt(table_count($self)) //.asInstanceOf[Rep[R]] //TODO: ".Count" doesn't work
      }
      //infix ("Max") ("selector" -> (A ==> R) :: R, TOrdering(R), addTpePars = R) implements reduce(R, 0, null, ${(a,b) => a max b})
      //infix ("Min") ("selector" -> (A ==> R) :: R, TOrdering(R), addTpePars = R) implements reduce(R, 0, null, ${(a,b) => a min b})

      //simple ops that can also be implemented as a reduce
      infix ("Count") (Nil :: MInt) implements single ${ table_size($self) }
      infix ("First") (Nil :: A) implements single ${ table_apply($self, 0) }
      infix ("Last") (Nil :: A) implements single ${ table_apply($self, table_size($self)-1) }

      //sorting ops
      // infix ("OrderBy")
      // infix ("OrderByDescending")
      // infix ("ThenBy")
      // infix ("ThenbyDescending")

      infix ("OrderBy") ("selector" -> (A ==> R) :: Table(A), addTpePars = R) implements single ${$self}
      infix ("ThenBy") ("selector" -> (A ==> R) :: Table(A), addTpePars = R) implements single ${$self}

      //multi-table ops
      // infix ("Join")
      // infix ("Union")

      //internal transformed ops
      compiler("groupByReduce")(("keySelector" -> (A ==> K), "valueSelector" -> (A ==> V), "reducer" -> ((V,V) ==> V), "condition" -> (A ==> MBoolean)) :: Table(V), addTpePars = (K,V)) implements hashFilterReduce((A,K,V), 0, ${condition}, ${$keySelector}, ${$valueSelector}, ${zeroType[V]}, ${$reducer})
      compiler("bulkDivide") (("counts" -> Table(MInt), "avgFunc" -> ((A,MInt) ==> A)) :: Table(A)) implements zip((A,MInt,A), (0,1), ${$avgFunc})

      //misc ops
      // infix ("toString") (Nil :: MString) implements codegen($cala, ${
      // })

      //methods needed to implement Table as a Delite ParallelCollectionBufer

      //accessors
      compiler ("table_raw_data") (Nil :: MArray(A)) implements getter(0, "data")
      compiler ("table_size") (Nil :: MInt) implements getter(0, "size")
      compiler ("table_apply") (MInt :: A) implements composite ${
        array_apply(table_raw_data($self), $1)
      }

      //mutators
      compiler ("table_set_raw_data") (MArray(A) :: MUnit, effect = write(0)) implements setter(0, "data", quotedArg(1))
      compiler ("table_set_size") (MInt :: MUnit, effect = write(0)) implements setter(0, "size", quotedArg(1))
      compiler ("table_update") (("i"->MInt,"e"->A) :: MUnit, effect = write(0)) implements composite ${
        array_update(table_raw_data($self), $i, $e)
      }

      compiler ("table_alloc") (MInt :: Table(R), addTpePars = R, effect = mutable) implements composite ${
        Table[R]($1)
      }

      infix ("insert") ((MInt,A) :: MUnit, effect = write(0)) implements single ${
        table_insertspace($self,$1,1)
        table_update($self, $1, $2)
      }

      infix ("append") (A :: MUnit, effect = write(0)) implements single ${
        table_insert($self, table_size($self), $1)
      }

      compiler ("table_insertspace") ((("pos",MInt),("len",MInt)) :: MUnit, effect = write(0)) implements single ${
        table_ensureextra($self,$len)
        val data = table_raw_data($self)
        array_copy(data,$pos,data,$pos+$len,table_size($self)-$pos)
        table_set_size($self,table_size($self)+$len)
      }

      compiler ("table_ensureextra") (("extra",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = table_raw_data($self)
        if (array_length(data) - table_size($self) < $extra) {
          table_realloc($self, table_size($self)+$extra)
        }
      }

      compiler ("table_realloc") (("minLen",MInt) :: MUnit, effect = write(0)) implements single ${
        val data = table_raw_data($self)
        var n = Math.max(4, array_length(data)*2).toInt
        while (n < $minLen) n = n*2
        val d = array_empty[A](n)
        array_copy(data, 0, d, 0, table_size($self))
        table_set_raw_data($self, d.unsafeImmutable)
      }

      compiler ("table_appendable") ((MInt,A) :: MBoolean) implements single("true")
      compiler ("table_dc_append") ((MInt,A) :: MUnit, effect = write(0)) implements single ${
        table_append($self, $2)
      }
      compiler ("table_copy") ((MInt,Table(A),MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = table_raw_data($self)
        val dest = table_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(A, lookupOp("table_alloc"), lookupOp("table_size"), lookupOp("table_apply"), lookupOp("table_update"), lookupOp("table_set_size"), lookupOp("table_appendable"), lookupOp("table_dc_append"), lookupOp("table_copy"))
    }
  }
}

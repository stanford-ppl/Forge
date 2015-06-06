package ppl.dsl.forge
package dsls
package optiql

import core.{ForgeApplication,ForgeApplicationRunner}

trait TableOps {
  this: OptiQLDSL =>

  def importTableOps() {
    val A = tpePar("A") //input types
    val B = tpePar("B")
    val C = tpePar("C")
    val K = tpePar("K") //key tpe
    val V = tpePar("V") //value tpe
    val R = tpePar("R") //result tpe

    val Table = tpe("Table", A)

    //internal data format
    //TODO: this is equivalent to a DeliteArrayBuffer, we should be able to just use that
    data(Table, "size" -> MInt, "data" -> MArray(A))

    //static constructors
    static (Table) ("apply", A, MInt :: Table(A), effect = mutable) implements allocates(Table, ${$0}, ${array_empty[A]($0) })
    static (Table) ("apply", A, MArray(A) :: Table(A)) implements allocates(Table, ${array_length($0)}, ${$0})
    static (Table) ("apply", A, (MArray(A), MInt) :: Table(A)) implements allocates(Table, ${$1}, ${$0})
    static (Table) ("apply", A, varArgs(A) :: Table(A)) implements allocates(Table, ${unit($0.length)}, ${array_fromseq($0)})
    static (Table) ("range", Nil, (MInt, MInt) :: Table(MInt)) implements composite ${
      val a = array_fromfunction($1-$0, i => i + $0)
      Table[Int](a)
    }

    static (Table) ("fromFile", A, CurriedMethodSignature(List(List(MString), List(MString ==> A)), Table(A))) implements composite ${
      val a = ForgeFileReader.readLines($0)($1)
      Table[A](a)
    }

    static (Table) ("fromFile", A, ("path"->MString, "separator"->MString) :: Table(A)) implements composite ${
      val a = ForgeFileReader.readLines($path)(line => createRecord[A](array_string_split(line, $separator, -1)))
      Table[A](a)
    }

    static (Table) ("fromString", A, ("data"->MString, "rowSeparator"->MString, "columnSeparator"->MString) :: Table(A)) implements composite ${
      val a = array_map(array_string_split($data, $rowSeparator), (s:Rep[String]) => createRecord[A](array_string_split(s, $columnSeparator, -1)))
      Table[A](a)
    }

    //(K,V) pairs for Tables
    val Tuple2 = lookupTpe("Tup2")
    fimplicit (Tuple2) ("pair_to_value", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) } //TODO: this isn't kicking in
    infix (Tuple2) ("key", (K,V), Tuple2(K,Table(V)) :: K) implements composite ${ tup2__1($0) }
    infix (Tuple2) ("values", (K,V), Tuple2(K,Table(V)) :: Table(V)) implements composite ${ tup2__2($0) }


    //sorting convenience method on Orderables
    direct (Table) ("asc", (A,K), "keySelector" -> (A ==> K) :: ((A,A) ==> MInt), TOrdering(K)) implements composite ${
      (a,b) => compareHackImpl(keySelector(a), keySelector(b)) //TODO: integrate compare into Forge Ordering (codegen issues)
    }

    direct (Table) ("desc", (A,K), "keySelector" -> (A ==> K) :: ((A,A) ==> MInt), TOrdering(K)) implements composite ${
      (a,b) => compareHackImpl(keySelector(b), keySelector(a))
    }


    val TableOps = withTpe (Table)
    TableOps {

      // bulk collect ops
      infix ("Select") ("selector" -> (A ==> R) :: Table(R), addTpePars = R) implements map((A,R), 0, ${$selector})
      infix ("SelectMany") ("selector" -> (A ==> Table(R)) :: Table(R), addTpePars = R) implements flatMap((A,R), 0, ${$selector})
      infix ("Where") ("predicate" -> (A ==> MBoolean) :: Table(A)) implements filter((A,A), 0, ${$predicate}, ${e => e})

      //bulk bucketReduce ops
      //TODO: composite op
      //TODO: what are the semantics of GroupBy? does it return a Map or a Table(Pairs), or ...?
      infix ("GroupBy") ("keySelector" -> (A ==> K) :: Table(Tuple2(K,Table(A))), addTpePars = K) implements single ${
        groupByHackImpl($self, $keySelector)
      }

      infix ("Distinct") ("keySelector" -> (A ==> K) :: Table(A), addTpePars = K) implements composite ${
        val map = groupByReduceOp($self, $keySelector, (e:Rep[A]) => e, (a:Rep[A],b:Rep[A]) => a, (e:Rep[A]) => unit(true))
        Table[A](fhashmap_values(map))
      }
      infix ("Distinct") (Nil :: Table(A)) implements composite ${ $0.Distinct((k:Rep[A]) => k) }

      //bulk reduce ops
      infix ("Sum") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric) implements mapReduce((A,R), 0, ${$selector}, ${zeroType[R]}, ${(a,b) => a + b})

      //TODO: composite op
      infix ("Average") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TNumeric withBound TFractional) implements single ${
        $self.Sum($selector) / upgradeInt[R](table_count($self))
      }

      infix ("Max") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TOrdering) implements mapReduce((A,R), 0, ${$selector}, ${minValue[R]}, ${(a,b) => a max b})
      infix ("Min") ("selector" -> (A ==> R) :: R, addTpePars = R withBound TOrdering) implements mapReduce((A,R), 0, ${$selector}, ${maxValue[R]}, ${(a,b) => a min b})

      infix ("Count") ("predicate" -> (A ==> MBoolean) :: MInt) implements mapReduce((A,MInt), 0, ${e => unit(1)}, ${unit(0)}, ${(a,b) => a + b}, Some(${$predicate}))
      //TODO: these create a scalac typer bug in mirroring definition (see Ops.scala:634)
      //infix ("First") ("predicate" -> (A ==> MBoolean) :: A) implements mapReduce((A,A), 0, ${e => e}, ${zeroType[A]}, ${(a,b) => a}, Some(${$predicate}))
      //infix ("Last") ("predicate" -> (A ==> MBoolean) :: A) implements mapReduce((A,A), 0, ${e => e}, ${zeroType[A]}, ${(a,b) => b}, Some(${$predicate}))

      //simple ops that can also be implemented as a reduce
      infix ("Count") (Nil :: MInt) implements single ${ table_size($self) }
      infix ("First") (Nil :: A) implements single ${ table_apply($self, 0) }
      infix ("Last") (Nil :: A) implements single ${ table_apply($self, table_size($self)-1) }

      //sorting ops
      infix ("OrderBy") ("sorts" -> varArgs((A,A) ==> MInt) :: Table(A)) implements composite ${
        Predef.assert($sorts.length > 0, "[OPTIQL ERROR]: OrderBy requires at least one argument")
        val buf = new scala.collection.mutable.ArrayBuffer[(Rep[A],Rep[A])=>Rep[Int]]
        buf += $sorts(0)
        for (i <- 1 until $sorts.length) {
          val compound = (a:Rep[A],b:Rep[A]) => {
            val prev = buf(i-1)(a,b)
            if (prev == unit(0)) $sorts(i)(a,b)
            else prev
          }
          buf += compound
        }

        sortHackImpl($self, buf(buf.size-1))
      }
      noInfixList :::= List("OrderBy") //varArgs(Function) only resolves properly with implicits; move into Forge codegen logic?


      //multi-table ops
      infix ("Join") (CurriedMethodSignature(List(List("t2"->Table(B)), List("k1"->(A ==> K), "k2"->(B ==> K)), List("result"->((A,B) ==> R))), Table(R)), addTpePars = (B,K,R)) implements composite ${
        join2($self, $k1, $t2, $k2, $result)
      }

      compiler ("join2")(("k1" -> (A ==> K), "t2" -> Table(B), "k2" -> (B ==> K), "result" -> ((A,B) ==> R)) :: Table(R), addTpePars = (B,K,R)) implements composite ${
        //TODO: we want to hash the smaller collection, but the size may be unknown (on disk); we could use file size as an approximation if we had some rewrites
        val grouped = array_buffer_groupBy(array_buffer_new_imm(table_raw_data($self), array_length(table_raw_data($self))), $k1) //FIXME: array_buffer vs. table
        val empty = Table(array_empty_imm[R](unit(0))) //note: control effects on IfThenElse require some manual hoisting
        $t2.SelectMany(e2 => {
          if (fhashmap_contains(grouped, $k2(e2))) {
            val buf = fhashmap_get(grouped, $k2(e2))
            Table(array_buffer_unsafe_result(buf), array_buffer_length(buf)).Select(e1 => $result(e1,e2))
          }
          else empty
        })
      }

      /*compiler ("join3")(("t1" -> Table(A), "k1" -> (A ==> K), "t2" -> Table(B), "k2" -> (B ==> K), "t3" -> Table(C), "k3" -> (C ==> K), "result" -> ((A,B,C) ==> R)) :: Table(R), addTpePars = (B,C,K,R)) implements composite ${
        ???
      }*/

      infix ("toArray") (Nil :: MArray(A)) implements composite ${ array_fromfunction($self.size, i => $self.apply(i)) }

      //printing ops
      //see extern files, currently manual Scala implementations

      //internal transformed ops
      compiler("groupByReduce")(("keySelector" -> (A ==> K), "valueSelector" -> (A ==> V), "reducer" -> ((V,V) ==> V), "condition" -> (A ==> MBoolean)) :: Table(V), addTpePars = (K,V)) implements composite ${
        val map = groupByReduceOp($self, $keySelector, $valueSelector, $reducer, $condition)
        Table[V](fhashmap_values(map))
      }
      compiler("groupByReduceOp")(("keySelector" -> (A ==> K), "valueSelector" -> (A ==> V), "reducer" -> ((V,V) ==> V), "condition" -> (A ==> MBoolean)) :: MHashMap(K,V), addTpePars = (K,V)) implements groupByReduce((A,K,V), 0, ${$keySelector}, ${$valueSelector}, ${zeroType[V]}, ${$reducer}, Some(${condition}))
      compiler("bulkDivide") (("counts" -> Table(MInt), "avgFunc" -> ((A,MInt) ==> A)) :: Table(A)) implements zip((A,MInt,A), (0,1), ${$avgFunc})


      //methods needed to implement Table as a Delite ParallelCollectionBufer

      //accessors
      infix ("apply") (MInt :: A) implements composite ${ table_apply_internal($self, $1) }
      infix ("size") (Nil :: MInt) implements composite ${ table_size_internal($self) }

      compiler ("table_raw_data") (Nil :: MArray(A)) implements getter(0, "data")
      compiler ("table_size_internal") (Nil :: MInt) implements getter(0, "size")
      compiler ("table_apply_internal") (MInt :: A) implements composite ${
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
        var n = unit(4) max (array_length(data)*2)
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

      parallelize as ParallelCollectionBuffer(A, lookupOp("table_alloc"), lookupOp("table_size_internal"), lookupOp("table_apply_internal"), lookupOp("table_update"), lookupOp("table_set_size"), lookupOp("table_appendable"), lookupOp("table_dc_append"), lookupOp("table_copy"))
    }
  }
}

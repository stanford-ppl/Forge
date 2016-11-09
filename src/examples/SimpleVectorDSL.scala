package ppl.dsl.forge
package examples

import core.{ForgeApplication,ForgeApplicationRunner}

object SimpleVectorDSLRunner extends ForgeApplicationRunner with SimpleVectorDSL

@dsl
trait SimpleVectorDSL extends ForgeApplication {
  /*
   * The name of your DSL. This is the name that will be used in generated files,
   * package declarations, etc.
   */
  override def dslName    = "SimpleVector"
  override def dslAuthor  = "Stanford PPL"
  override def dslVersion = "0.1"

  // The specification is the DSL definition (types, data structures, ops)
  /***
   * SimpleVectorDSL is a simple domain specific language. It's primary purpose is to demonstrate some of the capabilities of Forge.
   ***/
  def specification() = {

    // Include Scala ops
    importScalaOps()

    // The main portion of our DSL
    importVectorOps()
  }


  def importVectorOps() {
    // generic type parameters we will use
    val T = tpePar("T")
    val R = tpePar("R")

    /**
     * A single dimensional, parallel data structure with a dense array backing store
     */
    val Vector = tpe("Vector", T)

    // data fields
    data(Vector, ("_length", MInt), ("_data", MArray(T)))

    /** Allocates an empty Vector of type T
     * @param length: the length of the Vector to be allocated
     * @return an empty Vector of given length and type T
     */
    static (Vector) ("apply", T, MInt :: Vector(T), effect = mutable) implements allocates(Vector, ${$0}, ${ array_empty[T]($0) })

    // doesn't rewrite correctly if we use "withTpe (Vector) {", but works if we use:
    val VectorOps = withTpe (Vector)

    VectorOps {
      // getters and setters
      compiler ("vector_raw_data") (Nil :: MArray(T)) implements getter(0, "_data")
      compiler ("vector_set_raw_data") (MArray(T) :: MUnit, effect = write(0)) implements setter(0, "_data", quotedArg(1))
      compiler ("vector_set_length") (MInt :: MUnit, effect = write(0)) implements setter(0, "_length", quotedArg(1))

      /** Gets the current length of the Vector
       * @return the current length
       */
      infix ("length") (Nil :: MInt) implements getter(0, "_length")

      /** Returns the element at the given index
       * @param i: the index
       * @return the element at the given index
       */
      infix ("apply") (MInt :: T) implements composite ${ array_apply(vector_raw_data($self), $1) }

      // example named arg
      /** Updates the element at index i to e
       *
       * The vector must be mutable for this method to be used
       * @param i: the index
       * @param e: the element to add to the Vector
       */
      infix ("update") ((("i",MInt),("e",T)) :: MUnit, effect = write(0)) implements composite ${
        array_update(vector_raw_data($self), $i, $e)
      }

      // example named, default arg. 'MethodSignature' is currently explicitly needed when mixing arg types.
      /** Creates a shallow copy of the vector from indices start to end, non-inclusive
       * @return a new vector with elements from start until end (non-inclusive)
       */
      infix ("slice") (MethodSignature(List(("start",MInt,"unit(0)"),("end",MInt)), Vector(T))) implements single ${
        val out = Vector[T]($end - $start)
        var i = $start
        while (i < $end) {
          out(i-$start) = $self(i)
          i += 1
        }
        out
      }

      /** Inserts a given element e to the vector at index i
       *
       * The vector must be mutable for this method to be used
       * @param i: the index
       * @param e: the element to be inserted
       */
      infix ("insert") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
        vector_insertspace($self,$1,1)
        $self($1) = $2
      }

      /** Appends the given element e to the end of the vector
       *
       * The vector must be mutable for this method to be used
       * @param i: ignored?
       * @param e: the element to be appended
       */
      infix ("append") ((MInt,T) :: MUnit, effect = write(0)) implements single ${
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
        var n = unit(4) max (array_length(data)*2)
        while (n < $minLen) n = n*2
        val d = array_empty[T](n)
        array_copy(data, 0, d, 0, $self.length)
        vector_set_raw_data($self, d.unsafeImmutable)
      }


      // math
      /** Element-wise addition of numeric vectors
       * @param that
       * @return a new vector with the result of element-wise addition on every element in this and that
       */
      infix ("+") (Vector(T) :: Vector(T), TNumeric(T)) implements zip((T,T,T), (0,1), ${ (a,b) => a+b })

      /** Element-wise multiplication of numeric vectors
       * @param that
       * @return a new vector with the result of element-wise multiplication on every element in this and that
       */
      infix ("*") (T :: Vector(T), TNumeric(T)) implements map((T,T), 0, "e => e*"+quotedArg(1))
      /** Sums up the elements in this numeric vector
       * @return The sum of all elements in this vector
       */
      infix ("sum") (Nil :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${ (a,b) => a+b })

      // bulk
      /** Builds a new vector by applying the given function to all elements in the vector
       * @param f: the function to apply to each element
       * @return a new vector resulting from collecting the results of applying f element-wise
       */
      infix ("map") ((T ==> R) :: Vector(R), addTpePars = R) implements map((T,R), 0, ${ e => $1(e) })

      /** Reduces the elements of this numeric vector using the supplied associative binary operator
       * @param rf: the associative function used to reduce elements
       * @return the result of applying the reduction operator or numeric zero if this vector is empty
       */
      infix ("reduce") (((T,T) ==> T) :: T, TNumeric(T)) implements reduce(T, 0, ${numeric_zero[T]}, ${
        (a,b) => $1(a,b)
      })

      /** Selects all elements that match the supplied predicate
       * @param pred: the predicate function
       * @return A new vector with only the elements that matched the given predicate
       */
      infix ("filter") ((T ==> MBoolean) :: Vector(T)) implements filter((T,T), 0, ${e => $1(e)}, ${e => e})

      /** Applies an element-wise function prior to reducing the resulting elements with the supplied associative binary operator
       * @param f: the function to apply to each element
       * @param rf: the associative reduction function
       * @return the result of applying the reduction operator, or numeric zero if this vector is empty
       */
      infix ("mapreduce") ((T ==> T,(T,T) ==> T) :: T, TNumeric(T)) implements mapReduce((T,T), 0, ${e => $1(e)}, ${numeric_zero[T]}, ${(a,b) => $2(a,b)})

      /** Builds a new collection by applying the supplied function on all elements and concatenating the resulting vectors
       * @param f: the function to apply to each element
       * @return a new vector with the elements of all generated vectors concatenated
       */
      infix ("flatMap") ((T ==> Vector(R)) :: Vector(R), addTpePars = R) implements flatMap((T,R), 0, ${ e => $1(e) })

      val K = tpePar("K")
      val V = tpePar("V")

      // the Forge 'groupBy' pattern is currently limited to returning an MHashMap(K,MArrayBuffer(V)), so we need to convert that to a DSL type to return
      compiler ("groupby_helper") ((T ==> K,T ==> V) :: MHashMap(K, MArrayBuffer(V)), addTpePars = (K,V)) implements groupBy((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)})

      /** A somewhat strange implementation of groupBy
       * @param k: the key function, used to determine the bucket of each element
       * @param v: the value function, used to determine the actual value added to the bucket
       * @return a vector of vectors, where each index corresponded to a key
       */
      infix ("groupBy") ((T ==> K,T ==> V) :: Vector(Vector(V)), addTpePars = (K,V)) implements composite ${
        val map = groupby_helper($self, $1, $2)
        val groups = fhashmap_values(map)
        val out = Vector[Vector[V]](array_length(groups))
        var i = 0
        while (i < array_length(groups)) {
          val inGroup = groups(i)
          val outGroup = Vector[V](array_buffer_length(inGroup))
          var j = 0
          while (j < array_buffer_length(inGroup)) {
            outGroup(j) = array_buffer_apply(inGroup, j)
            j += 1
          }
          out(i) = outGroup.unsafeImmutable
          i += 1
        }
        out.unsafeImmutable
      }

      /** Groups the given elements using the given key and value functions, then reduces groups into single values using the
       * supplied reduction function
       * @param k: the key function, used to determine which bucket to reduce elements into
       * @param v: the value function, used to determine what value to reduce for a given element
       * @param rf: the associative reduction function, used to reduce values matching on the same bucket
       * @return a new HashMap with the resulting, reduced key-value pairs
       */
      infix ("groupByReduce") ((T ==> K,T ==> V,(V,V) ==> V) :: MHashMap(K,V), TNumeric(V), addTpePars = (K,V)) implements groupByReduce((T,K,V), 0, ${e => $1(e)}, ${e => $2(e)}, ${numeric_zero[V]}, ${(a,b) => $3(a,b)})

      // misc
      // will print out of order in parallel, but hey
      /** A bad implementation of pretty print
       * (Prints out of order when used in multithreaded contexts)
       */
      infix ("pprint") (Nil :: MUnit, effect = simple) implements foreach(T, 0, ${a => println(a)})


      // parallel collectionification
      // This enables a tpe to be passed in as the collection type of a Delite op

      // by convention, the return tpe of alloc must be its last tpe parameter, if it has any
      compiler ("vector_raw_alloc") (MInt :: Vector(R), addTpePars = R, effect = mutable) implements composite ${
        Vector[R]($1)
      }
      compiler ("vector_appendable") ((MInt,T) :: MBoolean) implements single("true")
      compiler ("vector_copy") ((MInt,Vector(T),MInt,MInt) :: MUnit, effect = write(2)) implements single ${
        val src = vector_raw_data($self)
        val dest = vector_raw_data($2)
        array_copy(src, $1, dest, $3, $4)
      }

      parallelize as ParallelCollectionBuffer(T, lookupOp("vector_raw_alloc"), lookupOp("length"), lookupOverloaded("apply",1), lookupOp("update"), lookupOp("vector_set_length"), lookupOp("vector_appendable"), lookupOp("append"), lookupOp("vector_copy"))
    }

    /**
     * Testing: some codegen op that takes a block with arguments
     * @param a: No clue
     * @param b: Some integer
     * @param c: No idea
     * @param d: Not sure
     * @param e: Some double
     * @param f: The function to be applied on e
     * @return definitely something probably
     **/
    val z = direct (Vector) ("foo", T, List(MInt ==> T, MInt, MThunk(MInt), (MInt,MInt) ==> MInt, MDouble, MDouble ==> MDouble) :: T) implements codegen($cala, ${
      var i = 0
      val a = new Array[$t[T]](100)

      while (i < 100) {
        a(i) = $b[0]($b[3]($b[2],$b[2]))
        a(i) = $b[0]($b[3]($1,$1))
        i += 1
      }
      println("a(5) = " + a(5))
      val z = $b[5]($b[4])
      val y = $b[2]+$1
      println("z = " + z)
      println("y = " + y)
      a(5)
    })

    /* Test for C++ supporting a block with a return value to be input of other blocks.
       Currently 'foo' above cannot be generated for C++ target because the outer-most block has return (e.g., $b[0]).
       TODO: enable the outermost blocks with a return.
    */
    val foo2 = direct (Vector) ("foo2", T, List(MInt ==> MUnit, MInt, MThunk(MInt), (MInt,T) ==> MInt) :: MUnit)

    impl (foo2) (codegen($cala, ${
      var i = 0
      val a = new Array[$t[T]](5)

      while (i < 5) {
        $b[0]($b[3]($1,a(i)))
        i += 1
      }
      println("i = " + i)
    }))

    impl (foo2) (codegen(cpp, ${
      int i = 0;
      $t[T] *a = new $t[T]();
      while (i < 5) {
        $b[0]($b[3]($1,a[i]));
        i += 1;
      }
      delete[] a;
      std::cout << "i = " << i << std::endl;
    }))

    ()
  }
}


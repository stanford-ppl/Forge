package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLFIFOs {
  this: DHDLDSL =>


  // TODO: Writing/reading at specific addresses from FIFO?
  def importFIFO() {
    val T = tpePar("T")
    val FIFO         = lookupTpe("FIFO")
    val BRAM         = lookupTpe("BRAM")
    val Tile         = lookupTpe("Tile")
    val SparseTile   = lookupTpe("SparseTile")
    val Indices      = lookupTpe("Indices")
    val Range        = lookupTpe("Range")
    val MVector      = lookupTpe("Vector")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")
    val Bit          = lookupTpe("Bit")

    // --- Nodes
    val fifo_new = internal (FIFO) ("fifo_new", T, (("size", Idx), ("zero", T)) :: FIFO(T), effect = mutable)
    val fifo_load = internal (FIFO) ("fifo_load", T, (("fifo", FIFO(T)), ("addr", Idx)) :: T, aliasHint = aliases(Nil))
    val fifo_store = internal (FIFO) ("fifo_store", T, (("fifo", FIFO(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))

    val fifo_push = internal (FIFO) ("push_fifo", T, (("fifo", FIFO(T)), ("value",T), ("en", Bit)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val fifo_pop  = internal (FIFO) ("pop_fifo", T, ("fifo", FIFO(T)) :: T, aliasHint = aliases(Nil))
    val fifo_count = internal (FIFO) ("count_fifo", T, ("fifo", FIFO(T)) :: Idx, aliasHint = aliases(Nil))

    // --- API
    /** Creates a FIFO with given size. Size must be a statically known signed integer (constant or parameter).
     * @param size
     **/
    static (FIFO) ("apply", T, (Idx) :: FIFO(T), TNum(T)) implements composite ${
      if (!isStaticSize($0)) stageError("Only constants and DSE parameters are allowed as size of FIFO")
      val fifo = fifo_new[T]($0, zero[T])
      dimsOf(fifo) = List($0)
      fifo
    }

    val FIFO_API = withTpe(FIFO)
    FIFO_API {
      /** Creates a push (write) port to this FIFO
       * @param value: the value to be pushed into the FIFO
       **/
      infix ("push") (T :: MUnit, effect = write(0)) implements composite ${ push_fifo($self, $1, true.asBit) }

      /** Creates a push (write) port to this FIFO with a write enable
       * @param value: the value to be pushed into the FIFO
       * @param wren: write enable
       **/
      infix ("push") ((T, Bit) :: MUnit, effect = write(0)) implements composite ${ push_fifo($self, $1, $2) }

      /** Creates a pop (read) port to this FIFO **/
      infix ("pop") (Nil :: T) implements composite ${ pop_fifo($self) }

      infix ("count") (Nil :: Idx) implements composite ${ count_fifo($self) }
    }

    // --- Scala Backend
    impl (fifo_new)   (codegen($cala, ${ scala.collection.mutable.Stack.fill(0.toInt)($zero) }))
    impl (fifo_load)  (codegen($cala, ${ $fifo.apply($addr.toInt) }))
    impl (fifo_store) (codegen($cala, ${ $fifo.update($addr.toInt, $value) }))
    impl (fifo_push)  (codegen($cala, ${ if ($en) $fifo.push($value); () }))
    impl (fifo_pop)   (codegen($cala, ${ $fifo.pop() }))
    impl (fifo_count) (codegen($cala, ${ FixedPoint[Signed,B32,B0]($fifo.length) }))


    // --- Unrolled nodes
    val push = internal (FIFO) ("par_push_fifo", T, (("fifo", FIFO(T)), ("value",MVector(T)), ("en", MVector(Bit)), ("shuffle", SBoolean)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    val pop  = internal (FIFO) ("par_pop_fifo", T, (("fifo", FIFO(T)), ("len", SInt)) :: MVector(T), aliasHint = aliases(Nil))

    impl (push) (codegen($cala, ${
      $value.zip($en).foreach{ case (v,e) => if (e) $fifo.push(v) }
    }))

    impl (pop) (codegen($cala, ${
      $fifo.slice(0, $len).toArray ++ new Array[$t[T]]($len - $fifo.length)
    }))

  }
}

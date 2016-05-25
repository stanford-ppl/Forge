package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLCaches {
  this: DHDLDSL =>

  def importCache() {
    val T = tpePar("T")
    val Cache   = lookupTpe("Cache")
    val Tile    = lookupTpe("Tile")
    val OffChip = lookupTpe("OffChipMem")
    val Idx     = lookupAlias("Index")
    val Indices = lookupTpe("Indices")

    // --- Nodes
    val cache_new = internal (Cache) ("cache_new", T, ("offchip", OffChip(T)) :: Cache(T), effect = mutable)
    val cache_load = internal (Cache) ("cache_load", T, (("cache", Cache(T)), ("addr", Idx)) :: T)
    val cache_store = internal (Cache) ("cache_store", T, (("cache", Cache(T)), ("addr", Idx), ("value", T)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))
    //TODO: cache_flush?

    // --- Internals
    internal (Cache) ("cache_create", T, (SOption(SString), OffChip(T)) :: Cache(T)) implements composite ${
      val cache = cache_new[T]($1)
      $0.foreach{name => nameOf(cache) = name }
      dimsOf(cache) = dimsOf($1)
      cache
    }

    /** @nodoc **/
    direct (Cache) ("cache_load_nd", T, (Cache(T), SList(Idx)) :: T) implements composite ${
      val addr = calcAddress($1, dimsOf($0))
      cache_load($0, addr)
    }
    /** @nodoc **/
    direct (Cache) ("cache_store_nd", T, (Cache(T), SList(Idx), T) :: MUnit, effect = write(0)) implements composite ${
      val addr = calcAddress($1, dimsOf($0))
      cache_store($0, addr, $2)
    }

    /** @nodoc **/
    direct (Cache) ("cache_calc_addr", T, (Cache(T), Indices) :: Idx) implements composite ${ calcAddress($1.toList, dimsOf($0)) }

    val Mem = lookupTpeClass("Mem").get
    val CacheMem = tpeClassInst("CacheMem", T, TMem(T, Cache(T)))
    infix (CacheMem) ("ld", T, (Cache(T), Idx) :: T) implements composite ${ cache_load_nd($0, List($1)) }
    infix (CacheMem) ("st", T, (Cache(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($0, List($1), $2) }
    infix (CacheMem) ("flatIdx", T, (Cache(T), Indices) :: Idx) implements composite ${ cache_calc_addr($0, $1) }

    // --- API
    /** Creates a Cache with given name and target OffChipMem. Dimensions is inherited from
     *  OffChipMem
     * @param name
     * @param offchip
     **/
    static (Cache) ("apply", T, (SString, OffChip(T)) :: Cache(T), TNum(T)) implements composite ${ cache_create(Some($0), $1) }

    /** Creates a unnamed Cache with target OffChipMem. Dimensions is inherited from
     *  OffChipMem
     * @param name
     * @param offchip
     **/
    static (Cache) ("apply", T, OffChip(T) :: Cache(T), TNum(T)) implements composite ${ cache_create(None, $0) }

    val Cache_API = withTpe(Cache)
    Cache_API {
      /* Load */
      /** Creates a read from this Cache at the given multi-dimensional address. Number of indices given can either be 1 or the
       * same as the number of dimensions that the cached OffChipMem was declared with.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param ii: multi-dimensional address
       **/
      infix ("apply") ((Idx, varArgs(Idx)) :: T) implements composite ${ cache_load_nd($self, $1 +: $2.toList) }

      /* Store */
      /** Creates a write to this Cache at the given 1D address.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: 1D address
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1), $2) }
      /** Creates a write to this Cache at the given 2D address. The cached OffChipMem must have initially been declared as 2D.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: row index
       * @param j: column index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1, $2), $3) }
      /** Creates a write to this Cache at the given 3D address. The cached OffChipMem must have initially been declared as 3D.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param i: row index
       * @param j: column index
       * @param k: page index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((Idx, Idx, Idx, T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, List($1, $2, $3), $4) }
      /** Creates a write to this Cache at the given multi-dimensional address. The number of indices given can either be 1 or the
       * same as the number of dimensions that the cached OffChipMem was declared with.
       * During a miss, the innermost pipeline that contains current load will be stalled until data
       * is loaded from offchip
       * @param ii: multi-dimensional index
       * @param x: element to be stored to Cache
       **/
      infix ("update") ((SSeq(Idx), T) :: MUnit, effect = write(0)) implements composite ${ cache_store_nd($self, $1.toList, $2) }

    }

    // --- Scala Backend
    impl (cache_new)   (codegen($cala, ${ $offchip }))
    impl (cache_load)  (codegen($cala, ${ $cache.apply($addr.toInt) }))
    impl (cache_store) (codegen($cala, ${ $cache.update($addr.toInt, $value) }))

    // --- Dot Backend
    impl (cache_new)   (codegen(dot, ${
      @ if (isDblBuf(sym)) {
        $sym [margin=0 rankdir="LR" label="{<st> | <ld>}" xlabel="$sym "
              shape="record" color=$dblbufBorderColor  style="filled"
              fillcolor=$cacheFillColor ]
      @ } else {
          $sym [label="$sym " shape="square" style="filled" fillcolor=$cacheFillColor ]
      @ }
      $offchip -> $sym
    }))
    impl (cache_load)  (codegen(dot, ${
      $addr -> $cache [ headlabel="addr" ]
      @ emitValDef(sym, cache)
    }))
    impl (cache_store) (codegen(dot, ${
      $addr -> $cache [ headlabel="addr" ]
      $value -> $cache [ headlabel="data" ]
    }))

  }
}

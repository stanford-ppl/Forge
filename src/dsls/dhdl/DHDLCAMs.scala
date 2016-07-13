package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLCAMs {
  this: DHDLDSL =>

  def importCAM() {
    val K = tpePar("K")
    val V = tpePar("V")
    val CAM = lookupTpe("CAM")
    val Idx = lookupAlias("Index")

    // --- Nodes
    val cam_new = internal (CAM) ("cam_new", (K,V), (("size", Idx), ("zero", V)) :: CAM(K,V), effect = mutable)
    val cam_load = internal (CAM) ("cam_load", (K,V), (("cam", CAM(K,V)), ("key", K)) :: V, aliasHint = aliases(Nil))
    val cam_store = internal (CAM) ("cam_store", (K,V), (("cam", CAM(K,V)), ("key", K), ("value", V)) :: MUnit, effect = write(0), aliasHint = aliases(Nil))

    // --- API
    /** Creates a CAM with given size. Size must be a statically known signed integer (constant or parameter).
     * @param size
     **/
    static (CAM) ("apply", (K,V), (Idx) :: CAM(K,V), (TNum(K),TNum(V)) ) implements composite ${
      if (!isStaticSize($0)) stageError("Only constants and DSE parameters are allowed as size of CAM")
      val cam = cam_new[K,V]($0, zero[V])
      dimsOf(cam) = List($0)
      cam
    }

    val CAM_API = withTpe(CAM)
    CAM_API {
      /** Creates a key-value store into this CAM
       * @param key: the key to be used to address this value
       * @param value: the value to be stored
       **/
      infix ("update") ((K,V) :: MUnit, effect = write(0)) implements composite ${ cam_store($self, $1, $2) }

      /** Creates a key-value read for this CAM
       * @param key
       **/
      infix ("apply") ((K) :: V) implements composite ${ cam_load($self, $1) }
    }

    // --- Scala Backend
    impl (cam_new)   (codegen($cala, ${ new scala.collection.mutable.HashMap[$t[K],$t[V]]() }))
    impl (cam_load)  (codegen($cala, ${ $cam.apply($key) }))
    impl (cam_store) (codegen($cala, ${ $cam.update($key, $value) }))

    // Unrolling is not yet supported for CAMs
  }
}

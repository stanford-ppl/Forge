package ppl.dsl.forge
package dsls
package dadl
/*
trait ArchOps {
  this: DADLDSL =>
  def importDADLArchOps() = {

    val ArchOps = grp("ArchOps")

    val T = tpePar("T")

    val BRAM = tpe("BRAM", tpePar("T"))
    data(BRAM, ("_name", MString),
      ("_banks", MInt),
      ("_data", MArray(T)))

    // To instantiate BRAM, support:
    // BRAM(name: String, banks: Int, wordWidth: Int, size: Int)
    static (BRAM) ("apply", T,
      MethodSignature(List( ("name", MString, "unit(\"\")"),
                            ("banks", MInt),
                            ("size", MInt)
                          ), MString),
      effect = mutable) implements
    codegen ($cala, ${"codegen for BRAM( " + $name + ", " + $banks + "," + $size +")"})

    val BRAMOps = withTpe(BRAM)
    BRAMOps {
      infix ("name") (Nil :: MString) implements getter(0, "_name")
      infix ("banks") (Nil :: MInt) implements getter(0, "_banks")
      infix ("data") (Nil :: MArray(T)) implements getter(0, "_data")
    }
  }
}
*/

package ppl.dsl.forge
package dsls
package dhdl

trait TupleJunk {
  this: DHDLDSL =>

  def importTupleTypeClassInstances() {
    val Coll = lookupTpeClass("Coll").get
    val Num  = lookupTpeClass("Num").get
    val Arith = lookupTpeClass("Arith").get
    val Order = lookupTpeClass("Order").get

    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString)).toList
      val parsWithIndex = (1 to arity).map{i => (pars(i-1), i)}

      // pack(( implicitly[TypeClass[A]].op, ... ))
      def tupFromStr(tc: String, op: String) = "pack((" + pars.map(p => "implicitly["+tc+"["+p.name+"]]."+op).mkString(",") + "))"
      // pack(( implicitly[TypeClass[A]].op(t._1), ... ))
      def tupFromSrcStr(tc: String, op: String) = "pack((" + parsWithIndex.map{case (p,i) => "implicitly["+tc+"["+p.name+"]]."+op+"(t._"+i+")"}.mkString(",") + "))"
      // pack(( implicitly[TypeClass[A]].op(t1._1,t2._1), ... ))
      def tupFromBinSrcStr(tc: String, op: String) = "pack((" + parsWithIndex.map{case (p,i) => "implicitly["+tc+"["+p.name+"]]."+op+"(t1._"+i+",t2._"+i+")"}.mkString(",") + "))"

      val collPars = pars.map(_ withBound TColl)
      val TupColl = tpeClassInst("CollTup"+arity, collPars, Coll(Tup))
      infix (TupColl) ("empty", collPars, Nil :: Tup) implements composite { tupFromStr("Coll","empty") }

      val numPars = pars.map(_ withBound TNum)
      val TupNum = tpeClassInst("NumTup"+arity, numPars, Num(Tup))
      infix (TupNum) ("zero", numPars, Nil :: Tup) implements composite { tupFromStr("Num", "zero") }

      val arithPars = pars.map(_ withBound TArith)
      val TupArith = tpeClassInst("ArithTup"+arity, arithPars, Arith(Tup))
      infix (TupArith) ("plus", arithPars, (("t1",Tup),("t2",Tup)) :: Tup) implements composite { tupFromBinSrcStr("Arith", "plus") }
      infix (TupArith) ("minus", arithPars, (("t1",Tup),("t2",Tup)) :: Tup) implements composite { tupFromBinSrcStr("Arith", "minus") }
      infix (TupArith) ("times", arithPars, (("t1",Tup),("t2",Tup)) :: Tup) implements composite { tupFromBinSrcStr("Arith", "times") }
      infix (TupArith) ("divide", arithPars, (("t1",Tup),("t2",Tup)) :: Tup) implements composite { tupFromBinSrcStr("Arith", "divide") }

      // Have to define ordering explicitly (no good default to assume here)
    }
  }
}

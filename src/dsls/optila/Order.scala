package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/* Somewhat of a duplicate of TOrdering, but allows use of implicitly[Order[T]] on Reps */
trait OrderOps {
  this: OptiLADSL =>

  object TOrder extends TypeClassSignature {
    def name = "Order"
    def prefix = "_or"
    def wrapper = Some("ortype")
  }

  def importOrderOps() {
    val T = tpePar("T")
    val A = tpePar("A")
    val B = tpePar("B")

    val Order = tpeClass("Order", TOrder, T)
    val BasicOrder = grp("BasicOrder")

    // Order type class interface
    infix (Order) ("gt", T, (T,T) :: MBoolean)
    infix (Order) ("gteq", T, (T,T) :: MBoolean)
    infix (Order) ("lt", T, (T,T) :: MBoolean)
    infix (Order) ("lteq", T, (T,T) :: MBoolean)
    infix (Order) ("max", T, (T,T) :: T)
    infix (Order) ("min", T, (T,T) :: T)
    infix (Order) ("compare", T, (T, T) :: MInt)

    // Basic functions (moved from BasicMath)
    direct (BasicOrder) ("min", T, (T,T) :: T, TOrder(T)) implements composite ${ implicitly[Order[T]].min($0, $1) }
    direct (BasicOrder) ("max", T, (T,T) :: T, TOrder(T)) implements composite ${ implicitly[Order[T]].max($0, $1) }
    direct (BasicOrder) ("compare", T, (T,T) :: MInt, TOrder(T)) implements composite ${ implicitly[Order[T]].compare($0, $1) }

    // primitive implementations
    for (Num <- List(MDouble, MFloat, MLong, MInt)) {
      val NumOrder = tpeClassInst("Order" + Num.name, Nil, Order(Num))
      infix (NumOrder) ("gt", Nil, (Num,Num) :: MBoolean) implements composite ${ $0 > $1 } 
      infix (NumOrder) ("gteq", Nil, (Num,Num) :: MBoolean) implements composite ${ $0 >= $1 }
      infix (NumOrder) ("lt", Nil, (Num,Num) :: MBoolean) implements composite ${ $0 < $1 }
      infix (NumOrder) ("lteq", Nil, (Num,Num) :: MBoolean) implements composite ${ $0 <= $1 }
      infix (NumOrder) ("max", Nil, (Num,Num) :: Num) implements composite ${ Math.max($0, $1) }
      infix (NumOrder) ("min", Nil, (Num,Num) :: Num) implements composite ${ Math.min($0, $1) }
    	infix (NumOrder) ("compare", Nil, (Num, Num) :: MInt) implements composite ${ $0 compare $1 }
    }

    // tuples of orders
    /*for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TOrder).toList

      def tupOrderStr(op: String) = "pack((" + pars.zipWithIndex.map(p => "implicitly[Order["+p._1.name+"]]."+op+"(t1._"+(p._2+1)+",t2._"+(p._2+1)+")").mkString(",") + "))"

      infix (BasicOrder) (">", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite   { tupOrderStr("gt") }
      infix (BasicOrder) (">=", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupOrderStr("gteq") }
      infix (BasicOrder) ("<", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite   { tupOrderStr("lt") }
      infix (BasicOrder) ("<=", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupOrderStr("lteq") }
      direct (BasicOrder) ("max", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupOrderStr("max") }
      direct (BasicOrder) ("min", pars, (("t1",Tup), ("t2",Tup)) :: Tup) implements composite { tupOrderStr("min") }
    }*/

  }
}
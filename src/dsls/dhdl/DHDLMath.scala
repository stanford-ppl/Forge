package ppl.dsl.forge
package dsls
package dhdl

trait DHDLMath {
  this: DHDLDSL =>

  object TArith extends TypeClassSignature {
    def name = "Arith"
    def prefix = "_a"
    def wrapper = Some("atype")
  }

  object TNum extends TypeClassSignature {
    def name = "Num"
    val prefix = "_nn"
    def wrapper = Some("nntype")
  }

  object TOrder extends TypeClassSignature {
    def name = "Order"
    val prefix = "_oo"
    def wrapper = Some("ootype")
  }

  object TColl extends TypeClassSignature {
    def name = "Coll"
    val prefix = "_cc"
    def wrapper = Some("cctype")
  }

  def importDHDLMath() {
    importCollectionOps()
    importNumOps()
    importArithOps()
    importOrderOps()
    importPrimitiveMath()
    importBasicMath()
    importBasicControl()
    importLiftingMath()
  }

  // --- Reduceable Type Class
  def importCollectionOps() {
    val T = tpePar("T")
    val Coll = tpeClass("Coll", TColl, T)
    infix (Coll) ("empty", T, Nil :: T)
    infix (Coll) ("zeros", T, T :: T)
  }


  // --- Num Type Class
  // TODO: Replace with instances of existing Scala Numeric type class?
  def importNumOps() {
    val T = tpePar("T")
    val Num = tpeClass("Num", TNum, T)
    infix (Num) ("zero", T, Nil :: T)

    val Prim = grp("DHDLPrim")
    direct (Prim) ("zero", T, Nil :: T, TNum(T)) implements redirect ${ implicitly[Num[T]].zero }
  }

  // --- Arith Type Class
  def importArithOps() {
    val T = tpePar("T")
    val Arith = tpeClass("Arith", TArith, T)
    infix (Arith) ("add", T, (T,T) :: T)
    infix (Arith) ("sub", T, (T,T) :: T)
    infix (Arith) ("mul", T, (T,T) :: T)
    infix (Arith) ("div", T, (T,T) :: T)
  }

  // --- Order Type Class
  // TODO: Replace with instances of existing Scala Ordering type class?
  // TODO: Infix == may not work on template types since == is already defined on all classes in Scala.
  def importOrderOps() {
    val T = tpePar("T")
    val Bit = lookupTpe("Bit")
    val Order = tpeClass("Order", TOrder, T)
    infix (Order) ("lt", T, (T,T) :: Bit)
    infix (Order) ("gt", T, (T,T) :: Bit)
    infix (Order) ("leq", T, (T,T) :: Bit)
    infix (Order) ("geq", T, (T,T) :: Bit)
    infix (Order) ("neq", T, (T,T) :: Bit)
    infix (Order) ("eql", T, (T,T) :: Bit)
  }

  def importPrimitiveMath() {
    val Coll = lookupTpeClass("Coll").get
    val Num = lookupTpeClass("Num").get
    val Arith = lookupTpeClass("Arith").get
    val Order = lookupTpeClass("Order").get

    val Prim = grp("DHDLPrim")
    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Bit   = lookupTpe("Bit")

    val Z = lookupTpe("B0",compile)

    // --- Nodes
    // Unlike Scala lib, we represent unary negation using a node since directly implementing
    // negation in hardware is simpler than figuring out that we're multiplying by -1.
    // NOTE: Modulus is only supported on integer types for now
    // NOTE: Right shift is arithmetic shift, not logical
    val neg_fix = direct (Prim) ("neg_fix", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F))
    val add_fix = direct (Prim) ("add_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val sub_fix = direct (Prim) ("sub_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val mul_fix = direct (Prim) ("mul_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val div_fix = direct (Prim) ("div_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val mod_fix = direct (Prim) ("mod_fix", (S,I),   (FixPt(S,I,Z), FixPt(S,I,Z)) :: FixPt(S,I,Z))
    val lt_fix  = direct (Prim) ("lt_fix",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    val leq_fix = direct (Prim) ("leq_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    val neq_fix = direct (Prim) ("neq_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    val eql_fix = direct (Prim) ("eql_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    val and_fix = direct (Prim) ("and_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val or_fix  = direct (Prim) ("or_fix",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    val lsh_fix = direct (Prim) ("lsh_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F))
    val rsh_fix = direct (Prim) ("rsh_fix", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F))


    val neg_flt = direct (Prim) ("neg_flt", (G,E), FltPt(G,E) :: FltPt(G,E))
    val add_flt = direct (Prim) ("add_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    val sub_flt = direct (Prim) ("sub_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    val mul_flt = direct (Prim) ("mul_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    val div_flt = direct (Prim) ("div_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    val lt_flt  = direct (Prim) ("lt_flt",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    val leq_flt = direct (Prim) ("leq_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    val neq_flt = direct (Prim) ("neq_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    val eql_flt = direct (Prim) ("eql_flt", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)


    val not_bit = direct (Prim) ("not_bit", Nil, Bit :: Bit)
    val and_bit = direct (Prim) ("and_bit", Nil, (Bit, Bit) :: Bit)
    val or_bit  = direct (Prim) ("or_bit" , Nil, (Bit, Bit) :: Bit)
    val xor_bit = direct (Prim) ("xor_bit", Nil, (Bit, Bit) :: Bit)   // aka !=
    val xnor_bit = direct (Prim) ("xnor_bit", Nil, (Bit, Bit) :: Bit) // aka ==


    // --- Type class instances
    // TODO: What should the precision of zero be? Should it take precision as an argument?
    val FixPtColl = tpeClassInst("FixPtColl", (S,I,F), Coll(FixPt(S,I,F)))
    infix (FixPtColl) ("empty", (S,I,F), Nil          :: FixPt(S,I,F)) implements composite ${ 0.as[FixPt[S,I,F]] }
    infix (FixPtColl) ("zeros", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F)) implements composite ${ 0.as[FixPt[S,I,F]] }

    val FixPtNum = tpeClassInst("FixPtNum", (S,I,F), Num(FixPt(S,I,F)))
    infix (FixPtNum) ("zero", (S,I,F), Nil :: FixPt(S,I,F)) implements composite ${ 0.as[FixPt[S,I,F]] }

    val FixPtArith = tpeClassInst("FixPtArith", (S,I,F), Arith(FixPt(S,I,F)))
    infix (FixPtArith) ("add", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ add_fix($0, $1) }
    infix (FixPtArith) ("sub", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ sub_fix($0, $1) }
    infix (FixPtArith) ("mul", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ mul_fix($0, $1) }
    infix (FixPtArith) ("div", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ div_fix($0, $1) }

    val FixPtOrder = tpeClassInst("FixPtOrder", (S,I,F), Order(FixPt(S,I,F)))
    infix (FixPtOrder) ("lt",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ lt_fix($0, $1) }
    infix (FixPtOrder) ("gt",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ lt_fix($1, $0) }
    infix (FixPtOrder) ("leq", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ leq_fix($0, $1) }
    infix (FixPtOrder) ("geq", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ leq_fix($1, $0) }
    infix (FixPtOrder) ("neq", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ neq_fix($0, $1) }
    infix (FixPtOrder) ("eql", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ eql_fix($0, $1) }


    val FltPtColl = tpeClassInst("FltPtColl", (G,E), Coll(FltPt(G,E)))
    infix (FltPtColl) ("empty", (G,E), Nil        :: FltPt(G,E)) implements composite ${ 0.as[FltPt[G,E]] }
    infix (FltPtColl) ("zeros", (G,E), FltPt(G,E) :: FltPt(G,E)) implements composite ${ 0.as[FltPt[G,E]] }

    val FltPtNum = tpeClassInst("FltPtNum", (G,E), Num(FltPt(G,E)))
    infix (FltPtNum) ("zero", (G,E), Nil :: FltPt(G,E)) implements composite ${ 0.as[FltPt[G,E]] }

    val FltPtArith = tpeClassInst("FltPtArith", (G,E), Arith(FltPt(G,E)))
    infix (FltPtArith) ("add", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ add_flt($0, $1) }
    infix (FltPtArith) ("sub", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ sub_flt($0, $1) }
    infix (FltPtArith) ("mul", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ mul_flt($0, $1) }
    infix (FltPtArith) ("div", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ div_flt($0, $1) }

    val FltPtOrder = tpeClassInst("FltPtOrder", (G,E), Order(FltPt(G,E)))
    infix (FltPtOrder) ("lt",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ lt_flt($0, $1) }
    infix (FltPtOrder) ("gt",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ lt_flt($1, $0) }
    infix (FltPtOrder) ("leq", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ leq_flt($0, $1) }
    infix (FltPtOrder) ("geq", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ leq_flt($1, $0) }
    infix (FltPtOrder) ("neq", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ neq_flt($0, $1) }
    infix (FltPtOrder) ("eql", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ eql_flt($0, $1) }


    val BitColl = tpeClassInst("BitColl", Nil, Coll(Bit))
    infix (BitColl) ("empty", Nil, Nil :: Bit) implements composite ${ false.toBit }
    infix (BitColl) ("zeros", Nil, Bit :: Bit) implements composite ${ false.toBit }

    val BitNum = tpeClassInst("BitNum", Nil, Num(Bit))
    infix (BitNum) ("zero", Nil, Nil :: Bit) implements composite ${ false.toBit }

    // TODO: Is this needed?
    val BitOrder = tpeClassInst("BitOrder", Nil, Order(Bit))
    infix (BitOrder) ("lt",  Nil, (Bit, Bit) :: Bit) implements composite ${ !$0 && $1 }
    infix (BitOrder) ("gt",  Nil, (Bit, Bit) :: Bit) implements composite ${ !$1 && $0 }
    infix (BitOrder) ("leq", Nil, (Bit, Bit) :: Bit) implements composite ${ !$0 || $1 }
    infix (BitOrder) ("geq", Nil, (Bit, Bit) :: Bit) implements composite ${ !$1 || $0 }
    infix (BitOrder) ("neq", Nil, (Bit, Bit) :: Bit) implements composite ${ xor_bit($0, $1) }
    infix (BitOrder) ("eql", Nil, (Bit, Bit) :: Bit) implements composite ${ xnor_bit($0, $1) }


    // --- API
    // Fixed Point
    infix (Prim) ("unary_-", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F))  implements redirect ${ neg_fix($0) }
    infix (Prim) ("+",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ add_fix($0, $1) }
    infix (Prim) ("-",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ sub_fix($0, $1) }
    infix (Prim) ("*",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ mul_fix($0, $1) }
    infix (Prim) ("/",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ div_fix($0, $1) }
    infix (Prim) ("%",  (S,I,F), (FixPt(S,I,Z), FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ mod_fix($0, $1) }

    infix (Prim) ("<",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ lt_fix($0, $1) }
    infix (Prim) (">",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ lt_fix($1, $0) }
    infix (Prim) ("<=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ leq_fix($0, $1) }
    infix (Prim) (">=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ leq_fix($1, $0) }
    infix (Prim) ("!=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ neq_fix($0, $1) }
    direct (Prim) ("__equal", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ eql_fix($0, $1) }

    infix (Prim) ("&", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ and_fix($0, $1) }
    infix (Prim) ("|", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ or_fix($0, $1) }

    infix (Prim) ("<<", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F)) implements redirect ${ lsh_fix($0, $1) }
    infix (Prim) (">>", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F)) implements redirect ${ rsh_fix($0, $1) }


    // Floating Point
    infix (Prim) ("unary_-", (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ neg_flt($0) }
    infix (Prim) ("+", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ add_flt($0, $1) }
    infix (Prim) ("-", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ sub_flt($0, $1) }
    infix (Prim) ("*", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ mul_flt($0, $1) }
    infix (Prim) ("/", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ div_flt($0, $1) }

    infix (Prim) ("<", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ lt_flt($0, $1) }
    infix (Prim) (">", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ lt_flt($1, $0) }
    infix (Prim) ("<=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ leq_flt($0, $1) }
    infix (Prim) (">=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ leq_flt($1, $0) }
    infix (Prim) ("!=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ neq_flt($0, $1) }
    direct (Prim) ("__equal", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ eql_flt($0, $1) }


    // Bit
    infix (Prim) ("unary_!", Nil, Bit :: Bit) implements redirect ${ not_bit($0) }
    infix (Prim) ("&&", Nil, (Bit, Bit) :: Bit) implements redirect ${ and_bit($0, $1) }
    infix (Prim) ("||", Nil, (Bit, Bit) :: Bit) implements redirect ${ or_bit($0, $1) }
    infix (Prim) ("!=", Nil, (Bit, Bit) :: Bit) implements redirect ${ xor_bit($0, $1) }
    infix (Prim) ("^",  Nil, (Bit, Bit) :: Bit) implements redirect ${ xor_bit($0, $1) }
    direct (Prim) ("__equal", Nil, (Bit, Bit) :: Bit) implements redirect ${ xnor_bit($0, $1) }


    // --- Scala Backend
    impl (neg_fix) (codegen($cala, ${ -$0 }))
    impl (add_fix) (codegen($cala, ${ $0 + $1 }))
    impl (sub_fix) (codegen($cala, ${ $0 - $1 }))
    impl (mul_fix) (codegen($cala, ${ $0 * $1 }))
    impl (div_fix) (codegen($cala, ${ $0 / $1 }))
    impl (mod_fix) (codegen($cala, ${ $0 % $1 }))
    impl (lt_fix)  (codegen($cala, ${ $0 < $1 }))
    impl (leq_fix) (codegen($cala, ${ $0 <= $1 }))
    impl (neq_fix) (codegen($cala, ${ $0 != $1 }))
    impl (eql_fix) (codegen($cala, ${ $0 == $1 }))
    impl (and_fix) (codegen($cala, ${ $0 & $1 }))
    impl (or_fix)  (codegen($cala, ${ $0 | $1 }))
    impl (lsh_fix) (codegen($cala, ${ $0 << $1 }))
    impl (rsh_fix) (codegen($cala, ${ $0 >> $1 }))

    impl (neg_flt) (codegen($cala, ${ -$0 }))
    impl (add_flt) (codegen($cala, ${ $0 + $1 }))
    impl (sub_flt) (codegen($cala, ${ $0 - $1 }))
    impl (mul_flt) (codegen($cala, ${ $0 * $1 }))
    impl (div_flt) (codegen($cala, ${ $0 / $1 }))
    impl (lt_flt)  (codegen($cala, ${ $0 < $1 }))
    impl (leq_flt) (codegen($cala, ${ $0 <= $1 }))
    impl (neq_flt) (codegen($cala, ${ $0 != $1 }))
    impl (eql_flt) (codegen($cala, ${ $0 == $1 }))

    impl (not_bit) (codegen($cala, ${ !$0 }))
    impl (and_bit) (codegen($cala, ${ $0 && $1 }))
    impl (or_bit)  (codegen($cala, ${ $0 || $1 }))
    impl (xor_bit) (codegen($cala, ${ $0 != $1 }))
    impl (xnor_bit) (codegen($cala, ${ $0 == $1 }))

    // --- Dot Backend
    impl (neg_fix) (codegen(dot, ${ $sym [label="unary_-:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym }))
		impl (add_fix) (codegen(dot, ${ $sym [label="+:fix" shape="square" style="filled" fillcolor="white"]  \n $0->$sym \n $1->$sym }))
		impl (sub_fix) (codegen(dot, ${ $sym [label="-:fix" shape="square" style="filled" fillcolor="white"]  \n $0->$sym \n $1->$sym }))
		impl (mul_fix) (codegen(dot, ${ $sym [label="*:fix" shape="square" style="filled" fillcolor="white"]  \n $0->$sym \n $1->$sym }))
		impl (div_fix) (codegen(dot, ${ $sym [label="/:fix" shape="square" style="filled" fillcolor="white"]  \n $0->$sym \n $1->$sym }))
		impl (lt_fix)  (codegen(dot, ${ $sym [label="<:fix" shape="square" style="filled" fillcolor="white"]  \n $0->$sym \n $1->$sym }))
		impl (leq_fix) (codegen(dot, ${ $sym [label="<=:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (neq_fix) (codegen(dot, ${ $sym [label="!=:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (eql_fix) (codegen(dot, ${ $sym [label="==:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (and_fix) (codegen(dot, ${ $sym [label="&:fix"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (or_fix)  (codegen(dot, ${ $sym [label="|:fix"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (lsh_fix) (codegen(dot, ${ $sym [label="<<:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
		impl (rsh_fix) (codegen(dot, ${ $sym [label=">>:fix" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))

    impl (neg_flt) (codegen(dot, ${ $sym [label="unary_-:flt"] \n $0->$sym }))
    impl (add_flt) (codegen(dot, ${ $sym [label="+:flt"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (sub_flt) (codegen(dot, ${ $sym [label="-:flt"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (mul_flt) (codegen(dot, ${ $sym [label="*:flt"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (div_flt) (codegen(dot, ${ $sym [label="/:flt"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (lt_flt)  (codegen(dot, ${ $sym [label="<:flt"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (leq_flt) (codegen(dot, ${ $sym [label="<=:flt" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (neq_flt) (codegen(dot, ${ $sym [label="!=:flt" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (eql_flt) (codegen(dot, ${ $sym [label="==:flt" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))

    impl (not_bit) (codegen(dot, 	${ $sym [label="~:bit" shape="square" style="filled" fillcolor="white"] \n $0->$sym }))
    impl (and_bit) (codegen(dot, 	${ $sym [label="&:bit" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (or_bit)  (codegen(dot, 	${ $sym [label="|:bit" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (xor_bit) (codegen(dot, 	${ $sym [label="xor:bit"  shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))
    impl (xnor_bit) (codegen(dot, ${ $sym [label="xnor:bit" shape="square" style="filled" fillcolor="white"] \n $0->$sym \n $1->$sym }))

    // --- MaxJ Backend
		//TODO: maxj negation?
    impl (neg_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = 0 - $0 ;
		}))
    impl (add_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 + $1 ;
		}))
    impl (sub_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 - $1 ;
		}))
    impl (mul_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 * $1 ;
		}))
    impl (div_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 / $1 ;
		}))
    impl (lt_fix)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 < $1 ;
		}))
    impl (leq_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			//TODO: maxj leq?
    	$sym <== $0 <= $1 ;
		}))
    impl (neq_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			//TODO: maxj neq?
    	$sym <== $0 != $1 ;
		}))
    impl (eql_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 === $1 ;
		}))
    impl (and_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 & $1 ;
		}))
    impl (or_fix)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 | $1 ;
		}))
			//TODO: maxj lsh?
    impl (lsh_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 + $1 ;
		}))
			//TODO: maxj rsh?
    impl (rsh_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 + $1 ;
		}))

		//TODO: maxj negation?
    impl (neg_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = 0 - $0 ;
		}))
    impl (add_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 + $1 ;
		}))
    impl (sub_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 - $1 ;
		}))
    impl (mul_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 * $1 ;
		}))
    impl (div_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 / $1 ;
		}))
    impl (lt_flt)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 < $1 ;
		}))
    impl (leq_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			//TODO: maxj leq?
    	$sym <== $0 <= $1 ;
		}))
    impl (neq_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			//TODO: maxj neq?
    	$sym <== $0 != $1 ;
		}))
    impl (eql_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre(n) $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 === $1 ;
		}))

    impl (not_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = ~( $0 );
		}))
    impl (and_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 & $1 ;
		}))
    impl (or_bit)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 | $1 ;
		}))
		//TODO: maxj xor?
    impl (xor_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 !== $1 ;
		}))
		//TODO: maxj xnor?
    impl (xnor_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 === $1 ;
		}))
  }

  def importBasicMath() {
    val T = tpePar("T")

    val Prim = grp("DHDLPrim")
    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Bit   = lookupTpe("Bit")

    // --- Nodes
    // TODO: Support fixed point log, exp, and sqrt?
    val abs_fix = direct (Prim) ("abs_fix", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F))
    //val log_fix = direct ("log_fix", Nil, Fix :: Fix)
    //val exp_fix = direct ("exp_fix", Nil, Fix :: Fix)
    //val sqrt_fix = direct ("sqrt_fix", Nil, Fix :: Fix)

    val abs_flt = direct (Prim) ("abs_flt", (G,E), FltPt(G,E) :: FltPt(G,E))
    val log_flt = direct (Prim) ("log_flt", (G,E), FltPt(G,E) :: FltPt(G,E))
    val exp_flt = direct (Prim) ("exp_flt", (G,E), FltPt(G,E) :: FltPt(G,E))
    val sqrt_flt = direct (Prim) ("sqrt_flt", (G,E), FltPt(G,E) :: FltPt(G,E))

    // --- API
    // TODO: Support a more general pow
    direct (Prim) ("pow", T, (T, SInt) :: T, TArith(T)) implements composite ${
      if ($1 < 1) stageError("Power less than 1 is currently unsupported.")
      productTree(List.fill($1){$0})
    }

    // Fixed Point
    infix  (Prim) ("**", (S,I,F), (FixPt(S,I,F), SInt) :: FixPt(S,I,F)) implements redirect ${ pow($0, $1) }
    direct (Prim) ("abs", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F)) implements redirect ${ abs_fix($0) }   // Absolute value
    //direct (Prim) ("log", Nil, Fix :: Fix) implements redirect ${ log_fix($0) }   // Natural Logarithm
    //direct (Prim) ("exp", Nil, Fix :: Fix) implements redirect ${ exp_fix($0) }   // Exponent (e^x)
    //direct (Prim) ("sqrt", Nil, Fix :: Fix) implements redirect ${ sqrt_fix($0) } // Square root

    // Floating Point
    infix  (Prim) ("**", (G,E), (FltPt(G,E), SInt) :: FltPt(G,E)) implements redirect ${ pow($0, $1) }
    direct (Prim) ("abs",  (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ abs_flt($0) }     // Absolute value
    direct (Prim) ("log",  (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ log_flt($0) }     // Natural Logarithm
    direct (Prim) ("exp",  (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ exp_flt($0) }     // Exponent (e^x)
    direct (Prim) ("sqrt", (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ sqrt_flt($0) }   // Square root


    // --- Scala Backend
    impl (abs_fix) (codegen($cala, ${ FixedPoint.abs($0) }))
    impl (abs_flt) (codegen($cala, ${ FloatPoint.abs($0) }))
    impl (log_flt) (codegen($cala, ${ FloatPoint.log($0) }))
    impl (exp_flt) (codegen($cala, ${ FloatPoint.exp($0) }))
    impl (sqrt_flt) (codegen($cala, ${ FloatPoint.sqrt($0) }))

    // --- Dot Backend
    impl (abs_fix) (codegen(dot,  ${ $sym [label="abs:fix" shape="square" style="filled" fillcolor="white"]
			$0 -> $sym
		}))

    impl (abs_flt) (codegen(dot,  ${ $sym [label="abs:flt" shape="square" style="filled" fillcolor="white"]
			$0->$sym
		}))
    impl (log_flt) (codegen(dot,  ${ $sym [label="log:flt" shape="square" style="filled" fillcolor="white"]
			$0->$sym
		}))
    impl (exp_flt) (codegen(dot,  ${ $sym [label="exp:flt" shape="square" style="filled" fillcolor="white"]
			$0->$sym
		}))
    impl (sqrt_flt) (codegen(dot, ${ $sym [label="sqrt:flt" shape="square" style="filled" fillcolor="white"]
			$0->$sym
		}))

    // --- MaxJ Backend
    impl (abs_fix) (codegen(maxj,  ${
			//TODO: parallelized virsion
			@ val pre = maxJPre(sym)
    	$pre $sym = KernelMath.abs( $0 );
		}))

    impl (abs_flt) (codegen(maxj,  ${
			//TODO: parallelized virsion
			@ val pre = maxJPre(sym)
    	$pre $sym = KernelMath.abs( $0 );
		}))
    //impl (log_flt) (codegen(maxj,  ${
		//	//TODO: parallelized virsion
		//	@ val pre = maxJPre(sym)
    //	$pre $sym = KernelMath.log(new KernelMath.Range(-Float.MAX_VALUE, Float.MAX_VALUE), $0, $tpstr(sym) );
		//}))
    impl (exp_flt) (codegen(maxj,  ${
		}))
    impl (sqrt_flt) (codegen(maxj, ${
		}))

  }

  // TODO: Any reason for min and max to be nodes?
  def importBasicControl() {
    val T = tpePar("T")
    val CT = tpePar("CT", stage=compile)
    val Prim = grp("DHDLPrim")
    val Bit = lookupTpe("Bit")

    val mux = direct (Prim) ("mux", T, (("sel", Bit), ("a",T), ("b",T)) :: T, TNum(T))

    direct (Prim) ("mux", (T,CT), (("sel", Bit), ("a", T), ("b", CT)) :: T, (TNum(T), TNumeric(CT))) implements composite ${
      mux($sel, $a, lift_to[CT,T]($b))
    }
    direct (Prim) ("mux", (T,CT), (("sel", Bit), ("a", CT), ("b", T)) :: T, (TNum(T), TNumeric(CT))) implements composite ${
      mux($sel, lift_to[CT,T]($a), $b)
    }

    direct (Prim) ("min", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ mux(implicitly[Order[T]].lt($a, $b), a, b) }
    direct (Prim) ("max", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ mux(implicitly[Order[T]].gt($a, $b), a, b) }

    // --- Scala Backend
    impl (mux) (codegen($cala, ${ if ($sel) $a else $b }))

    // --- Dot Backend
    impl (mux) (codegen(dot, ${ $sym [label="mux", shape="diamond" style="filled" fillcolor="white"] }))

    // --- MaxJ Backend
    impl (mux) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = $sel ? $a : $b ;
		}))
	}

  // Infix operations with a Scala type on the LHS
  def importLiftingMath() {
    val Prim = grp("DHDLPrim")
    val FltPt = lookupTpe("FltPt")
    val FixPt = lookupTpe("FixPt")
    val Bit   = lookupTpe("Bit")

    val Z = lookupTpe("B0",compile)

    // TODO: RHS can only be an integer, but LHS can be a float. What to do here? Is this an important case to support? (Probaby not)
    //infix (Prim) ("<<", (T,S,I), (T, FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ lsh_fix($0, fixPt[Int,S,I,B0]($1)) }
    //infix (Prim) (">>", (T,S,I), (T, FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ rsh_fix($0, fixPt[Int,S,I,B0]($1)) }

    infix (Prim) ("<<", (S,I), (FixPt(S,I,Z), SInt) :: FixPt(S,I,Z)) implements redirect ${ lsh_fix($0, fixPt[Int,S,I,B0]($1)) }
    infix (Prim) (">>", (S,I), (FixPt(S,I,Z), SInt) :: FixPt(S,I,Z)) implements redirect ${ rsh_fix($0, fixPt[Int,S,I,B0]($1)) }


    // Have to resort to the exhaustive version for these :(
    UnstagedNumerics.foreach { (T, TT) =>
      infix (Prim) ("+",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ add_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) ("-",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ sub_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) ("*",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ mul_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) ("/",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ div_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) ("%",  (S,I,F), (T, FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ mod_fix(fixPt[\$TT,S,I,B0]($0), $1) }

      infix (Prim) ("<",  (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ lt_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) (">",  (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ lt_fix($1, fixPt[\$TT,S,I,F]($0)) }
      infix (Prim) ("<=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ leq_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) (">=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ leq_fix($1, fixPt[\$TT,S,I,F]($0)) }
      infix (Prim) ("!=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ neq_fix(fixPt[\$TT,S,I,F]($0), $1) }
      direct (Prim) ("__equal", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ eql_fix(fixPt[\$TT,S,I,F]($0), $1) }

      infix (Prim) ("&", (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ and_fix(fixPt[\$TT,S,I,F]($0), $1) }
      infix (Prim) ("|", (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ or_fix(fixPt[\$TT,S,I,F]($0), $1) }

      infix (Prim) ("+",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ add_flt(fltPt[\$TT,G,E]($0), $1) }
      infix (Prim) ("-",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ sub_flt(fltPt[\$TT,G,E]($0), $1) }
      infix (Prim) ("*",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ mul_flt(fltPt[\$TT,G,E]($0), $1) }
      infix (Prim) ("/",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ div_flt(fltPt[\$TT,G,E]($0), $1) }

      infix (Prim) ("<",  (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ lt_flt(fltPt[\$TT,G,E]($0), $1) }
      infix (Prim) (">",  (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ lt_flt($1, fltPt[\$TT,G,E]($0)) }
      infix (Prim) ("<=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ leq_flt(fltPt[\$TT,G,E]($0), $1) }
      infix (Prim) (">=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ leq_flt($1, fltPt[\$TT,G,E]($0)) }
      infix (Prim) ("!=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ neq_flt(fltPt[\$TT,G,E]($0), $1) }
      direct (Prim) ("__equal", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ eql_flt(fltPt[\$TT,G,E]($0), $1) }


      infix (Prim) ("+",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ add_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) ("-",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ sub_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) ("*",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ mul_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) ("/",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ div_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) ("%",  (S,I,F), (FixPt(S,I,Z), T) :: FixPt(S,I,Z)) implements redirect ${ mod_fix($0, fixPt[\$TT,S,I,B0]($1)) }

      infix (Prim) ("<",  (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ lt_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) (">",  (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ lt_fix(fixPt[\$TT,S,I,F]($1), $0) }
      infix (Prim) ("<=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ leq_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) (">=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ leq_fix(fixPt[\$TT,S,I,F]($1), $0) }
      infix (Prim) ("!=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ neq_fix($0, fixPt[\$TT,S,I,F]($1)) }
      direct (Prim) ("__equal", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ eql_fix($0, fixPt[\$TT,S,I,F]($1)) }

      infix (Prim) ("&", (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ and_fix($0, fixPt[\$TT,S,I,F]($1)) }
      infix (Prim) ("|", (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ or_fix($0, fixPt[\$TT,S,I,F]($1)) }

      infix (Prim) ("+",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ add_flt($0, fltPt[\$TT,G,E]($1)) }
      infix (Prim) ("-",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ sub_flt($0, fltPt[\$TT,G,E]($1)) }
      infix (Prim) ("*",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ mul_flt($0, fltPt[\$TT,G,E]($1)) }
      infix (Prim) ("/",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ div_flt($0, fltPt[\$TT,G,E]($1)) }

      infix (Prim) ("<",  (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ lt_flt($0, fltPt[\$TT,G,E]($1)) }
      infix (Prim) (">",  (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ lt_flt(fltPt[\$TT,G,E]($1), $0) }
      infix (Prim) ("<=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ leq_flt($0, fltPt[\$TT,G,E]($1)) }
      infix (Prim) (">=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ leq_flt(fltPt[\$TT,G,E]($1), $0) }
      infix (Prim) ("!=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ neq_flt($0, fltPt[\$TT,G,E]($1)) }
      direct (Prim) ("__equal", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ eql_flt($0, fltPt[\$TT,G,E]($1)) }
    }
  }
}


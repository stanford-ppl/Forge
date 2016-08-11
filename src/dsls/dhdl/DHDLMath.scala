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
  }


  // --- Num Type Class
  def importNumOps() {
    val T = tpePar("T")
    val Num = tpeClass("Num", TNum, T)
    infix (Num) ("zero", T, Nil :: T)

    val Math = grp("Math")
    direct (Math) ("zero", T, Nil :: T, TNum(T)) implements redirect ${ implicitly[Num[T]].zero }
  }

  // --- Arith Type Class
  def importArithOps() {
    val T = tpePar("T")
    val Arith = tpeClass("Arith", TArith, T)
    infix (Arith) ("plus", T, (T,T) :: T)
    infix (Arith) ("minus", T, (T,T) :: T)
    infix (Arith) ("times", T, (T,T) :: T)
    infix (Arith) ("divide", T, (T,T) :: T)
  }

  // --- Order Type Class
  // TODO: Replace with instances of existing Scala Ordering type class?
  // TODO: Infix == may not work on template types since == is already defined on all classes in Scala.
  def importOrderOps() {
    val T = tpePar("T")
    val Bit = lookupTpe("Bit")
    val Order = tpeClass("Order", TOrder, T)
    infix (Order) ("lessThan", T, (T,T) :: Bit)
    infix (Order) ("greaterThan", T, (T,T) :: Bit)
    infix (Order) ("lessThanOrEqual", T, (T,T) :: Bit)
    infix (Order) ("greaterThanOrEqual", T, (T,T) :: Bit)
    infix (Order) ("notEqual", T, (T,T) :: Bit)
    infix (Order) ("equals", T, (T,T) :: Bit)
  }

  def importPrimitiveMath() {
    val Coll = lookupTpeClass("Coll").get
    val Num = lookupTpeClass("Num").get
    val Arith = lookupTpeClass("Arith").get
    val Order = lookupTpeClass("Order").get

    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Bit   = lookupTpe("Bit")

    val Z = lookupTpe("B0",compile)

    // --- Nodes
    // Unlike Scala lib, we represent unary negation using a node since directly implementing
    // negation in hardware is simpler than figuring out that we're multiplying by -1.
    // NOTE: Modulus is only supported on integer types for now
    // NOTE: Right shift is arithmetic shift, not logical
    /** @nodoc **/
    val neg_fix = direct (FixPt) ("neg", (S,I,F), (FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val add_fix = direct (FixPt) ("add", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val sub_fix = direct (FixPt) ("sub", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val mul_fix = direct (FixPt) ("mul", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val div_fix = direct (FixPt) ("div", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val mod_fix = direct (FixPt) ("mod", (S,I),   (FixPt(S,I,Z), FixPt(S,I,Z)) :: FixPt(S,I,Z))
    /** @nodoc **/
    val lt_fix  = direct (FixPt) ("lt",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    /** @nodoc **/
    val leq_fix = direct (FixPt) ("leq", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    /** @nodoc **/
    val neq_fix = direct (FixPt) ("neq", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    /** @nodoc **/
    val eql_fix = direct (FixPt) ("eql", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit)
    /** @nodoc **/
    val and_fix = direct (FixPt) ("and", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val or_fix  = direct (FixPt) ("or",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F))
    /** @nodoc **/
    val lsh_fix = direct (FixPt) ("lsh", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F))
    /** @nodoc **/
    val rsh_fix = direct (FixPt) ("rsh", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F))


    /** @nodoc **/
    val neg_flt = direct (FltPt) ("neg", (G,E), (FltPt(G,E)) :: FltPt(G,E))
    /** @nodoc **/
    val add_flt = direct (FltPt) ("add", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    /** @nodoc **/
    val sub_flt = direct (FltPt) ("sub", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    /** @nodoc **/
    val mul_flt = direct (FltPt) ("mul", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    /** @nodoc **/
    val div_flt = direct (FltPt) ("div", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E))
    /** @nodoc **/
    val lt_flt  = direct (FltPt) ("lt",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    /** @nodoc **/
    val leq_flt = direct (FltPt) ("leq", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    /** @nodoc **/
    val neq_flt = direct (FltPt) ("neq", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)
    /** @nodoc **/
    val eql_flt = direct (FltPt) ("eql", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit)

    /** @nodoc **/
    val not_bit  = direct (Bit) ("not",  Nil, (Bit) :: Bit)
    /** @nodoc **/
    val and_bit  = direct (Bit) ("and",  Nil, (Bit, Bit) :: Bit)
    /** @nodoc **/
    val or_bit   = direct (Bit) ("or",   Nil, (Bit, Bit) :: Bit)
    /** @nodoc **/
    val xor_bit  = direct (Bit) ("xor",  Nil, (Bit, Bit) :: Bit)    // aka !=
    /** @nodoc **/
    val xnor_bit = direct (Bit) ("xnor", Nil, (Bit, Bit) :: Bit)    // aka ==


    // --- Type class instances
    val FixPtColl = tpeClassInst("FixPtColl", (S,I,F), Coll(FixPt(S,I,F)))
    infix (FixPtColl) ("empty", (S,I,F), Nil :: FixPt(S,I,F)) implements composite ${ 0.as[FixPt[S,I,F]] }

    val FixPtNum = tpeClassInst("FixPtNum", (S,I,F), Num(FixPt(S,I,F)))
    infix (FixPtNum) ("zero", (S,I,F), Nil :: FixPt(S,I,F)) implements composite ${ 0.as[FixPt[S,I,F]] }

    val FixPtArith = tpeClassInst("FixPtArith", (S,I,F), Arith(FixPt(S,I,F)))
    infix (FixPtArith) ("plus", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ add($0, $1) }
    infix (FixPtArith) ("minus", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ sub($0, $1) }
    infix (FixPtArith) ("times", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ mul($0, $1) }
    infix (FixPtArith) ("divide", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements composite ${ div($0, $1) }

    val FixPtOrder = tpeClassInst("FixPtOrder", (S,I,F), Order(FixPt(S,I,F)))
    infix (FixPtOrder) ("lessThan",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ lt($0, $1) }
    infix (FixPtOrder) ("greaterThan",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ lt($1, $0) }
    infix (FixPtOrder) ("lessThanOrEqual", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ leq($0, $1) }
    infix (FixPtOrder) ("greaterThanOrEqual", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ leq($1, $0) }
    infix (FixPtOrder) ("notEqual", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ neq($0, $1) }
    infix (FixPtOrder) ("equals", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements composite ${ eql($0, $1) }


    val FltPtColl = tpeClassInst("FltPtColl", (G,E), Coll(FltPt(G,E)))
    infix (FltPtColl) ("empty", (G,E), Nil :: FltPt(G,E)) implements composite ${ 0.as[FltPt[G,E]] }

    val FltPtNum = tpeClassInst("FltPtNum", (G,E), Num(FltPt(G,E)))
    infix (FltPtNum) ("zero", (G,E), Nil :: FltPt(G,E)) implements composite ${ 0.as[FltPt[G,E]] }

    val FltPtArith = tpeClassInst("FltPtArith", (G,E), Arith(FltPt(G,E)))
    infix (FltPtArith) ("plus", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ add($0, $1) }
    infix (FltPtArith) ("minus", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ sub($0, $1) }
    infix (FltPtArith) ("times", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ mul($0, $1) }
    infix (FltPtArith) ("divide", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements composite ${ div($0, $1) }

    val FltPtOrder = tpeClassInst("FltPtOrder", (G,E), Order(FltPt(G,E)))
    infix (FltPtOrder) ("lessThan",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ lt($0, $1) }
    infix (FltPtOrder) ("greaterThan",  (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ lt($1, $0) }
    infix (FltPtOrder) ("lessThanOrEqual", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ leq($0, $1) }
    infix (FltPtOrder) ("greaterThanOrEqual", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ leq($1, $0) }
    infix (FltPtOrder) ("notEqual", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ neq($0, $1) }
    infix (FltPtOrder) ("equals", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements composite ${ eql($0, $1) }


    val BitColl = tpeClassInst("BitColl", Nil, Coll(Bit))
    infix (BitColl) ("empty", Nil, Nil :: Bit) implements composite ${ false.asBit }

    val BitNum = tpeClassInst("BitNum", Nil, Num(Bit))
    infix (BitNum) ("zero", Nil, Nil :: Bit) implements composite ${ false.asBit }

    val BitOrder = tpeClassInst("BitOrder", Nil, Order(Bit))
    infix (BitOrder) ("lessThan",  Nil, (Bit, Bit) :: Bit) implements composite ${ !$0 && $1 }
    infix (BitOrder) ("greaterThan",  Nil, (Bit, Bit) :: Bit) implements composite ${ !$1 && $0 }
    infix (BitOrder) ("lessThanOrEqual", Nil, (Bit, Bit) :: Bit) implements composite ${ !$0 || $1 }
    infix (BitOrder) ("greaterThanOrEqual", Nil, (Bit, Bit) :: Bit) implements composite ${ !$1 || $0 }
    infix (BitOrder) ("notEqual", Nil, (Bit, Bit) :: Bit) implements composite ${ xor($0, $1) }
    infix (BitOrder) ("equals", Nil, (Bit, Bit) :: Bit) implements composite ${ xnor($0, $1) }


    // --- API
    // Fixed Point
    infix (FixPt) ("unary_-", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F))  implements redirect ${ neg($0) }
    infix (FixPt) ("+",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ add($0, $1) }
    infix (FixPt) ("-",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ sub($0, $1) }
    infix (FixPt) ("*",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ mul($0, $1) }
    infix (FixPt) ("/",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ div($0, $1) }
    infix (FixPt) ("%",  (S,I),   (FixPt(S,I,Z), FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ mod($0, $1) }

    infix (FixPt) ("<",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ lt($0, $1) }
    infix (FixPt) (">",  (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ lt($1, $0) }
    infix (FixPt) ("<=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ leq($0, $1) }
    infix (FixPt) (">=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ leq($1, $0) }
    infix (FixPt) ("!=", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ neq($0, $1) }
    direct (FixPt) ("__equal", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: Bit) implements redirect ${ eql($0, $1) }

    infix (FixPt) ("&", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ and($0, $1) }
    infix (FixPt) ("|", (S,I,F), (FixPt(S,I,F), FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ or($0, $1) }

    infix (FixPt) ("<<", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F)) implements redirect ${ lsh($0, $1) }
    /** Arithmetic shift right **/
    infix (FixPt) (">>", (S,I,F), (FixPt(S,I,F), FixPt(S,I,Z)) :: FixPt(S,I,F)) implements redirect ${ rsh($0, $1) }


    // Floating Point
    infix (FltPt) ("unary_-", (G,E), FltPt(G,E) :: FltPt(G,E)) implements redirect ${ neg($0) }
    infix (FltPt) ("+", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ add($0, $1) }
    infix (FltPt) ("-", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ sub($0, $1) }
    infix (FltPt) ("*", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ mul($0, $1) }
    infix (FltPt) ("/", (G,E), (FltPt(G,E), FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ div($0, $1) }

    infix (FltPt) ("<", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ lt($0, $1) }
    infix (FltPt) (">", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ lt($1, $0) }
    infix (FltPt) ("<=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ leq($0, $1) }
    infix (FltPt) (">=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ leq($1, $0) }
    infix (FltPt) ("!=", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ neq($0, $1) }
    direct (FltPt) ("__equal", (G,E), (FltPt(G,E), FltPt(G,E)) :: Bit) implements redirect ${ eql($0, $1) }


    // Bit
    infix (Bit) ("unary_!", Nil, Bit :: Bit) implements redirect ${ not($0) }
    infix (Bit) ("&&", Nil, (Bit, Bit) :: Bit) implements redirect ${ and($0, $1) }
    infix (Bit) ("||", Nil, (Bit, Bit) :: Bit) implements redirect ${ or($0, $1) }
    infix (Bit) ("!=", Nil, (Bit, Bit) :: Bit) implements redirect ${ xor($0, $1) }
    infix (Bit) ("^",  Nil, (Bit, Bit) :: Bit) implements redirect ${ xor($0, $1) }
    direct (Bit) ("__equal", Nil, (Bit, Bit) :: Bit) implements redirect ${ xnor($0, $1) }

    // --- Rewrite rules
    val True = "Def(ConstBit(true))"
    val False = "Def(ConstBit(false))"
    val X = "x"
    rewrite (and_bit) using commutative((True,X) -> X)
    rewrite (and_bit) using commutative((False,X) -> ${ false.asBit })
    rewrite (or_bit) using commutative((True,X) -> ${ true.asBit })
    rewrite (or_bit) using commutative((False,X) -> X)
    rewrite (not_bit) using pattern((False) -> ${ true.asBit })
    rewrite (not_bit) using pattern((True) -> ${ false.asBit })
    rewrite (xor_bit) using commutative((True,X) -> ${ not(x) })
    rewrite (xor_bit) using commutative((False,X) -> X)
    rewrite (xnor_bit) using commutative((True,X) -> X)
    rewrite (xnor_bit) using commutative((False,X) -> ${ not(x) })

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

    // --- C++ Backend
    impl (neg_fix) (codegen(cpp, ${ -$0 }))
    impl (add_fix) (codegen(cpp, ${ $0 + $1 }))
    impl (sub_fix) (codegen(cpp, ${ $0 - $1 }))
    impl (mul_fix) (codegen(cpp, ${ $0 * $1 }))
    impl (div_fix) (codegen(cpp, ${ $0 / $1 }))
    impl (mod_fix) (codegen(cpp, ${ $0 % $1 }))
    impl (lt_fix)  (codegen(cpp, ${ $0 < $1 }))
    impl (leq_fix) (codegen(cpp, ${ $0 <= $1 }))
    impl (neq_fix) (codegen(cpp, ${ $0 != $1 }))
    impl (eql_fix) (codegen(cpp, ${ $0 == $1 }))
    impl (and_fix) (codegen(cpp, ${ $0 & $1 }))
    impl (or_fix)  (codegen(cpp, ${ $0 | $1 }))
    impl (lsh_fix) (codegen(cpp, ${ $0 << $1 }))
    impl (rsh_fix) (codegen(cpp, ${ $0 >> $1 }))

    impl (neg_flt) (codegen(cpp, ${ -$0 }))
    impl (add_flt) (codegen(cpp, ${ $0 + $1 }))
    impl (sub_flt) (codegen(cpp, ${ $0 - $1 }))
    impl (mul_flt) (codegen(cpp, ${ $0 * $1 }))
    impl (div_flt) (codegen(cpp, ${ $0 / $1 }))
    impl (lt_flt)  (codegen(cpp, ${ $0 < $1 }))
    impl (leq_flt) (codegen(cpp, ${ $0 <= $1 }))
    impl (neq_flt) (codegen(cpp, ${ $0 != $1 }))
    impl (eql_flt) (codegen(cpp, ${ $0 == $1 }))

    impl (not_bit) (codegen(cpp, ${ !$0 }))
    impl (and_bit) (codegen(cpp, ${ $0 && $1 }))
    impl (or_bit)  (codegen(cpp, ${ $0 || $1 }))
    impl (xor_bit) (codegen(cpp, ${ $0 != $1 }))
    impl (xnor_bit) (codegen(cpp, ${ $0 == $1 }))

    // --- MaxJ Backend
    impl (neg_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = -$0 ;
		}))
    impl (add_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 + $1 ;
		}))
    impl (sub_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 - $1 ;
		}))
    impl (mul_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 * $1 ;
		}))
    impl (div_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 / $1 ;
		}))
    impl (lt_fix)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 < $1 ;
		}))
    impl (leq_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 <= $1 ;
		}))
    impl (neq_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== ( $0 !== $1 );
		}))
    impl (eql_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			$sym <== ( $0 === $1) ;
		}))
    impl (and_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 & $1 ;
		}))
    impl (or_fix)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 | $1 ;
		}))
    impl (lsh_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 << $1 ;
		}))
    impl (rsh_fix) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = $0 >> $1 ;
		}))

    impl (neg_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = -$0 ;
		}))
    impl (add_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 + $1 ;
		}))
    impl (sub_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 - $1 ;
		}))
    impl (mul_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 * $1 ;
		}))
    impl (div_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 / $1 ;
		}))
    impl (lt_flt)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 < $1 ;
		}))
    impl (leq_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== $0 <= $1 ;
		}))
    impl (neq_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
    	$sym <== ( $0 !== $1 );
		}))
    impl (eql_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = dfeFixOffset(1, 0, SignMode.UNSIGNED).newInstance(this);
			$sym <== $0 === $1 ;
		}))

    impl (not_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = ~( $0 );
		}))
    impl (and_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 & $1 ;
		}))
    impl (or_bit)  (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 | $1 ;
		}))
    impl (xor_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = $0 ^ $1 ;
		}))
    impl (xnor_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre $sym = ~ ( $0 ^ $1 ) ;
		}))
  }

  def importBasicMath() {
    val T = tpePar("T")

    val Math  = grp("Math")
    val FixPt = lookupTpe("FixPt")
    val FltPt = lookupTpe("FltPt")
    val Bit   = lookupTpe("Bit")

    // --- Nodes
    // TODO: Support fixed point log, exp, and sqrt?
    /** Absolute value **/
    val abs_fix = direct (FixPt) ("abs", (S,I,F), FixPt(S,I,F) :: FixPt(S,I,F))
    /** Natural logarithm **/
    //val log_fix = direct (FixPt) ("log", Nil, Fix :: Fix)
    /** Natural exponential (Euler's number, e, raised to the given exponent) **/
    //val exp_fix = direct (FixPt) ("exp", Nil, Fix :: Fix)
    /** Square root **/
    //val sqrt_fix = direct (FixPt) ("sqrt", Nil, Fix :: Fix)

    /** Absolute value **/
    val abs_flt = direct (FltPt) ("abs", (G,E), FltPt(G,E) :: FltPt(G,E))
    /** Natural logarithm **/
    val log_flt = direct (FltPt) ("log", (G,E), FltPt(G,E) :: FltPt(G,E))
    /** Natural exponential (Euler's number, e, raised to the given exponent) **/
    val exp_flt = direct (FltPt) ("exp", (G,E), FltPt(G,E) :: FltPt(G,E))
    /** Square root **/
    val sqrt_flt = direct (FltPt) ("sqrt", (G,E), FltPt(G,E) :: FltPt(G,E))

    // --- API
    // Scala's fold and reduce don't produce a binary tree - use these functions instead
    internal (Math) ("reduceTreeLevel", T, (SList(T), ((T,T) ==> T)) :: SList(T)) implements composite ${
      $0.length match {
        case len if len < 1 => stageError("Cannot reduce empty list!")
        case 1 => $0
        case len if len % 2 == 0 => reduceTreeLevel(List.tabulate(len/2){i => $1( $0(2*i), $0(2*i+1)) }, $1)
        case len => reduceTreeLevel(List.tabulate(len/2){i => $1( $0(2*i), $0(2*i+1)) } :+ $0.last, $1)
      }
    }

    // TODO: User facing version should actually take Vectors!
    /** Creates a reduction tree structure of the given list of symbols
     * @param syms: List of symbols to reduce
     * @param rFunc: Associative reduction function
     **/
    direct (Math) ("reduceTree", T, CurriedMethodSignature(List(List(SList(T)), List((T,T) ==> T)),T)) implements composite ${
      reduceTreeLevel($0, $1).head
    }
    /** Creates a reduction tree which calculates the product of the given symbols **/
    direct (Math) ("productTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reduceTree[T]($0){(a,b) => implicitly[Arith[T]].times(a,b) }
    }
    /** Creates a reduction tree which calculates the sum of the given symbols **/
    direct (Math) ("sumTree", T, SList(T) :: T, TArith(T)) implements composite ${
      reduceTree[T]($0){(a,b) => implicitly[Arith[T]].plus(a,b) }
    }


    // TODO: Support a more general pow
    /** Integer power implemented as a simple reduction tree
     * @param x
     * @param n: exponent, currently must be an integer greater than zero
     **/
    direct (Math) ("pow", T, (T, SInt) :: T, TArith(T)) implements composite ${
      if ($1 < 1) stageError("Power less than 1 is currently unsupported.")
      productTree( List.fill($1){$0} )
    }

    /** Integer power
     * @param x
     * @param n: exponent, currently must be an integer greater than zero
     **/
    infix  (FixPt) ("**", (S,I,F), (FixPt(S,I,F), SInt) :: FixPt(S,I,F)) implements redirect ${ pow($0, $1) }

    /** Integer power
     * @param x
     * @param n: exponent, currently must be an integer greater than zero
     **/
    infix  (Math) ("**", (G,E), (FltPt(G,E), SInt) :: FltPt(G,E)) implements redirect ${ pow($0, $1) }

    // --- Scala Backend
    impl (abs_fix) (codegen($cala, ${ FixedPoint.abs($0) }))
    impl (abs_flt) (codegen($cala, ${ FloatPoint.abs($0) }))
    impl (log_flt) (codegen($cala, ${ FloatPoint.log($0) }))
    impl (exp_flt) (codegen($cala, ${ FloatPoint.exp($0) }))
    impl (sqrt_flt) (codegen($cala, ${ FloatPoint.sqrt($0) }))

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
    impl (log_flt) (codegen(maxj,  ${
			@ val pre = maxJPre(sym)
			@ val ts = tpstr(parOf(sym)) (sym.tp, implicitly[SourceContext])
    	$pre $sym = KernelMath.log(new KernelMath.Range( -Float.MAX_VALUE, Float.MAX_VALUE), $0, $ts );
		}))
    impl (exp_flt) (codegen(maxj,  ${
			@ val pre = maxJPre(sym)
			$pre $sym = KernelMath.exp( $0 );
		}))
    impl (sqrt_flt) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = KernelMath.sqrt( $0 );
		}))

  }

  // NOTE: Min and Max don't really need to be nodes, but this simplifies maxj codegen a little
  def importBasicControl() {
    val T = tpePar("T")
    val CT = tpePar("CT", stage=compile)
    val Ctrl = grp("BasicCtrl")
    val Bit = lookupTpe("Bit")

    val mux = internal (Ctrl) ("mux2", T, (("sel", Bit), ("a",T), ("b",T)) :: T, TNum(T))

    /** 2 input multiplexer **/
    direct (Ctrl) ("mux", T, (("sel", Bit), ("a",T), ("b",T)) :: T, TNum(T)) implements composite ${ mux2(sel,a,b) }
    /** @nodoc - 2 input multiplexer with a constant second argument **/
    direct (Ctrl) ("mux", (T,CT), (("sel", Bit), ("a", T), ("b", CT)) :: T, (TNum(T), TNumeric(CT))) implements composite ${ mux($sel, $a, lift_to[CT,T]($b)) }
    /** @nodoc - 2 input multiplexer with a constant first argument **/
    direct (Ctrl) ("mux", (T,CT), (("sel", Bit), ("a", CT), ("b", T)) :: T, (TNum(T), TNumeric(CT))) implements composite ${ mux($sel, lift_to[CT,T]($a), $b) }


    /** Selects the minimum of two given values. Implemented as a mux with a less-than comparison **/
    direct (Ctrl) ("min", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ min2(a,b) }
    /** @nodoc - 2 input minimum with a constant second argument **/
    direct (Ctrl) ("min", (T,CT), (("a", T), ("b", CT)) :: T, (TNum(T), TNumeric(CT), TOrder(T))) implements composite ${ min2($a, lift_to[CT,T]($b)) }
    /** @nodoc - 2 input minimum with a constant first argument **/
    direct (Ctrl) ("min", (T,CT), (("a", CT), ("b", T)) :: T, (TNum(T), TNumeric(CT), TOrder(T))) implements composite ${ min2(lift_to[CT,T]($a), $b) }


    /** Selects the maximum of two given values. Implemented as a mux with a greater-than comparison **/
    direct (Ctrl) ("max", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ max2(a,b) }
    /** @nodoc - 2 input maximum with a constant second argument **/
    direct (Ctrl) ("max", (T,CT), (("a", T), ("b", CT)) :: T, (TNum(T), TNumeric(CT), TOrder(T))) implements composite ${ max2($a, lift_to[CT,T]($b)) }
    /** @nodoc - 2 input maximum with a constant first argument **/
    direct (Ctrl) ("max", (T,CT), (("a", CT), ("b", T)) :: T, (TNum(T), TNumeric(CT), TOrder(T))) implements composite ${ max2(lift_to[CT,T]($a), $b) }


    // --- Scala Backend
    impl (mux) (codegen($cala, ${ if ($sel) $a else $b }))

    // --- MaxJ Backend
    impl (mux) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
			$pre $sym = $sel ? $a : $b ;
		}))
	}

  // Infix operations with a Scala type on the LHS or RHS
  // TODO: Should these be documented?
  def importLiftingMath() {
    val FltPt = lookupTpe("FltPt")
    val FixPt = lookupTpe("FixPt")
    val Bit   = lookupTpe("Bit")

    val Z = lookupTpe("B0",compile)

    infix (FixPt) ("<<", (S,I), (FixPt(S,I,Z), SInt) :: FixPt(S,I,Z)) implements redirect ${ lsh($0, fixPt[Int,S,I,B0]($1)) }
    infix (FixPt) (">>", (S,I), (FixPt(S,I,Z), SInt) :: FixPt(S,I,Z)) implements redirect ${ rsh($0, fixPt[Int,S,I,B0]($1)) }

    // Have to resort to the exhaustive version for these :(
    UnstagedNumerics.foreach { (T, TT) =>
      // Fixed point
      infix (FixPt) ("+",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ add(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) ("-",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ sub(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) ("*",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ mul(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) ("/",  (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ div(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) ("%",  (S,I),   (T, FixPt(S,I,Z)) :: FixPt(S,I,Z)) implements redirect ${ mod(fixPt[\$TT,S,I,B0]($0), $1) }

      infix (FixPt) ("<",  (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ lt(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) (">",  (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ lt($1, fixPt[\$TT,S,I,F]($0)) }
      infix (FixPt) ("<=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ leq(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) (">=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ leq($1, fixPt[\$TT,S,I,F]($0)) }
      infix (FixPt) ("!=", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ neq(fixPt[\$TT,S,I,F]($0), $1) }
      direct (FixPt) ("__equal", (S,I,F), (T, FixPt(S,I,F)) :: Bit) implements redirect ${ eql(fixPt[\$TT,S,I,F]($0), $1) }

      infix (FixPt) ("&", (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ and(fixPt[\$TT,S,I,F]($0), $1) }
      infix (FixPt) ("|", (S,I,F), (T, FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ or(fixPt[\$TT,S,I,F]($0), $1) }

      infix (FixPt) ("+",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ add($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) ("-",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ sub($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) ("*",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ mul($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) ("/",  (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ div($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) ("%",  (S,I),   (FixPt(S,I,Z), T) :: FixPt(S,I,Z)) implements redirect ${ mod($0, fixPt[\$TT,S,I,B0]($1)) }

      infix (FixPt) ("<",  (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ lt($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) (">",  (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ lt(fixPt[\$TT,S,I,F]($1), $0) }
      infix (FixPt) ("<=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ leq($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) (">=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ leq(fixPt[\$TT,S,I,F]($1), $0) }
      infix (FixPt) ("!=", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ neq($0, fixPt[\$TT,S,I,F]($1)) }
      direct (FixPt) ("__equal", (S,I,F), (FixPt(S,I,F), T) :: Bit) implements redirect ${ eql($0, fixPt[\$TT,S,I,F]($1)) }

      infix (FixPt) ("&", (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ and($0, fixPt[\$TT,S,I,F]($1)) }
      infix (FixPt) ("|", (S,I,F), (FixPt(S,I,F), T) :: FixPt(S,I,F)) implements redirect ${ or($0, fixPt[\$TT,S,I,F]($1)) }

      // Floating point
      infix (FltPt) ("+",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ add(fltPt[\$TT,G,E]($0), $1) }
      infix (FltPt) ("-",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ sub(fltPt[\$TT,G,E]($0), $1) }
      infix (FltPt) ("*",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ mul(fltPt[\$TT,G,E]($0), $1) }
      infix (FltPt) ("/",  (G,E), (T, FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ div(fltPt[\$TT,G,E]($0), $1) }

      infix (FltPt) ("<",  (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ lt(fltPt[\$TT,G,E]($0), $1) }
      infix (FltPt) (">",  (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ lt($1, fltPt[\$TT,G,E]($0)) }
      infix (FltPt) ("<=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ leq(fltPt[\$TT,G,E]($0), $1) }
      infix (FltPt) (">=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ leq($1, fltPt[\$TT,G,E]($0)) }
      infix (FltPt) ("!=", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ neq(fltPt[\$TT,G,E]($0), $1) }
      direct (FltPt) ("__equal", (G,E), (T, FltPt(G,E)) :: Bit) implements redirect ${ eql(fltPt[\$TT,G,E]($0), $1) }

      infix (FltPt) ("+",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ add($0, fltPt[\$TT,G,E]($1)) }
      infix (FltPt) ("-",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ sub($0, fltPt[\$TT,G,E]($1)) }
      infix (FltPt) ("*",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ mul($0, fltPt[\$TT,G,E]($1)) }
      infix (FltPt) ("/",  (G,E), (FltPt(G,E), T) :: FltPt(G,E)) implements redirect ${ div($0, fltPt[\$TT,G,E]($1)) }

      infix (FltPt) ("<",  (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ lt($0, fltPt[\$TT,G,E]($1)) }
      infix (FltPt) (">",  (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ lt(fltPt[\$TT,G,E]($1), $0) }
      infix (FltPt) ("<=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ leq($0, fltPt[\$TT,G,E]($1)) }
      infix (FltPt) (">=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ leq(fltPt[\$TT,G,E]($1), $0) }
      infix (FltPt) ("!=", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ neq($0, fltPt[\$TT,G,E]($1)) }
      direct (FltPt) ("__equal", (G,E), (FltPt(G,E), T) :: Bit) implements redirect ${ eql($0, fltPt[\$TT,G,E]($1)) }
    }
  }
}


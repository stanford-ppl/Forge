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

    val Prim = grp("DHDLPrim")
    infix (Prim) ("+", T, (T,T) :: T, TArith(T)) implements redirect ${ implicitly[Arith[T]].add($0, $1) }
    infix (Prim) ("-", T, (T,T) :: T, TArith(T)) implements redirect ${ implicitly[Arith[T]].sub($0, $1) }
    infix (Prim) ("*", T, (T,T) :: T, TArith(T)) implements redirect ${ implicitly[Arith[T]].mul($0, $1) }
    infix (Prim) ("/", T, (T,T) :: T, TArith(T)) implements redirect ${ implicitly[Arith[T]].div($0, $1) }
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

    val Prim = grp("DHDLPrim")
    infix (Prim) ("<", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].lt($0, $1) }
    infix (Prim) (">", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].gt($0, $1) }
    infix (Prim) ("<=", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].leq($0, $1) }
    infix (Prim) (">=", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].geq($0, $1) }
    infix (Prim) ("!=", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].neq($0, $1) }
    direct (Prim) ("__equal", T, (T,T) :: Bit, TOrder(T)) implements redirect ${ implicitly[Order[T]].eql($0, $1) }
  }


	def importPrimitiveMath() {
    val Coll = lookupGrp("Coll").asInstanceOf[Rep[DSLTypeClass]]
    val Num = lookupGrp("Num").asInstanceOf[Rep[DSLTypeClass]]
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val Order = lookupGrp("Order").asInstanceOf[Rep[DSLTypeClass]]

    val Prim = grp("DHDLPrim")
		val Fix = lookupTpe("Fix")
    val Flt = lookupTpe("Flt")
    val Bit = lookupTpe("Bit")

    // --- Nodes
    // Unlike Scala lib, we represent unary negation using a node since directly implementing
    // negation in hardware is simpler than figuring out that we're multiplying by -1.
    val neg_fix = direct (Prim) ("neg_fix", Nil, Fix :: Fix)
    val add_fix = direct (Prim) ("add_fix", Nil, (Fix, Fix) :: Fix)
    val sub_fix = direct (Prim) ("sub_fix", Nil, (Fix, Fix) :: Fix)
    val mul_fix = direct (Prim) ("mul_fix", Nil, (Fix, Fix) :: Fix)
    val div_fix = direct (Prim) ("div_fix", Nil, (Fix, Fix) :: Fix)
    val lt_fix  = direct (Prim) ("lt_fix",  Nil, (Fix, Fix) :: Bit)
    val leq_fix = direct (Prim) ("leq_fix", Nil, (Fix, Fix) :: Bit)
    val neq_fix = direct (Prim) ("neq_fix", Nil, (Fix, Fix) :: Bit)
    val eql_fix = direct (Prim) ("eql_fix", Nil, (Fix, Fix) :: Bit)
    val and_fix = direct (Prim) ("and_fix", Nil, (Fix, Fix) :: Fix)
    val or_fix  = direct (Prim) ("or_fix",  Nil, (Fix, Fix) :: Fix)
    val lsh_fix = direct (Prim) ("lsh_fix", Nil, (Fix, Fix) :: Fix)
    val rsh_fix = direct (Prim) ("rsh_fix", Nil, (Fix, Fix) :: Fix)


    val neg_flt = direct (Prim) ("neg_flt", Nil, Flt :: Flt)
    val add_flt = direct (Prim) ("add_flt", Nil, (Flt, Flt) :: Flt)
    val sub_flt = direct (Prim) ("sub_flt", Nil, (Flt, Flt) :: Flt)
    val mul_flt = direct (Prim) ("mul_flt", Nil, (Flt, Flt) :: Flt)
    val div_flt = direct (Prim) ("div_flt", Nil, (Flt, Flt) :: Flt)
    val lt_flt  = direct (Prim) ("lt_flt",  Nil, (Flt, Flt) :: Bit)
    val leq_flt = direct (Prim) ("leq_flt", Nil, (Flt, Flt) :: Bit)
    val neq_flt = direct (Prim) ("neq_flt", Nil, (Flt, Flt) :: Bit)
    val eql_flt = direct (Prim) ("eql_flt", Nil, (Flt, Flt) :: Bit)


    val not_bit = direct (Prim) ("not_bit", Nil, Bit :: Bit)
    val and_bit = direct (Prim) ("and_bit", Nil, (Bit, Bit) :: Bit)
    val or_bit  = direct (Prim) ("or_bit" , Nil, (Bit, Bit) :: Bit)
    val xor_bit = direct (Prim) ("xor_bit", Nil, (Bit, Bit) :: Bit)   // aka !=
    val xnor_bit = direct (Prim) ("xnor_bit", Nil, (Bit, Bit) :: Bit) // aka ==


    // --- Type class instances
    // TODO: What should the precision of zero be? Should it take precision as an argument?
    val FixColl = tpeClassInst("FixColl", Nil, Coll(Fix))
    infix (FixColl) ("empty", Nil, Nil :: Fix) implements composite ${ 0.toFixPt }
    infix (FixColl) ("zeros", Nil, Fix :: Fix) implements composite ${ 0.toFixPt }

    val FixNum = tpeClassInst("FixNum", Nil, Num(Fix))
    infix (FixNum) ("zero", Nil, Nil :: Fix) implements composite ${ 0.toFixPt }

    val FixArith = tpeClassInst("FixArith", Nil, Arith(Fix))
    infix (FixArith) ("add", Nil, (Fix, Fix) :: Fix) implements composite ${ add_fix($0, $1) }
    infix (FixArith) ("sub", Nil, (Fix, Fix) :: Fix) implements composite ${ sub_fix($0, $1) }
    infix (FixArith) ("mul", Nil, (Fix, Fix) :: Fix) implements composite ${ mul_fix($0, $1) }
    infix (FixArith) ("div", Nil, (Fix, Fix) :: Fix) implements composite ${ div_fix($0, $1) }

    val FixOrder = tpeClassInst("FixOrder", Nil, Order(Fix))
    infix (FixOrder) ("lt",  Nil, (Fix, Fix) :: Bit) implements composite ${ lt_fix($0, $1) }
    infix (FixOrder) ("gt",  Nil, (Fix, Fix) :: Bit) implements composite ${ lt_fix($1, $0) }
    infix (FixOrder) ("leq", Nil, (Fix, Fix) :: Bit) implements composite ${ leq_fix($0, $1) }
    infix (FixOrder) ("geq", Nil, (Fix, Fix) :: Bit) implements composite ${ leq_fix($1, $0) }
    infix (FixOrder) ("neq", Nil, (Fix, Fix) :: Bit) implements composite ${ neq_fix($0, $1) }
    infix (FixOrder) ("eql", Nil, (Fix, Fix) :: Bit) implements composite ${ eql_fix($0, $1) }


    val FltColl = tpeClassInst("FltColl", Nil, Coll(Flt))
    infix (FltColl) ("empty", Nil, Nil :: Flt) implements composite ${ 0.toFltPt }
    infix (FltColl) ("zeros", Nil, Flt :: Flt) implements composite ${ 0.toFltPt }

    val FltNum = tpeClassInst("FltNum", Nil, Num(Flt))
    infix (FltNum) ("zero", Nil, Nil :: Fix) implements composite ${ 0.toFltPt }

    val FltArith = tpeClassInst("FltArith", Nil, Arith(Flt))
    infix (FltArith) ("add", Nil, (Flt, Flt) :: Flt) implements composite ${ add_flt($0, $1) }
    infix (FltArith) ("sub", Nil, (Flt, Flt) :: Flt) implements composite ${ sub_flt($0, $1) }
    infix (FltArith) ("mul", Nil, (Flt, Flt) :: Flt) implements composite ${ mul_flt($0, $1) }
    infix (FltArith) ("div", Nil, (Flt, Flt) :: Flt) implements composite ${ div_flt($0, $1) }

    val FltOrder = tpeClassInst("FltOrder", Nil, Order(Flt))
    infix (FltOrder) ("lt",  Nil, (Flt, Flt) :: Bit) implements composite ${ lt_flt($0, $1) }
    infix (FltOrder) ("gt",  Nil, (Flt, Flt) :: Bit) implements composite ${ lt_flt($1, $0) }
    infix (FltOrder) ("leq", Nil, (Flt, Flt) :: Bit) implements composite ${ leq_flt($0, $1) }
    infix (FltOrder) ("geq", Nil, (Flt, Flt) :: Bit) implements composite ${ leq_flt($1, $0) }
    infix (FltOrder) ("neq", Nil, (Flt, Flt) :: Bit) implements composite ${ neq_flt($0, $1) }
    infix (FltOrder) ("eql", Nil, (Flt, Flt) :: Bit) implements composite ${ eql_flt($0, $1) }


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
    infix (Prim) ("unary_-", Nil, Fix :: Fix)  implements redirect ${ neg_fix($0) }
		infix (Prim) ("+", Nil, (Fix, Fix) :: Fix) implements redirect ${ add_fix($0, $1) }
    infix (Prim) ("-", Nil, (Fix, Fix) :: Fix) implements redirect ${ sub_fix($0, $1) }
    infix (Prim) ("*", Nil, (Fix, Fix) :: Fix) implements redirect ${ mul_fix($0, $1) }
    infix (Prim) ("/", Nil, (Fix, Fix) :: Fix) implements redirect ${ div_fix($0, $1) }

    infix (Prim) ("<", Nil, (Fix, Fix) :: Bit) implements redirect ${ lt_fix($0, $1) }
    infix (Prim) (">", Nil, (Fix, Fix) :: Bit) implements redirect ${ lt_fix($1, $0) }
    infix (Prim) ("<=", Nil, (Fix, Fix) :: Bit) implements redirect ${ leq_fix($0, $1) }
    infix (Prim) (">=", Nil, (Fix, Fix) :: Bit) implements redirect ${ leq_fix($1, $0) }
    infix (Prim) ("!=", Nil, (Fix, Fix) :: Bit) implements redirect ${ neq_fix($0, $1) }
    direct (Prim) ("__equal", Nil, (Fix, Fix) :: Bit) implements redirect ${ eql_fix($0, $1) }

    infix (Prim) ("&", Nil, (Fix, Fix) :: Fix) implements redirect ${ and_fix($0, $1) }
    infix (Prim) ("|", Nil, (Fix, Fix) :: Fix) implements redirect ${ or_fix($0, $1) }

    // TODO: RHS can only be an integer - where to check this?
    infix (Prim) ("<<", Nil, (Fix, Fix) :: Fix) implements redirect ${ lsh_fix($0, $1) }
    infix (Prim) (">>", Nil, (Fix, Fix) :: Fix) implements redirect ${ rsh_fix($0, $1) }


    // Floating Point
    infix (Prim) ("unary_-", Nil, Flt :: Flt) implements redirect ${ neg_flt($0) }
    infix (Prim) ("+", Nil, (Flt, Flt) :: Flt) implements redirect ${ add_flt($0, $1) }
    infix (Prim) ("-", Nil, (Flt, Flt) :: Flt) implements redirect ${ sub_flt($0, $1) }
    infix (Prim) ("*", Nil, (Flt, Flt) :: Flt) implements redirect ${ mul_flt($0, $1) }
    infix (Prim) ("/", Nil, (Flt, Flt) :: Flt) implements redirect ${ div_flt($0, $1) }

    infix (Prim) ("<", Nil, (Flt, Flt) :: Bit) implements redirect ${ lt_flt($0, $1) }
    infix (Prim) (">", Nil, (Flt, Flt) :: Bit) implements redirect ${ lt_flt($1, $0) }
    infix (Prim) ("<=", Nil, (Flt, Flt) :: Bit) implements redirect ${ leq_flt($0, $1) }
    infix (Prim) (">=", Nil, (Flt, Flt) :: Bit) implements redirect ${ leq_flt($1, $0) }
    infix (Prim) ("!=", Nil, (Flt, Flt) :: Bit) implements redirect ${ neq_flt($0, $1) }
    direct (Prim) ("__equal", Nil, (Flt, Flt) :: Bit) implements redirect ${ eql_flt($0, $1) }


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
    	$pre(n) $sym = $0 + $1 ;
		}))
		//TODO: maxj xnor?
    impl (xnor_bit) (codegen(maxj, ${
			@ val pre = maxJPre(sym)
    	$pre(n) $sym = $0 + $1 ;
		}))
  }

  def importBasicMath() {
    val T = tpePar("T")
    val Prim = grp("DHDLPrim")
    val Fix = lookupTpe("Fix")
    val Flt = lookupTpe("Flt")
    val Bit = lookupTpe("Bit")

    // --- Nodes
    // TODO: Support fixed point log, exp, and sqrt?
    val abs_fix = direct (Prim) ("abs_fix", Nil, Fix :: Fix)
    //val log_fix = direct ("log_fix", Nil, Fix :: Fix)
    //val exp_fix = direct ("exp_fix", Nil, Fix :: Fix)
    //val sqrt_fix = direct ("sqrt_fix", Nil, Fix :: Fix)

    val abs_flt = direct (Prim) ("abs_flt", Nil, Flt :: Flt)
    val log_flt = direct (Prim) ("log_flt", Nil, Flt :: Flt)
    val exp_flt = direct (Prim) ("exp_flt", Nil, Flt :: Flt)
    val sqrt_flt = direct (Prim) ("sqrt_flt", Nil, Flt :: Flt)

    // --- API
    // TODO: Support a more general pow
    direct (Prim) ("pow", T, (T, SInt) :: T, TArith(T)) implements composite ${ productTree(List.fill($1){$0}) }

    // Fixed Point
    infix  (Prim) ("**", Nil, (Fix, SInt) :: Fix) implements redirect ${ pow($0, $1) }
    direct (Prim) ("abs", Nil, Fix :: Fix) implements redirect ${ abs_fix($0) }   // Absolute value
    //direct (Prim) ("log", Nil, Fix :: Fix) implements redirect ${ log_fix($0) }   // Natural Logarithm
    //direct (Prim) ("exp", Nil, Fix :: Fix) implements redirect ${ exp_fix($0) }   // Exponent (e^x)
    //direct (Prim) ("sqrt", Nil, Fix :: Fix) implements redirect ${ sqrt_fix($0) } // Square root

    // Floating Point
    infix  (Prim) ("**", Nil, (Flt, SInt) :: Flt) implements redirect ${ pow($0, $1) }
    direct (Prim) ("abs", Nil, Flt :: Flt) implements redirect ${ abs_flt($0) }     // Absolute value
    direct (Prim) ("log", Nil, Flt :: Flt) implements redirect ${ log_flt($0) }     // Natural Logarithm
    direct (Prim) ("exp", Nil, Flt :: Flt) implements redirect ${ exp_flt($0) }     // Exponent (e^x)
    direct (Prim) ("sqrt", Nil, Flt :: Flt) implements redirect ${ sqrt_flt($0) }   // Square root

    // --- Scala Backend
    impl (abs_fix) (codegen($cala, ${ scala.math.abs($0) }))

    impl (abs_flt) (codegen($cala, ${ scala.math.abs($0) }))
    impl (log_flt) (codegen($cala, ${ scala.math.log($0) }))
    impl (exp_flt) (codegen($cala, ${ scala.math.exp($0) }))
    impl (sqrt_flt) (codegen($cala, ${ scala.math.sqrt($0) }))

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
    val Prim = grp("DHDLPrim")
    val Bit = lookupTpe("Bit")

    val mux = direct (Prim) ("mux", T, (("sel", Bit), ("a",T), ("b",T)) :: T, TNum(T))

    direct (Prim) ("min", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ mux($a < $b, a, b) }
    direct (Prim) ("max", T, (("a", T), ("b", T)) :: T, (TOrder(T), TNum(T))) implements composite ${ mux($a > $b, a, b) }

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
    val T = tpePar("T", stage = compile)
    val Prim = grp("DHDLPrimLift")
    val Flt = lookupTpe("Flt")
    val Fix = lookupTpe("Fix")
    val Bit = lookupTpe("Bit")

    // Fix point
    infix (Prim) ("+", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ add_fix(fixPt($0), $1) }
    infix (Prim) ("-", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ sub_fix(fixPt($0), $1) }
    infix (Prim) ("*", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ mul_fix(fixPt($0), $1) }
    infix (Prim) ("/", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ div_fix(fixPt($0), $1) }

    infix (Prim) ("<", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ lt_fix(fixPt($0), $1) }
    infix (Prim) (">", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ lt_fix($1, fixPt($0)) }
    infix (Prim) ("<=", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ leq_fix(fixPt($0), $1) }
    infix (Prim) (">=", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ leq_fix($1, fixPt($0)) }
    infix (Prim) ("!=", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ neq_fix(fixPt($0), $1) }
    infix (Prim) ("!=", T, (Fix, T) :: Bit, TNumeric(T)) implements redirect ${ neq_fix($0, fixPt($1)) }
    direct (Prim) ("__equal", T, (T, Fix) :: Bit, TNumeric(T)) implements redirect ${ eql_fix(fixPt($0), $1) }
    direct (Prim) ("__equal", T, (Fix, T) :: Bit, TNumeric(T)) implements redirect ${ eql_fix($0, fixPt($1)) }

    infix (Prim) ("&", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ and_fix(fixPt($0), $1) }
    infix (Prim) ("|", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ or_fix(fixPt($0), $1) }

    // TODO: RHS can only be an integer - where to check this? (Answer: type system!)
    infix (Prim) ("<<", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ lsh_fix(fixPt($0), $1) }
    infix (Prim) (">>", T, (T, Fix) :: Fix, TNumeric(T)) implements redirect ${ rsh_fix(fixPt($0), $1) }

    // Float point
    infix (Prim) ("+", T, (T, Flt) :: Flt, TNumeric(T)) implements redirect ${ add_flt(fltPt($0), $1) }
    infix (Prim) ("-", T, (T, Flt) :: Flt, TNumeric(T)) implements redirect ${ sub_flt(fltPt($0), $1) }
    infix (Prim) ("*", T, (T, Flt) :: Flt, TNumeric(T)) implements redirect ${ mul_flt(fltPt($0), $1) }
    infix (Prim) ("/", T, (T, Flt) :: Flt, TNumeric(T)) implements redirect ${ div_flt(fltPt($0), $1) }

    infix (Prim) ("<", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ lt_flt(fltPt($0), $1) }
    infix (Prim) (">", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ lt_flt($1, fltPt($0)) }
    infix (Prim) ("<=", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ leq_flt(fltPt($0), $1) }
    infix (Prim) (">=", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ leq_flt($1, fltPt($0)) }
    infix (Prim) ("!=", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ neq_flt(fltPt($0), $1) }
    infix (Prim) ("!=", T, (Flt, T) :: Bit, TNumeric(T)) implements redirect ${ neq_flt($0, fltPt($1)) }
    direct (Prim) ("__equal", T, (T, Flt) :: Bit, TNumeric(T)) implements redirect ${ eql_flt(fltPt($0), $1) }
    direct (Prim) ("__equal", T, (Flt, T) :: Bit, TNumeric(T)) implements redirect ${ eql_flt($0, fltPt($1)) }
  }
}


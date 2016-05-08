package ppl.dsl.forge
package dsls
package dhdl

@dsl
trait DHDLRegs {
  this: DHDLDSL =>

  // TODO: Better / more correct way of exposing register reset?
  def importRegs() {
    val T = tpePar("T")
    val Reg          = lookupTpe("Reg")
    val FixPt        = lookupTpe("FixPt")
    val FltPt        = lookupTpe("FltPt")
    val Bit          = lookupTpe("Bit")
    val RegType      = lookupTpe("RegType", stage=compile)
    val Indices      = lookupTpe("Indices")
    val CounterChain = lookupTpe("CounterChain")
    val Idx          = lookupAlias("Index")

    // --- Nodes
    val reg_new   = internal (Reg) ("reg_new", T, ("init", T) :: Reg(T), effect = mutable)
    val reg_read  = internal (Reg) ("reg_read", T, ("reg", Reg(T)) :: T, aliasHint = aliases(Nil), effect = simple)
    val reg_write = internal (Reg) ("reg_write", T, (("reg", Reg(T)), ("value", T)) :: MUnit, effect = write(0))
    val reg_reset = internal (Reg) ("reg_reset", T, ("reg", Reg(T)) :: MUnit, effect = write(0))

    // --- Internals
    /** @nodoc **/
    direct (Reg) ("regCreate", T, (SOption(SString), T, RegType) :: Reg(T), effect = mutable) implements composite ${
      val reg = reg_new[T](init = $1)
      $0.foreach{name => nameOf(reg) = name }
      isDblBuf(reg) = false
      regType(reg) = $2
      resetValue(reg) = $1
      reg
    }

    /** @nodoc **/
    direct (Reg) ("readReg", T, ("reg", Reg(T)) :: T) implements composite ${ reg_read($0) }
    /** @nodoc **/
    direct (Reg) ("writeReg", T, (("reg", Reg(T)), ("value", T)) :: MUnit, effect = write(0)) implements composite ${ reg_write($0, $1) }

    val Mem = lookupTpeClass("Mem").get
    val RegMem = tpeClassInst("RegMem", T, TMem(T, Reg(T)))
    infix (RegMem) ("ld", T, (Reg(T), Idx) :: T) implements composite ${ readReg($0) } // Ignore address
    infix (RegMem) ("st", T, (Reg(T), Idx, T) :: MUnit, effect = write(0)) implements composite ${ writeReg($0, $2) }
    infix (RegMem) ("flatIdx", T, (Reg(T), Indices) :: Idx) implements composite ${ 0.as[Index] }
    infix (RegMem) ("iterator", T, (Reg(T), SList(MInt)) :: CounterChain) implements composite ${ CounterChain(Counter(max=1)) }
    infix (RegMem) ("empty", T, Reg(T) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](None, zero[T], Regular) }

    // --- API
    /* Reg */
    /** Creates a register with type T and given name **/
    static (Reg) ("apply", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](Some($0), zero[T], Regular) }
    /** Creates an unnamed register with type T **/
    static (Reg) ("apply", T, Nil :: Reg(T), TNum(T)) implements composite ${ regCreate[T](None, zero[T], Regular) }

    UnstagedNumerics.foreach{ (ST,_) =>
      /** Creates a register of type T with given name and reset value **/
      static (Reg) ("apply", T, (("name", SString), ("reset", ST)) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](Some($name), $reset.as[T], Regular) }
      /** Creates an unnamed register with type T and given reset value **/
      static (Reg) ("apply", T, ("reset", ST) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](None, $reset.as[T], Regular) }
    }

    /** Creates a named input argument from the host CPU **/
    direct (Reg) ("ArgIn", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](Some($name), zero[T], ArgumentIn) }
    /** Creates an unnamed input argument from the host CPU **/
    direct (Reg) ("ArgIn", T, Nil :: Reg(T), TNum(T)) implements composite ${ regCreate[T](None, zero[T], ArgumentIn) }

    /** Creats a named output argument to the host CPU **/
    direct (Reg) ("ArgOut", T, ("name", SString) :: Reg(T), TNum(T)) implements composite ${ regCreate[T](Some($name), zero[T], ArgumentOut) }
    /** Creats an unnamed output argument to the host CPU **/
    direct (Reg) ("ArgOut", T, Nil :: Reg(T), TNum(T)) implements composite ${ regCreate[T](None, zero[T], ArgumentOut) }

    val Reg_API = withTpe(Reg)
    Reg_API {
      /** Reads the current value of this register **/
      infix ("value") (Nil :: T) implements redirect ${ readReg($self) }
      /** Creates a writer to this Reg. Note that Regs and ArgOuts can only have one writer, while ArgIns cannot have any **/
      infix (":=") (("x",T) :: MUnit, effect = write(0)) implements composite ${
        if (regType($self) == ArgumentIn) stageError("Writing to an input argument is disallowed")
        reg_write($self, $1)
      }
      /** @nodoc - User register reset is not yet well-defined **/
      infix ("rst") (Nil :: MUnit, effect = write(0)) implements composite ${ reg_reset($self) }
    }

    // TODO: Should warn/error if not an ArgIn?
    /** Enables implicit reading from fixed point type Regs **/
    fimplicit (Reg) ("regFixToFix", (S,I,F), Reg(FixPt(S,I,F)) :: FixPt(S,I,F)) implements redirect ${ readReg($0) }
    /** Enables implicit reading from floating point type Regs **/
    fimplicit (Reg) ("regFltToFlt", (G,E), Reg(FltPt(G,E)) :: FltPt(G,E)) implements redirect ${ readReg($0) }
    /** Enables implicit reading from bit type Regs **/
    fimplicit (Reg) ("regBitToBit", Nil, Reg(Bit) :: Bit) implements redirect ${ readReg($0) }



    // --- Scala Backend
    impl (reg_new)   (codegen($cala, ${ Array($init) }))
    impl (reg_read)  (codegen($cala, ${ $reg.apply(0) }))
    impl (reg_write) (codegen($cala, ${ $reg.update(0, $value) }))
    impl (reg_reset) (codegen($cala, ${
      @ val init = resetValue($reg)
      $reg.update(0, $init)
    }))

    // --- Dot Backend
    impl (reg_new)   (codegen(dot, ${
      @ regType(sym) match {
        @ case Regular =>
          @ if (isDblBuf(sym)) {
              $sym [margin=0, rankdir="LR", label="{<st> | <ld>}" xlabel="$sym "
                    shape="record" color=$dblbufBorderColor style="filled"
                    fillcolor=$regFillColor ]
          @ } else {
              $sym [label= "\$sym" shape="square" style="filled" fillcolor=$regFillColor ]
          @ }
        @ case ArgumentIn =>
          @ alwaysGen {
            $sym [label=$sym shape="Msquare" style="filled" fillcolor=$regFillColor ]
          @ }
        @ case ArgumentOut =>
          @ alwaysGen {
            $sym [label=$sym shape="Msquare" style="filled" fillcolor=$regFillColor ]
          @ }
      @ }
    }))
    impl (reg_read)  (codegen(dot, ${
      @ emitValDef(sym, reg)
    }))
    impl (reg_write) (codegen(dot, ${
      $value -> $reg
    }))
    impl (reg_reset) (codegen(dot, ${
    }))

    // --- MaxJ Backend
    //reg_new (extern)
    impl (reg_read)  (codegen(maxj, ${
      @ val pre = maxJPre(sym)
      @ val regStr = regType(reg) match {
      @   case Regular => quote(reg) + "_hold"
      @   case _ => quote(reg)
      @ }
      $pre $sym = $regStr ;
    }))
    //reg_write (extern)
    impl (reg_reset) (codegen(maxj, ${
    }))
  }
}

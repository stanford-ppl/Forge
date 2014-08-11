package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner,Config}

trait ComplexOps {
  this: OptiLADSL =>

  def importComplexOps() {
    val Complex = tpe("Complex")
    
    data(Complex, ("_real", MDouble), ("_imag", MDouble))

    static (Complex) ("apply", Nil, (MDouble,MDouble) :: Complex) implements allocates(Complex, ${$0}, ${$1})

    val ComplexOps = withTpe(Complex)
    ComplexOps {
   	  infix ("real") (Nil :: MDouble) implements getter(0, "_real")
   	  infix ("imag") (Nil :: MDouble) implements getter(0, "_imag")

   	  infix ("conj") (Nil :: Complex) implements composite ${ Complex($self.real, -$self.imag) }
   	  infix ("+") (Complex :: Complex) implements composite ${ Complex($self.real+$1.real, $self.imag+$1.imag) }
   	  infix ("-") (Complex :: Complex) implements composite ${ Complex($self.real-$1.real, $self.imag-$1.imag) }
   	  infix ("*") (Complex :: Complex) implements composite ${ Complex($self.real*$1.real - $self.imag*$1.imag, $self.real*$1.imag+$self.imag*$1.real) }
   	  infix ("/") (Complex :: Complex) implements composite ${ Complex(($self.real*$1.real+$self.imag*$1.imag)/(square($1.real)+square($1.imag)), ($self.imag*$1.real-$self.real*$1.imag)/(square($1.real)+square($1.imag))) }
   	  infix ("abs") (Nil :: Complex) implements composite ${ Complex(sqrt(square($self.real)+square($self.imag)), unit(0.0)) }
   	  infix ("exp") (Nil :: Complex) implements composite ${ Complex(exp($self.real)*cos($self.imag), exp($self.real)*sin($self.imag)) }
   	  infix ("log") (Nil :: Complex) implements composite ${ Complex(log(sqrt(square($self.real)+square($self.imag))), atan2($self.imag, $self.real)) }

   	  direct ("__equal") (Complex :: MBoolean) implements composite ${ $self.real == $1.real && $self.imag == $1.imag }
    }

    // add Complex to Arith
    val Arith = lookupGrp("Arith").asInstanceOf[Rep[DSLTypeClass]]
    val ComplexArith = tpeClassInst("ArithComplex", Nil, Arith(Complex))
    infix (ComplexArith) ("zero", Nil, Complex :: Complex) implements composite ${ Complex(unit(0.0),unit(0.0)) }
    infix (ComplexArith) ("empty", Nil, Nil :: Complex) implements composite ${ Complex(unit(0.0),unit(0.0)) }
    infix (ComplexArith) ("+", Nil, (Complex,Complex) :: Complex) implements composite ${ complex_pl($0,$1) }
    infix (ComplexArith) ("-", Nil, (Complex,Complex) :: Complex) implements composite ${ complex_sub($0,$1) }
    infix (ComplexArith) ("*", Nil, (Complex,Complex) :: Complex) implements composite ${ complex_mul($0,$1) }
    infix (ComplexArith) ("/", Nil, (Complex,Complex) :: Complex) implements composite ${ complex_div($0,$1) }
    infix (ComplexArith) ("abs", Nil, Complex :: Complex) implements composite ${ complex_abs($0) }
    infix (ComplexArith) ("exp", Nil, Complex :: Complex) implements composite ${ complex_exp($0) }
    infix (ComplexArith) ("log", Nil, Complex :: Complex) implements composite ${ complex_log($0) }

    // add Complex to Stringable
    val Stringable = lookupGrp("Stringable").asInstanceOf[Rep[DSLTypeClass]]
    val ComplexStringable = tpeClassInst("StringableComplex", Nil, Stringable(Complex))
    infix (ComplexStringable) ("makeStr", Nil, Complex :: MString) implements composite ${ 
	  if ($0.imag < unit(0.0)) {
	    $0.real.makeStr + " - " + abs($0.imag) + "i"
	  }
	  else {
	    $0.real.makeStr + " + " + abs($0.imag) + "i"
	  }   
    }

  }
}

package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait StringableOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait Stringable[T] {
    def makeStr(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[String]
  }

  def strtype[A,B](x: Stringable[A]) = x.asInstanceOf[Stringable[B]]

  /**
   * Type class instances
   */

  implicit def canStringableDouble: Stringable[Double] = new Stringable[Double] {
    def makeStr(__arg0: Rep[Double])(implicit __pos: SourceContext) = {
      optila_fmt_str(__arg0)
    }
  }

  implicit def canStringableDenseVectorView[T:Stringable:Typ]: Stringable[DenseVectorView[T]] = new Stringable[DenseVectorView[T]] {
    def makeStr(__arg0: Rep[DenseVectorView[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableStr: Stringable[String] = new Stringable[String] {
    def makeStr(__arg0: Rep[String])(implicit __pos: SourceContext) = {
      __arg0
    }
  }

  implicit def canStringableTup3[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ]: Stringable[Tup3[A,B,C]] = new Stringable[Tup3[A,B,C]] {
    def makeStr(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableDenseVector[T:Stringable:Typ]: Stringable[DenseVector[T]] = new Stringable[DenseVector[T]] {
    def makeStr(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableTup6[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ,E:Stringable:Typ,F:Stringable:Typ]: Stringable[Tup6[A,B,C,D,E,F]] = new Stringable[Tup6[A,B,C,D,E,F]] {
    def makeStr(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableSparseMatrixBuildable[T:Stringable:Typ]: Stringable[SparseMatrixBuildable[T]] = new Stringable[SparseMatrixBuildable[T]] {
    def makeStr(__arg0: Rep[SparseMatrixBuildable[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableComplex: Stringable[Complex] = new Stringable[Complex] {
    def makeStr(__arg0: Rep[Complex])(implicit __pos: SourceContext) = unit("???") /*TR{
      if (__arg0.imag < unit(0.0)) {
      	    __arg0.real.makeStr + " - " + abs(__arg0.imag) + "i"
      	  }
      	  else {
      	    __arg0.real.makeStr + " + " + abs(__arg0.imag) + "i"
      	  }
    }*/
  }

  implicit def canStringableFloat: Stringable[Float] = new Stringable[Float] {
    def makeStr(__arg0: Rep[Float])(implicit __pos: SourceContext) = {
      optila_fmt_str(__arg0)
    }
  }

  implicit def canStringableTup9[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ,E:Stringable:Typ,F:Stringable:Typ,G:Stringable:Typ,H:Stringable:Typ,I:Stringable:Typ]: Stringable[Tup9[A,B,C,D,E,F,G,H,I]] = new Stringable[Tup9[A,B,C,D,E,F,G,H,I]] {
    def makeStr(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableDenseMatrixView[T:Stringable:Typ]: Stringable[DenseMatrixView[T]] = new Stringable[DenseMatrixView[T]] {
    def makeStr(__arg0: Rep[DenseMatrixView[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableDenseMatrix[T:Stringable:Typ]: Stringable[DenseMatrix[T]] = new Stringable[DenseMatrix[T]] {
    def makeStr(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableInt: Stringable[Int] = new Stringable[Int] {
    def makeStr(__arg0: Rep[Int])(implicit __pos: SourceContext) = {
      optila_fmt_str(__arg0)
    }
  }

  implicit def canStringableSparseVector[T:Stringable:Typ]: Stringable[SparseVector[T]] = new Stringable[SparseVector[T]] {
    def makeStr(__arg0: Rep[SparseVector[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableSparseMatrix[T:Stringable:Typ]: Stringable[SparseMatrix[T]] = new Stringable[SparseMatrix[T]] {
    def makeStr(__arg0: Rep[SparseMatrix[T]])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableBool: Stringable[Boolean] = new Stringable[Boolean] {
    def makeStr(__arg0: Rep[Boolean])(implicit __pos: SourceContext) = {
      optila_fmt_str(__arg0)
    }
  }

  implicit def canStringableTup7[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ,E:Stringable:Typ,F:Stringable:Typ,G:Stringable:Typ]: Stringable[Tup7[A,B,C,D,E,F,G]] = new Stringable[Tup7[A,B,C,D,E,F,G]] {
    def makeStr(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableTup8[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ,E:Stringable:Typ,F:Stringable:Typ,G:Stringable:Typ,H:Stringable:Typ]: Stringable[Tup8[A,B,C,D,E,F,G,H]] = new Stringable[Tup8[A,B,C,D,E,F,G,H]] {
    def makeStr(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableIndexVector: Stringable[IndexVector] = new Stringable[IndexVector] {
    def makeStr(__arg0: Rep[IndexVector])(implicit __pos: SourceContext) = {
      __arg0.makeString
    }
  }

  implicit def canStringableTup4[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ]: Stringable[Tup4[A,B,C,D]] = new Stringable[Tup4[A,B,C,D]] {
    def makeStr(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableTup5[A:Stringable:Typ,B:Stringable:Typ,C:Stringable:Typ,D:Stringable:Typ,E:Stringable:Typ]: Stringable[Tup5[A,B,C,D,E]] = new Stringable[Tup5[A,B,C,D,E]] {
    def makeStr(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }

  implicit def canStringableTup2[A:Stringable:Typ,B:Stringable:Typ]: Stringable[Tup2[A,B]] = new Stringable[Tup2[A,B]] {
    def makeStr(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      unit("???")
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class Stringable2StringableOps[T](self: Rep[T])(implicit __cb0: Typ[T],__tc: Stringable[T]){
    def makeStr()(implicit __pos: SourceContext) = stringable_makestr[T](self)
  }

  def stringable_makestr[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Stringable[T]): Rep[String] = __tc.makeStr(__arg0)
}

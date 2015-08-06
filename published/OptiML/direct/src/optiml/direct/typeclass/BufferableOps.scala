package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait BufferableOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait Bufferable[T] {
    def mutable(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def write(__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[Unit]
    def size(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[Int]
  }

  def buftype[A,B](x: Bufferable[A]) = x.asInstanceOf[Bufferable[B]]

  /**
   * Type class instances
   */

  implicit def canBufferableTup5[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ,E:Bufferable:Typ]: Bufferable[Tup5[A,B,C,D,E]] = new Bufferable[Tup5[A,B,C,D,E]] {
    def mutable(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable,t._5.mutable))
    }
    def write(t1: Rep[Tup5[A,B,C,D,E]],t2: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
      t1._5.write(t2._5)
    }
    def size(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)+bufferable_size(t._5)
    }
  }

  implicit def canBufferableDenseMatrix[T:Typ]: Bufferable[DenseMatrix[T]] = new Bufferable[DenseMatrix[T]] {
    def mutable(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      DenseMatrix[T](__arg0.numRows, __arg0.numCols)
    }
    def write(__arg0: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      (unit(0)::__arg0.size) foreach { i =>
        
        densematrix_raw_update(__arg1,i,densematrix_raw_apply(__arg0,i))
      }
    }
    def size(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      __arg0.size
    }
  }

  implicit def canBufferableTup6[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ,E:Bufferable:Typ,F:Bufferable:Typ]: Bufferable[Tup6[A,B,C,D,E,F]] = new Bufferable[Tup6[A,B,C,D,E,F]] {
    def mutable(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable,t._5.mutable,t._6.mutable))
    }
    def write(t1: Rep[Tup6[A,B,C,D,E,F]],t2: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
      t1._5.write(t2._5)
      t1._6.write(t2._6)
    }
    def size(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)+bufferable_size(t._5)+bufferable_size(t._6)
    }
  }

  implicit def canBufferableTup2[A:Bufferable:Typ,B:Bufferable:Typ]: Bufferable[Tup2[A,B]] = new Bufferable[Tup2[A,B]] {
    def mutable(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable))
    }
    def write(t1: Rep[Tup2[A,B]],t2: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
    }
    def size(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)
    }
  }

  implicit def canBufferableTup4[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ]: Bufferable[Tup4[A,B,C,D]] = new Bufferable[Tup4[A,B,C,D]] {
    def mutable(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable))
    }
    def write(t1: Rep[Tup4[A,B,C,D]],t2: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
    }
    def size(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)
    }
  }

  implicit def canBufferableTup3[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ]: Bufferable[Tup3[A,B,C]] = new Bufferable[Tup3[A,B,C]] {
    def mutable(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable))
    }
    def write(t1: Rep[Tup3[A,B,C]],t2: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
    }
    def size(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)
    }
  }

  implicit def canBufferableTup7[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ,E:Bufferable:Typ,F:Bufferable:Typ,G:Bufferable:Typ]: Bufferable[Tup7[A,B,C,D,E,F,G]] = new Bufferable[Tup7[A,B,C,D,E,F,G]] {
    def mutable(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable,t._5.mutable,t._6.mutable,t._7.mutable))
    }
    def write(t1: Rep[Tup7[A,B,C,D,E,F,G]],t2: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
      t1._5.write(t2._5)
      t1._6.write(t2._6)
      t1._7.write(t2._7)
    }
    def size(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)+bufferable_size(t._5)+bufferable_size(t._6)+bufferable_size(t._7)
    }
  }

  implicit def canBufferableTup8[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ,E:Bufferable:Typ,F:Bufferable:Typ,G:Bufferable:Typ,H:Bufferable:Typ]: Bufferable[Tup8[A,B,C,D,E,F,G,H]] = new Bufferable[Tup8[A,B,C,D,E,F,G,H]] {
    def mutable(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable,t._5.mutable,t._6.mutable,t._7.mutable,t._8.mutable))
    }
    def write(t1: Rep[Tup8[A,B,C,D,E,F,G,H]],t2: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
      t1._5.write(t2._5)
      t1._6.write(t2._6)
      t1._7.write(t2._7)
      t1._8.write(t2._8)
    }
    def size(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)+bufferable_size(t._5)+bufferable_size(t._6)+bufferable_size(t._7)+bufferable_size(t._8)
    }
  }

  implicit def canBufferableTup9[A:Bufferable:Typ,B:Bufferable:Typ,C:Bufferable:Typ,D:Bufferable:Typ,E:Bufferable:Typ,F:Bufferable:Typ,G:Bufferable:Typ,H:Bufferable:Typ,I:Bufferable:Typ]: Bufferable[Tup9[A,B,C,D,E,F,G,H,I]] = new Bufferable[Tup9[A,B,C,D,E,F,G,H,I]] {
    def mutable(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t._1.mutable,t._2.mutable,t._3.mutable,t._4.mutable,t._5.mutable,t._6.mutable,t._7.mutable,t._8.mutable,t._9.mutable))
    }
    def write(t1: Rep[Tup9[A,B,C,D,E,F,G,H,I]],t2: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      t1._1.write(t2._1)
      t1._2.write(t2._2)
      t1._3.write(t2._3)
      t1._4.write(t2._4)
      t1._5.write(t2._5)
      t1._6.write(t2._6)
      t1._7.write(t2._7)
      t1._8.write(t2._8)
      t1._9.write(t2._9)
    }
    def size(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      bufferable_size(t._1)+bufferable_size(t._2)+bufferable_size(t._3)+bufferable_size(t._4)+bufferable_size(t._5)+bufferable_size(t._6)+bufferable_size(t._7)+bufferable_size(t._8)+bufferable_size(t._9)
    }
  }

  implicit def canBufferableDenseVector[T:Typ]: Bufferable[DenseVector[T]] = new Bufferable[DenseVector[T]] {
    def mutable(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      val l:Rep[Int] = __arg0.length //TR compiler crash without type annotation!
      val r = __arg0.isRow
      DenseVector[T](l, r)
    }
    def write(__arg0: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      __arg0.indices foreach { i => __arg1(i) = __arg0(i) }
    }
    def size(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      __arg0.length
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class Bufferable2BufferableOps[T](self: Rep[T])(implicit __cb0: Typ[T],__tc: Bufferable[T]){
    def mutable()(implicit __pos: SourceContext) = bufferable_mutable[T](self)
    def write(__arg1: Rep[T])(implicit __pos: SourceContext) = bufferable_write[T](self,__arg1)
    def size()(implicit __pos: SourceContext) = bufferable_size[T](self)
  }

  def bufferable_mutable[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Bufferable[T]): Rep[T] = __tc.mutable(__arg0)
  def bufferable_write[T](__arg0: Rep[T],__arg1: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Bufferable[T]): Rep[Unit] = __tc.write(__arg0,__arg1)
  def bufferable_size[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Bufferable[T]): Rep[Int] = __tc.size(__arg0)
}

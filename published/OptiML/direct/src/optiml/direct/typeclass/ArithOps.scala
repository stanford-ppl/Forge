package optiml.direct.typeclass

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

trait ArithOps extends Base with scala.math.Numeric.ExtraImplicits {
  this: OptiML => 

  /**
   * Type class
   */
  trait Arith[T] {
    def zero(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def empty(implicit __pos: SourceContext): Rep[T]
    def +(__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def -(__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def *(__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def /(__arg0: Rep[T],__arg1: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def abs(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def exp(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
    def log(__arg0: Rep[T])(implicit __pos: SourceContext): Rep[T]
  }

  def atype[A,B](x: Arith[A]) = x.asInstanceOf[Arith[B]]

  /**
   * Type class instances
   */

  implicit def canArithDouble: Arith[Double] = new Arith[Double] {
    def zero(__arg0: Rep[Double])(implicit __pos: SourceContext) = {
      unit(0.0)
    }
    def empty(implicit __pos: SourceContext) = {
      unit(0.0)
    }
    def +(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = {
      forge_double_plus(__arg0,__arg1)
    }
    def -(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = {
      forge_double_minus(__arg0,__arg1)
    }
    def *(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = {
      forge_double_times(__arg0,__arg1)
    }
    def /(__arg0: Rep[Double],__arg1: Rep[Double])(implicit __pos: SourceContext) = {
      forge_double_divide(__arg0,__arg1)
    }
    def abs(__arg0: Rep[Double])(implicit __pos: SourceContext) = {
      math_object_abs(__arg0)
    }
    def exp(__arg0: Rep[Double])(implicit __pos: SourceContext) = {
      math_object_exp(__arg0)
    }
    def log(__arg0: Rep[Double])(implicit __pos: SourceContext) = {
      math_object_log(__arg0)
    }
  }

  implicit def canArithTup8[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ,E:Arith:Typ,F:Arith:Typ,G:Arith:Typ,H:Arith:Typ]: Arith[Tup8[A,B,C,D,E,F,G,H]] = new Arith[Tup8[A,B,C,D,E,F,G,H]] {
    def zero(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero,t._5.zero,t._6.zero,t._7.zero,t._8.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty,implicitly[Arith[E]].empty,implicitly[Arith[F]].empty,implicitly[Arith[G]].empty,implicitly[Arith[H]].empty))
    }
    def +(t1: Rep[Tup8[A,B,C,D,E,F,G,H]],t2: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4,t1._5+t2._5,t1._6+t2._6,t1._7+t2._7,t1._8+t2._8))
    }
    def -(t1: Rep[Tup8[A,B,C,D,E,F,G,H]],t2: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4,t1._5-t2._5,t1._6-t2._6,t1._7-t2._7,t1._8-t2._8))
    }
    def *(t1: Rep[Tup8[A,B,C,D,E,F,G,H]],t2: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4,t1._5*t2._5,t1._6*t2._6,t1._7*t2._7,t1._8*t2._8))
    }
    def /(t1: Rep[Tup8[A,B,C,D,E,F,G,H]],t2: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4,t1._5/t2._5,t1._6/t2._6,t1._7/t2._7,t1._8/t2._8))
    }
    def abs(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs,t._5.abs,t._6.abs,t._7.abs,t._8.abs))
    }
    def exp(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp,t._5.exp,t._6.exp,t._7.exp,t._8.exp))
    }
    def log(t: Rep[Tup8[A,B,C,D,E,F,G,H]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log,t._5.log,t._6.log,t._7.log,t._8.log))
    }
  }

  implicit def canArithLong: Arith[Long] = new Arith[Long] {
    def zero(__arg0: Rep[Long])(implicit __pos: SourceContext) = {
      unit(0L)
    }
    def empty(implicit __pos: SourceContext) = {
      unit(0L)
    }
    def +(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = {
      forge_long_plus(__arg0,__arg1)
    }
    def -(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = {
      forge_long_minus(__arg0,__arg1)
    }
    def *(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = {
      forge_long_times(__arg0,__arg1)
    }
    def /(__arg0: Rep[Long],__arg1: Rep[Long])(implicit __pos: SourceContext) = {
      forge_long_divide(__arg0,__arg1)
    }
    def abs(__arg0: Rep[Long])(implicit __pos: SourceContext) = {
      math_object_abs(__arg0.toDouble).toLong
    }
    def exp(__arg0: Rep[Long])(implicit __pos: SourceContext) = {
      math_object_exp(__arg0.toDouble).toLong
    }
    def log(__arg0: Rep[Long])(implicit __pos: SourceContext) = {
      math_object_log(__arg0.toDouble).toLong
    }
  }

  implicit def canArithInt: Arith[Int] = new Arith[Int] {
    def zero(__arg0: Rep[Int])(implicit __pos: SourceContext) = {
      unit(0)
    }
    def empty(implicit __pos: SourceContext) = {
      unit(0)
    }
    def +(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
      forge_int_plus(__arg0,__arg1)
    }
    def -(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
      forge_int_minus(__arg0,__arg1)
    }
    def *(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
      forge_int_times(__arg0,__arg1)
    }
    def /(__arg0: Rep[Int],__arg1: Rep[Int])(implicit __pos: SourceContext) = {
      forge_int_divide(__arg0,__arg1)
    }
    def abs(__arg0: Rep[Int])(implicit __pos: SourceContext) = {
      math_object_abs(__arg0).toInt
    }
    def exp(__arg0: Rep[Int])(implicit __pos: SourceContext) = {
      math_object_exp(__arg0).toInt
    }
    def log(__arg0: Rep[Int])(implicit __pos: SourceContext) = {
      math_object_log(__arg0).toInt
    }
  }

  implicit def canArithTup3[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ]: Arith[Tup3[A,B,C]] = new Arith[Tup3[A,B,C]] {
    def zero(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty))
    }
    def +(t1: Rep[Tup3[A,B,C]],t2: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3))
    }
    def -(t1: Rep[Tup3[A,B,C]],t2: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3))
    }
    def *(t1: Rep[Tup3[A,B,C]],t2: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3))
    }
    def /(t1: Rep[Tup3[A,B,C]],t2: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3))
    }
    def abs(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs))
    }
    def exp(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp))
    }
    def log(t: Rep[Tup3[A,B,C]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log))
    }
  }

  implicit def canArithTup7[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ,E:Arith:Typ,F:Arith:Typ,G:Arith:Typ]: Arith[Tup7[A,B,C,D,E,F,G]] = new Arith[Tup7[A,B,C,D,E,F,G]] {
    def zero(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero,t._5.zero,t._6.zero,t._7.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty,implicitly[Arith[E]].empty,implicitly[Arith[F]].empty,implicitly[Arith[G]].empty))
    }
    def +(t1: Rep[Tup7[A,B,C,D,E,F,G]],t2: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4,t1._5+t2._5,t1._6+t2._6,t1._7+t2._7))
    }
    def -(t1: Rep[Tup7[A,B,C,D,E,F,G]],t2: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4,t1._5-t2._5,t1._6-t2._6,t1._7-t2._7))
    }
    def *(t1: Rep[Tup7[A,B,C,D,E,F,G]],t2: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4,t1._5*t2._5,t1._6*t2._6,t1._7*t2._7))
    }
    def /(t1: Rep[Tup7[A,B,C,D,E,F,G]],t2: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4,t1._5/t2._5,t1._6/t2._6,t1._7/t2._7))
    }
    def abs(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs,t._5.abs,t._6.abs,t._7.abs))
    }
    def exp(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp,t._5.exp,t._6.exp,t._7.exp))
    }
    def log(t: Rep[Tup7[A,B,C,D,E,F,G]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log,t._5.log,t._6.log,t._7.log))
    }
  }

  implicit def canArithTup9[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ,E:Arith:Typ,F:Arith:Typ,G:Arith:Typ,H:Arith:Typ,I:Arith:Typ]: Arith[Tup9[A,B,C,D,E,F,G,H,I]] = new Arith[Tup9[A,B,C,D,E,F,G,H,I]] {
    def zero(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero,t._5.zero,t._6.zero,t._7.zero,t._8.zero,t._9.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty,implicitly[Arith[E]].empty,implicitly[Arith[F]].empty,implicitly[Arith[G]].empty,implicitly[Arith[H]].empty,implicitly[Arith[I]].empty))
    }
    def +(t1: Rep[Tup9[A,B,C,D,E,F,G,H,I]],t2: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4,t1._5+t2._5,t1._6+t2._6,t1._7+t2._7,t1._8+t2._8,t1._9+t2._9))
    }
    def -(t1: Rep[Tup9[A,B,C,D,E,F,G,H,I]],t2: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4,t1._5-t2._5,t1._6-t2._6,t1._7-t2._7,t1._8-t2._8,t1._9-t2._9))
    }
    def *(t1: Rep[Tup9[A,B,C,D,E,F,G,H,I]],t2: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4,t1._5*t2._5,t1._6*t2._6,t1._7*t2._7,t1._8*t2._8,t1._9*t2._9))
    }
    def /(t1: Rep[Tup9[A,B,C,D,E,F,G,H,I]],t2: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4,t1._5/t2._5,t1._6/t2._6,t1._7/t2._7,t1._8/t2._8,t1._9/t2._9))
    }
    def abs(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs,t._5.abs,t._6.abs,t._7.abs,t._8.abs,t._9.abs))
    }
    def exp(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp,t._5.exp,t._6.exp,t._7.exp,t._8.exp,t._9.exp))
    }
    def log(t: Rep[Tup9[A,B,C,D,E,F,G,H,I]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log,t._5.log,t._6.log,t._7.log,t._8.log,t._9.log))
    }
  }

  implicit def canArithTup5[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ,E:Arith:Typ]: Arith[Tup5[A,B,C,D,E]] = new Arith[Tup5[A,B,C,D,E]] {
    def zero(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero,t._5.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty,implicitly[Arith[E]].empty))
    }
    def +(t1: Rep[Tup5[A,B,C,D,E]],t2: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4,t1._5+t2._5))
    }
    def -(t1: Rep[Tup5[A,B,C,D,E]],t2: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4,t1._5-t2._5))
    }
    def *(t1: Rep[Tup5[A,B,C,D,E]],t2: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4,t1._5*t2._5))
    }
    def /(t1: Rep[Tup5[A,B,C,D,E]],t2: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4,t1._5/t2._5))
    }
    def abs(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs,t._5.abs))
    }
    def exp(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp,t._5.exp))
    }
    def log(t: Rep[Tup5[A,B,C,D,E]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log,t._5.log))
    }
  }

  implicit def canArithTup4[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ]: Arith[Tup4[A,B,C,D]] = new Arith[Tup4[A,B,C,D]] {
    def zero(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty))
    }
    def +(t1: Rep[Tup4[A,B,C,D]],t2: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4))
    }
    def -(t1: Rep[Tup4[A,B,C,D]],t2: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4))
    }
    def *(t1: Rep[Tup4[A,B,C,D]],t2: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4))
    }
    def /(t1: Rep[Tup4[A,B,C,D]],t2: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4))
    }
    def abs(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs))
    }
    def exp(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp))
    }
    def log(t: Rep[Tup4[A,B,C,D]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log))
    }
  }

  implicit def canArithTup6[A:Arith:Typ,B:Arith:Typ,C:Arith:Typ,D:Arith:Typ,E:Arith:Typ,F:Arith:Typ]: Arith[Tup6[A,B,C,D,E,F]] = new Arith[Tup6[A,B,C,D,E,F]] {
    def zero(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero,t._3.zero,t._4.zero,t._5.zero,t._6.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty,implicitly[Arith[C]].empty,implicitly[Arith[D]].empty,implicitly[Arith[E]].empty,implicitly[Arith[F]].empty))
    }
    def +(t1: Rep[Tup6[A,B,C,D,E,F]],t2: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2,t1._3+t2._3,t1._4+t2._4,t1._5+t2._5,t1._6+t2._6))
    }
    def -(t1: Rep[Tup6[A,B,C,D,E,F]],t2: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2,t1._3-t2._3,t1._4-t2._4,t1._5-t2._5,t1._6-t2._6))
    }
    def *(t1: Rep[Tup6[A,B,C,D,E,F]],t2: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2,t1._3*t2._3,t1._4*t2._4,t1._5*t2._5,t1._6*t2._6))
    }
    def /(t1: Rep[Tup6[A,B,C,D,E,F]],t2: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2,t1._3/t2._3,t1._4/t2._4,t1._5/t2._5,t1._6/t2._6))
    }
    def abs(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs,t._3.abs,t._4.abs,t._5.abs,t._6.abs))
    }
    def exp(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp,t._3.exp,t._4.exp,t._5.exp,t._6.exp))
    }
    def log(t: Rep[Tup6[A,B,C,D,E,F]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log,t._3.log,t._4.log,t._5.log,t._6.log))
    }
  }

  implicit def canArithDenseMatrix[T:Arith:Typ]: Arith[DenseMatrix[T]] = new Arith[DenseMatrix[T]] {
    def zero(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      (unit(0)::__arg0.numRows,unit(0)::__arg0.numCols) { (i,j) => implicitly[Arith[T]].empty }
    }
    def empty(implicit __pos: SourceContext) = {
      densematrix_fromarray[T](array_empty_imm[T](unit(0)),unit(0),unit(0))
    }
    def +(__arg0: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_pl(__arg0,__arg1)
    }
    def -(__arg0: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_sub(__arg0,__arg1)
    }
    def *(__arg0: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_mulclnmul(__arg0,__arg1)
    }
    def /(__arg0: Rep[DenseMatrix[T]],__arg1: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_div(__arg0,__arg1)
    }
    def abs(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_abs(__arg0)
    }
    def exp(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_exp(__arg0)
    }
    def log(__arg0: Rep[DenseMatrix[T]])(implicit __pos: SourceContext) = {
      densematrix_log(__arg0)
    }
  }

  implicit def canArithDenseVector[T:Arith:Typ]: Arith[DenseVector[T]] = new Arith[DenseVector[T]] {
    def zero(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_fromfunc[T](__arg0.length, __arg0.isRow, i => implicitly[Arith[T]].empty)
    }
    def empty(implicit __pos: SourceContext) = {
      densevector_fromarray[T](array_empty_imm[T](unit(0)),unit(true))
    }
    def +(__arg0: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_pl(__arg0,__arg1)
    }
    def -(__arg0: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_sub(__arg0,__arg1)
    }
    def *(__arg0: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_mul(__arg0,__arg1)
    }
    def /(__arg0: Rep[DenseVector[T]],__arg1: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_div(__arg0,__arg1)
    }
    def abs(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_abs(__arg0)
    }
    def exp(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_exp(__arg0)
    }
    def log(__arg0: Rep[DenseVector[T]])(implicit __pos: SourceContext) = {
      densevector_log(__arg0)
    }
  }

  implicit def canArithTup2[A:Arith:Typ,B:Arith:Typ]: Arith[Tup2[A,B]] = new Arith[Tup2[A,B]] {
    def zero(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t._1.zero,t._2.zero))
    }
    def empty(implicit __pos: SourceContext) = {
      pack((implicitly[Arith[A]].empty,implicitly[Arith[B]].empty))
    }
    def +(t1: Rep[Tup2[A,B]],t2: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t1._1+t2._1,t1._2+t2._2))
    }
    def -(t1: Rep[Tup2[A,B]],t2: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t1._1-t2._1,t1._2-t2._2))
    }
    def *(t1: Rep[Tup2[A,B]],t2: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t1._1*t2._1,t1._2*t2._2))
    }
    def /(t1: Rep[Tup2[A,B]],t2: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t1._1/t2._1,t1._2/t2._2))
    }
    def abs(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t._1.abs,t._2.abs))
    }
    def exp(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t._1.exp,t._2.exp))
    }
    def log(t: Rep[Tup2[A,B]])(implicit __pos: SourceContext) = {
      pack((t._1.log,t._2.log))
    }
  }

  implicit def canArithFloat: Arith[Float] = new Arith[Float] {
    def zero(__arg0: Rep[Float])(implicit __pos: SourceContext) = {
      unit(0f)
    }
    def empty(implicit __pos: SourceContext) = {
      unit(0f)
    }
    def +(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = {
      forge_float_plus(__arg0,__arg1)
    }
    def -(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = {
      forge_float_minus(__arg0,__arg1)
    }
    def *(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = {
      forge_float_times(__arg0,__arg1)
    }
    def /(__arg0: Rep[Float],__arg1: Rep[Float])(implicit __pos: SourceContext) = {
      forge_float_divide(__arg0,__arg1)
    }
    def abs(__arg0: Rep[Float])(implicit __pos: SourceContext) = {
      math_object_abs(__arg0).toFloat
    }
    def exp(__arg0: Rep[Float])(implicit __pos: SourceContext) = {
      math_object_exp(__arg0).toFloat
    }
    def log(__arg0: Rep[Float])(implicit __pos: SourceContext) = {
      math_object_log(__arg0).toFloat
    }
  }

  implicit def canArithComplex: Arith[Complex] = new Arith[Complex] {
    def zero(__arg0: Rep[Complex])(implicit __pos: SourceContext) = {
      Complex(unit(0.0),unit(0.0))
    }
    def empty(implicit __pos: SourceContext) = {
      Complex(unit(0.0),unit(0.0))
    }
    def +(__arg0: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_pl(__arg0,__arg1)
    }
    def -(__arg0: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_sub(__arg0,__arg1)
    }
    def *(__arg0: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_mul(__arg0,__arg1)
    }
    def /(__arg0: Rep[Complex],__arg1: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_div(__arg0,__arg1)
    }
    def abs(__arg0: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_abs(__arg0)
    }
    def exp(__arg0: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_exp(__arg0)
    }
    def log(__arg0: Rep[Complex])(implicit __pos: SourceContext) = {
      complex_log(__arg0)
    }
  }


  /**
   * Forwarders - these allow infix notation to be used when the type class is available
   */
  implicit class Arith2ArithOps[T](self: Rep[T])(implicit __cb0: Typ[T],__tc: Arith[T]){
    def zero()(implicit __pos: SourceContext) = arith_zero[T](self)
    def empty()(implicit __pos: SourceContext) = arith_empty[T]()
    def +(__arg1: Rep[T])(implicit __pos: SourceContext) = arith_pl[T](self,__arg1)
    def -(__arg1: Rep[T])(implicit __pos: SourceContext) = arith_sub[T](self,__arg1)
    def *(__arg1: Rep[T])(implicit __pos: SourceContext) = arith_mul[T](self,__arg1)
    def /(__arg1: Rep[T])(implicit __pos: SourceContext) = arith_div[T](self,__arg1)
    def abs()(implicit __pos: SourceContext) = arith_abs[T](self)
    def exp()(implicit __pos: SourceContext) = arith_exp[T](self)
    def log()(implicit __pos: SourceContext) = arith_log[T](self)
  }

  def arith_zero[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.zero(__arg0)
  def arith_empty[T]()(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.empty
  def arith_pl[T](__arg0: Rep[T],__arg1: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.+(__arg0,__arg1)
  def arith_sub[T](__arg0: Rep[T],__arg1: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.-(__arg0,__arg1)
  def arith_mul[T](__arg0: Rep[T],__arg1: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.*(__arg0,__arg1)
  def arith_div[T](__arg0: Rep[T],__arg1: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc./(__arg0,__arg1)
  def arith_abs[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.abs(__arg0)
  def arith_exp[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.exp(__arg0)
  def arith_log[T](__arg0: Rep[T])(implicit __cb0: Typ[T],__pos: SourceContext,__tc: Arith[T]): Rep[T] = __tc.log(__arg0)
}

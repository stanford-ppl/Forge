package optiml.direct

import scala.annotation.unchecked.uncheckedVariance
//import scala.reflect.{Manifest,SourceContext}

trait ReppableOps extends Base with scala.math.Numeric.ExtraImplicits {
  trait Reppable[A,B] {
    def view(__arg0: A)(implicit __pos: SourceContext): Rep[B]
  }

  implicit def canReppableIdentity[A:Typ]: Reppable[Rep[A],A] = new Reppable[Rep[A],A] {
    def view(__arg0: Rep[A])(implicit __pos: SourceContext) = {
      __arg0
    }
  }

  implicit def canReppableLiteral[A:Typ]: Reppable[A,A] = new Reppable[A,A] {
    def view(__arg0: A)(implicit __pos: SourceContext) = {
      unit(__arg0)
    }
  }
}

trait ReppableCompilerOps extends ReppableOps

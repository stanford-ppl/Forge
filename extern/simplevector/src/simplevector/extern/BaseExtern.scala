package simplevector.extern

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common._

/**
 * One issue with these external modules is compile order: the module can't use
 * any ops in the generated dsl as part of their implementation.
 * 
 * We could get around this by creating yet another project (simplevector-base?)
 * that simplevector-extern and simplevector-gen both relied on, that would define
 * common parameters...
 */
trait BaseWrapper {
  type Rep[+T] = T // unfortunate
}
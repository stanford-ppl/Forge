package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait FeatureHelperOps extends Base {
  this: OptiML => 

  def unique(__arg0: Rep[String])(implicit __pos: SourceContext) = featurehelper_unique(__arg0)(__pos)
  def loadUniqueMappings(__arg0: Rep[String])(implicit __pos: SourceContext) = featurehelper_loaduniquemappings(__arg0)(__pos)
  def dumpUniqueMappings(__arg0: Rep[String])(implicit __pos: SourceContext) = featurehelper_dumpuniquemappings(__arg0)(__pos)


  def featurehelper_unique(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Int]
  def featurehelper_loaduniquemappings(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Int]
  def featurehelper_dumpuniquemappings(__arg0: Rep[String])(implicit __pos: SourceContext): Rep[Unit]
}

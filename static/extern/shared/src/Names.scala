package LOWERCASE_DSL_NAME.shared

import scala.virtualization.lms.common.Base

// Candidate for being pushd into Delite
trait NameTypes extends Base {
  def getSymName(e: Rep[Any]): Option[String]
  def setSymName(e: Rep[Any], name: String): Unit

  object nameOf {
    def apply(e: Rep[Any]) = getSymName(e)
    def update(e: Rep[Any], name: String) = setSymName(e, name)
  }
}

trait NameOps extends ForgeMetadataOps with NameTypes {
  case class MName(name: String) extends Metadata

  override def getSymName(e: Rep[Any]): Option[String] = meta[MName](e).map(_.name)
  override def setSymName(e: Rep[Any], name: String) = setMetadata(e, MName(name))
}

trait NameCompilerOps extends NameOps

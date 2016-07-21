package LOWERCASE_DSL_NAME.shared

import scala.virtualization.lms.common.Base

// Candidate for being pushd into Delite
trait NameOps extends ForgeMetadataOps {

  case class MName(name: String) extends Metadata

  def getSymName(e: Rep[Any]): Option[String] = meta[MName](e).map(_.name)
  def setSymName(e: Rep[Any], name: String) = setMetadata(e, MName(name))

  object nameOf {
    def apply(e: Rep[Any]) = getSymName(e)
    def update(e: Rep[Any], name: String) = setSymName(e, name)
  }
}

trait NameCompilerOps extends NameOps

package LOWERCASE_DSL_NAME.library

import scala.reflect.SourceContext
import scala.virtualization.lms.common.MetadataOps

// Library version of metadata
trait ForgeMetadataWrapper extends MetadataOps { this: HUMAN_DSL_NAMEBase =>

  var metadata: Map[Rep[Any], SymbolProperties] = Map.empty

  // Directly add symbol property metadata mapping for symbol
  def setProps(e: Rep[Any], p: SymbolProperties)(implicit ctx: SourceContext): Unit = {
    metadata += e -> (meet(MetaOverwrite, metadata.get(e), Some(p))).get
  }

  def getProps(e: Rep[Any]): Option[SymbolProperties] = Some(metadata.getOrElse(e, initRep(e)))

  def defaultMetadata(e: Rep[Any]): List[Metadata] = Nil

  def initRep(e: Rep[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropMap[Datakey[_],Metadata](data.map{m => m.key -> m}.toList)
    val typeData = PropMap[Datakey[_],Metadata](defaultMetadata(e).map{m => m.key -> m})
    val symData = meet(MetaTypeInit, givenData, typeData)
    initProps(e, symData, child, index)
  }

  def unapplyArrayLike(e: Rep[Any]): Option[Option[SymbolProperties]] = None
  def unapplyStructLike(e: Rep[Any]): Option[List[(String,Rep[Any])]] = None

  object ArrayLike {
    def unapply(e: Rep[Any]) = unapplyArrayLike(e)
  }
  object StructLike {
    def unapply(e: Rep[Any]) = unapplyStructLike(e)
  }

  // Should be overwritten for data structure types (e.g. structs, arrays)
  def initProps(e: Rep[Any], symData: PropMap[Datakey[_],Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = e match {
    case ArrayLike(typeChild) =>
      val symChild = meet(MetaTypeInit, child, typeChild)
      ArrayProperties(symChild, symData)

    case StructLike(fields) =>
      val children = fields.map{ case (index,f) =>
        val typeData = initRep(f.asInstanceOf[Rep[Any]])
        metadata.get(f) match {
          case Some(data) => index -> meet(MetaTypeInit, typeData, data)
          case None => index -> typeData
        }
      }
      val typeFields = PropMap(children)
      val symFields = (index,child) match {
        case (Some(index),Some(child)) => meet(MetaTypeInit, PropMap(index, child), typeFields)
        case _ => typeFields
      }
      StructProperties(symFields, symData)

    case _ => ScalarProperties(symData)
  }
}

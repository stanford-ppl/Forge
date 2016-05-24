package optima.compiler.transform

import optima.shared._
import optima.shared.ops._
import optima.compiler._
import optima.compiler.ops._

import scala.virtualization.lms.internal.Traversal
import scala.reflect.SourceContext

// TODO: Not a full analysis stage right now - just fills in layouts as flat
trait LayoutAnalyzer extends Traversal {
  val IR: OptiMAExp
  import IR._
  override val name = "LayoutAnalyzer"
  override val debugMode = false

  private def subtype(view: Boolean, buff: Boolean) = (view, buff) match {
    case (false,false) => Plain
    case (false,true) => Buffer
    case (true,false) => View
    case (true,true) => BuffView
  }

  // TODO: These should probably live elsewhere
  def isArrayNDType(x: Manifest[_]): Boolean = isSubtype(x.erasure, classOf[ArrayND[_]])

  def containsArrayNDType[T](tp: Manifest[T]): Boolean = tp match {
    case tp if isArrayNDType(tp) => true
    case StructType(_,elems) => elems.map{f => containsArrayNDType(f._2)}.fold(false){_||_}
    case tp => tp.typeArguments.map{f => containsArrayNDType(f)}.fold(false){_||_}
  }

  // Recursively set everything to be flat, row-major
  def setLayout(p: Option[SymbolProperties])(implicit ctx: SourceContext): Option[SymbolProperties] = p match {
    case Some(a: ArrayProperties) =>
      val sub = subtype(isPhysView(a), isPhysBuffer(a))
      Some(ArrayProperties(setLayout(a.child), PropMap(classOf[MLayout], MLayout(rank(a),Flat,sub) )))
    case Some(s: StructProperties) =>
      Some(StructProperties(s.children.map{child => setLayout(Some(child)).get}, s.data))
    case _ => p
  }

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    for ((e,m) <- metadata) {
      implicit val ctx: SourceContext = mpos(e.pos)

      if (containsArrayNDType(e.tp)) {
        val origProps = getProps(e)
        val newProps = setLayout(origProps)
        setProps(e, meet(MetaOverwrite, origProps, newProps))
      }
    }
    (s)
  }
}

trait LayoutAnalysisExp
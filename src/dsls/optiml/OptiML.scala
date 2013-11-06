package ppl.dsl.forge
package dsls
package optiml

import optila.OptiLADSL
import core.ForgeApplicationRunner

object OptiMLDSLRunner extends ForgeApplicationRunner with OptiMLDSL

trait OptiMLDSL extends OptiLADSL
  with SetOps with BufferableOps {

  override def dslName = "OptiML"

  override def specification() = {
    // include OptiLA
    super.specification()

    // disallow "while"
    // this has the unfortunate side-effect of breaking even internally used whiles, which are lifted the same way...
    // we could get around this by only overriding while inside a new trait that is only mixed in at the Application level (sort of a de-lift)

    // val whileDo = lookupOp("Misc","__whileDo")
    // compiler (whileDo) lookupImpl("Misc","__whileDo").apply(0)
    // impl (whileDo) (composite ${ fatal("illegal operation: 'while'. try using 'untilconverged' instead") })

    extern(grp("Sum"))
    importBufferableOps()
    importSetOps()
    importUntilConverged()
  }

  def importUntilConverged() {
    // pull in distance metrics
    importDistanceMetrics()

    val Control = grp("Control")
    val T = tpePar("T")

    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Control) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".001"), ("minIter", MInt, "1"), ("maxIter", MInt, "1000")), ("block", T ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
      var delta = scala.Double.MaxValue
      var cur = x
      var iter = 0

      while ((abs(delta) > tol && iter < maxIter) || iter < minIter) {
        val prev = cur
        val next = block(cur)
        iter += 1
        delta = diff(prev,next)
        cur = next
      }

      if (iter == maxIter){
        println("Maximum iterations exceeded")
      }

      cur
    }

    // double-buffered untilconverged. 'block' must not change the structure of the input across iterations.
    direct (Control) ("untilconverged_buffered", T withBound TBufferable, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".001"), ("minIter", MInt, "1"), ("maxIter", MInt, "1000")), ("block", T ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
      val bufA = x.mutable
      val bufB = x.mutable
      // val bufSize = bufferable_size(bufA)
      x.write(bufA)

      var delta = scala.Double.MaxValue
      var iter = 0

      while ((abs(delta) > tol && iter < maxIter) || iter < minIter) {
        // if all goes well, everything fuses (what about the delta function?)
        // can't single buffer because we can't DCE the writes to the first buffer even if we fuse
        val cur = block(bufA.unsafeImmutable)

        // doesn't supress fusion, so then will only run after cur.write(bufB), which may be too late
        // however, it seems it will prevent DCEing cur, so commented out for now...
        // if (bufferable_size(cur) != bufSize) {
        //   fatal("untilconverged buffer changed size")
        // }

        cur.write(bufB)
        delta = diff(bufA,bufB)

        // fusion-friendly copy instead of swap (using vars is nested mutable and using a branch causes a control dependency)
        // cur.write(bufB) should always be able to fuse, but bufB.write(bufA) should only fuse if block pipelines its input reads
        // currently, though, this never fuses, since bufB is mutable which prevents it (and bufB.unsafeImmutable also prevents it)
        bufB.write(bufA)

        iter += 1
      }

      if (iter == maxIter){
        println("Maximum iterations exceeded")
      }

      bufA
    }

  }

  def importDistanceMetrics() {
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")

    // val Arith = lookupGrp("Arith")
    val Prim = lookupGrp("Primitive2")
    val T = tpePar("T")

    // default metric is ABS

    // don't kick in when polymorphic, for unknown reasons
    // fimplicit (DenseVector) ("dist", T, (DenseVector(T),DenseVector(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    // fimplicit (DenseMatrix) ("dist", T, (DenseMatrix(T),DenseMatrix(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    fimplicit (Prim) ("dist", Nil, (MInt,MInt) :: MDouble) implements composite ${ abs($0-$1) }
    fimplicit (Prim) ("dist", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ abs($0-$1) }

    val DMetric = tpe("DistanceMetric", stage = compile)
    identifier (DMetric) ("ABS")
    identifier (DMetric) ("SQUARE")
    identifier (DMetric) ("EUC")

    for (TP <- List(DenseVector,DenseMatrix,SparseVector)) {
      fimplicit (TP) ("dist", Nil, (TP(MDouble),TP(MDouble)) :: MDouble) implements composite ${ sum(abs($0 - $1)) }

      direct (TP) ("dist", Nil, (TP(MDouble),TP(MDouble),DMetric) :: MDouble) implements composite ${
        $2 match {
          case ABS => dist($0,$1)
          case SQUARE => sum(square($0 - $1))
          case EUC => sqrt(sum(square($0 - $1)))
        }
      }
    }
  }
}

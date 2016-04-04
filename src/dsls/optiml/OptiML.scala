package ppl.dsl.forge
package dsls
package optiml

import core.ForgeApplicationRunner
import optila.OptiLADSL
import factor._

object OptiMLDSLRunner extends ForgeApplicationRunner with OptiMLDSL

trait OptiMLDSL extends OptiLADSL
  with MLIOOps with FeatureOps with SetOps with StreamOps with ImageOps with TreeOps
  with FactorOps with FactorGraphOps
  with ClassifierOps with ValidateOps
  with BufferableOps with TrainingSetLikeOps {

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

    // OptiML types
    val V = tpePar("V")
    val T = tpePar("T")
    val HashStream = tpe("HashStream", V)
    val DHashStream = tpe("DHashStream", V)
    val FileStream = tpe("FileStream")
    val ComputeStream = tpe("ComputeStream", T)

    // OptiML ops
    importSetOps()
    importTrainingSetLikeOps()
    importByteBuffer()
    importKeyValueStoreOps()
    extern(grp("Sum"))
    importBufferableOps()
    importFeatureOps()
    importFeatureHelperOps()
    importUntilConverged()
    importAllFactorGraphOps()
    importMLIOOps()
    importStreamOps()
    importImageOps()
    importTreeOps()
    importClassifierOps()
    importValidateOps()
  }

  def importUntilConverged() {
    val Control = grp("Control")
    val T = tpePar("T")

    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Control) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, "unit(.001)"), ("minIter", MInt, "unit(1)"), ("maxIter", MInt, "unit(1000)"), ("verbose", MBoolean, "unit(false)")), ("block", (T,MInt) ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
      var delta = scala.Double.MaxValue
      var cur = x
      var iter = 0

      while ((abs(delta) > tol && iter < maxIter) || iter < minIter) {
        val prev = cur
        val next = block(cur, iter)
        delta = diff(prev, next)
        if (verbose) println("[optiml]: iter " + iter + ", delta: " + delta)
        iter += 1
        cur = next
      }

      if (verbose) {
        if (iter == maxIter) {
          println("[optiml]: maximum iterations (" + iter + ") exceeded")
        }
        else {
          println("[optiml]: converged in " + iter + " iterations")
        }
      }

      cur
    }

    // this is a convenience method that allows a user to override the 'diff' function without explicitly passing other implicits
    direct (Control) ("untilconverged_withdiff", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, "unit(.001)"), ("minIter", MInt, "unit(1)"), ("maxIter", MInt, "unit(1000)")), ("block", (T,MInt) ==> T), ("diff", (T,T) ==> MDouble)), T)) implements redirect ${
      untilconverged(x, tol, minIter, maxIter)(block)(manifest[T], implicitly[SourceContext], diff)
    }

    // double-buffered untilconverged. 'block' must not change the structure of the input across iterations.
    direct (Control) ("untilconverged_buffered", T withBound TBufferable, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, "unit(.001)"), ("minIter", MInt, "unit(1)"), ("maxIter", MInt, "unit(1000)")), ("block", T ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
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
}

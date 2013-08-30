package ppl.dsl.forge
package examples
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
    importVecMatConstructor()
    importUntilConverged()
  }

  def importVecMatConstructor() {
    val DenseVector = lookupTpe("DenseVector")
    val IndexVector = lookupTpe("IndexVector")
    val T = tpePar("T")

    // vector constructor (0 :: end) { ... }
    noSourceContextList ::= "::" // surpress SourceContext implicit because it interferes with the 'apply' method being immediately callable
    infix (IndexVector) ("::", Nil, ((("end", MInt), ("start", MInt)) :: IndexVector)) implements composite ${ IndexVector($start, $end) }

    // should add apply directly to IndexVector, otherwise we have issues with ambiguous implicit conversions
    infix (IndexVector) ("apply", T, (IndexVector, MInt ==> T)  :: DenseVector(T)) implements composite ${ $0.map($1) }

    // matrix constructor (0::numRows,0::numCols) { ... }
    val DenseMatrix = lookupTpe("DenseMatrix")

    infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexVector), (MInt,MInt) ==> T) :: DenseMatrix(T)) implements composite ${
      val (rowIndices,colIndices) = $0

      // can fuse with flat matrix loops
      val v = (0::rowIndices.length*colIndices.length).toDense
      val indices = densematrix_fromarray(densevector_raw_data(v),rowIndices.length,colIndices.length)
      indices map { i =>
        val rowIndex = i / colIndices.length
        val colIndex = i % colIndices.length
        $1(rowIndex,colIndex)
      }

      // could fuse with nested matrix loops (loops over rowIndices), but not with loops directly over individual matrix elements -- like map!
      // it seems best for us to be consistent: matrix loops should either all be flat or all be nested. which one? should we use lowerings?
      // however, mutable version also supresses fusion due to unsafeImmutable...

      // val out = DenseMatrix[T](rowIndices.length,colIndices.length)
      // rowIndices foreach { i =>
      //   colIndices foreach { j =>
      //     out(i,j) = $1(i,j)
      //   }
      // }
      // out.unsafeImmutable
    }

    val IndexWildcard = tpe("IndexWildcard", stage = compile)
    identifier (IndexWildcard) ("*")

    infix (IndexVector) ("apply", T, (CTuple2(IndexVector,IndexWildcard), MInt ==> DenseVector(T)) :: DenseMatrix(T)) implements composite ${
      val rowIndices = $0._1
      val first = $1(rowIndices(0)) // better be pure, because we ignore it to maintain normal loop size below
      val out = DenseMatrix[T](rowIndices.length,first.length)
      rowIndices foreach { i =>
        out(i) = $1(i)
      }
      out.unsafeImmutable
    }
  }

  def importUntilConverged() {
    // pull in distance metrics
    importDistanceMetrics()

    val Control = grp("Control")
    val T = tpePar("T")

    // "block" should not mutate the input, but always produce a new copy. in this version, block can change the structure of the input across iterations (e.g. increase its size)
    direct (Control) ("untilconverged", T, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".001"), ("maxIter", MInt, "1000")), ("block", T ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
      var delta = scala.Double.MaxValue
      var cur = x
      var iter = 0

      while (abs(delta) > tol && (iter < maxIter)){
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
    direct (Control) ("untilconverged_buffered", T withBound TBufferable, CurriedMethodSignature(List(List(("x", T), ("tol", MDouble, ".001"), ("maxIter", MInt, "1000")), ("block", T ==> T)), T), ("diff", (T,T) ==> MDouble)) implements composite ${
      val bufA = x.mutable
      val bufB = x.mutable
      // val bufSize = bufferable_size(bufA)
      x.write(bufA)

      var delta = scala.Double.MaxValue
      var iter = 0

      while (abs(delta) > tol && (iter < maxIter)){
        // if all goes well, everything fuses (what about the delta function?)
        // can't single buffer because we can't DCE the writes to the first buffer even if we fuse

        // FIXME: simple tests fuse, but RBM gets the wrong answer when we pass in .unsafeImmutable, with or without fusion
        // apparently unsafeImmutable on structs is not always safe (can bypass rewrites?)
        // val cur = block(bufA.unsafeImmutable)
        val cur = block(bufA)

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
    // val Arith = lookupGrp("Arith")
    val Prim = lookupGrp("Primitive2")
    val T = tpePar("T")

    // default metric is ABS

    // don't kick in when polymorphic, for unknown reasons
    // fimplicit (DenseVector) ("dist", T, (DenseVector(T),DenseVector(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }
    // fimplicit (DenseMatrix) ("dist", T, (DenseMatrix(T),DenseMatrix(T)) :: MDouble, ("conv", T ==> MDouble)) implements composite ${ sum(abs($0.toDouble - $1.toDouble)) }

    fimplicit (Prim) ("dist", Nil, (MDouble,MDouble) :: MDouble) implements composite ${ abs($0-$1) }
    fimplicit (DenseVector) ("dist", Nil, (DenseVector(MDouble),DenseVector(MDouble)) :: MDouble) implements composite ${ sum(abs($0 - $1)) }
    fimplicit (DenseMatrix) ("dist", Nil, (DenseMatrix(MDouble),DenseMatrix(MDouble)) :: MDouble) implements composite ${ sum(abs($0 - $1)) }

    val DMetric = tpe("DistanceMetric", stage = compile)
    identifier (DMetric) ("ABS")
    identifier (DMetric) ("SQUARE")
    identifier (DMetric) ("EUC")

    for (TP <- List(DenseVector,DenseMatrix)) {
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

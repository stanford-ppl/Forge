package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait SetOps {
  this: OptiMLDSL =>

  def importSetOps() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")

    val D = tpePar("D")
    val L = tpePar("L")

    val TrainingSet = tpe("TrainingSet", (D,L))
    data(TrainingSet, ("_data", DenseMatrix(D)), ("_labels", DenseVector(L)))
    static (TrainingSet) ("apply", (D,L), (DenseMatrix(D), DenseVector(L)) :: TrainingSet(D,L)) implements allocates(TrainingSet, ${$0}, ${$1})

    val TrainingSetOps = withTpe(TrainingSet)
    TrainingSetOps {
      infix ("labels") (Nil :: DenseVector(L)) implements getter(0, "_labels")
    }

    val TestSet = tpe("TestSet", D)
    data(TestSet, ("_data", DenseMatrix(D)))
    static (TestSet) ("apply", D, DenseMatrix(D) :: TestSet(D)) implements allocates(TestSet, ${$0})

    for (t <- List(TrainingSet, TestSet)) {
      val ops = withTpe(t)
      ops {
        infix ("data") (Nil :: DenseMatrix(D)) implements getter(0, "_data")

        infix ("apply") ((MInt,MInt) :: D) implements composite ${ $self.data.apply($1,$2) }
        infix ("apply") (MInt :: DenseVectorView(D)) implements composite ${ $self.data.apply($1) }
        // infix ("apply") (IndexVector :: t) implements composite ${ TrainingSet($self.data.apply($1),$self.labels.apply($1)) }   // scalac typer crash...

        infix ("numSamples") (Nil :: MInt) implements composite ${ $self.data.numRows }
        infix ("numFeatures") (Nil :: MInt) implements composite ${ $self.data.numCols }
      }
    }
  }
}

package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait SetOps {
  this: OptiMLDSL =>

  def importSetOps() {
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val SparseVectorView = lookupTpe("SparseVectorView")
    val IndexVector = lookupTpe("IndexVector")

    val D = tpePar("D")
    val L = tpePar("L")

    // Dense and Sparse TrainingSets have identical interfaces, except for different backing data matrix types.
    for (s <- List("DenseTrainingSet", "SparseTrainingSet")) {
      val TrainingSet = tpe(s, (D,L))
      val Matrix = if (s == "DenseTrainingSet") DenseMatrix else SparseMatrix
      val VectorView = if (s == "DenseTrainingSet") DenseVectorView else SparseVectorView

      data(TrainingSet, ("_data", Matrix(D)), ("_labels", DenseVector(L)))
      static (TrainingSet) ("apply", (D,L), (Matrix(D), DenseVector(L)) :: TrainingSet(D,L)) implements allocates(TrainingSet, ${$0}, ${$1})

      //val TrainingSetOps = withTpe(TrainingSet)
      //TrainingSetOps {
      import org.scala_lang.virtualized.virtualize
      magic()
      @virtualize
      def magic[R]() = withTpee(TrainingSet){
        infix ("labels") (Nil :: DenseVector(L)) implements getter(0, "_labels")
      }

      // is there any use for a separate TestSet type (readability)? to actually measure error,
      // TestSets will typically have labels, and therefore be structurally identical to TrainingSet

      // val DenseTestSet = tpe("DenseTestSet", D)
      // data(DenseTestSet, ("_data", DenseMatrix(D)))
      // static (DenseTestSet) ("apply", D, DenseMatrix(D) :: DenseTestSet(D)) implements allocates(DenseTestSet, ${$0})

      for (t <- List(TrainingSet/*, DenseTestSet*/)) {
        //val ops = withTpe(t)
        //ops {
        import org.scala_lang.virtualized.virtualize
        magic2()
        @virtualize
        def magic2[R]() = withTpee(t){
          infix ("data") (Nil :: Matrix(D)) implements getter(0, "_data")

          infix ("apply") ((MInt,MInt) :: D) implements composite ${ $self.data.apply($1,$2) }
          infix ("apply") (MInt :: VectorView(D)) implements composite ${ $self.data.apply($1) }
          // infix ("apply") (IndexVector :: t) implements composite ${ DenseTrainingSet($self.data.apply($1),$self.labels.apply($1)) }   // scalac typer crash...

          infix ("numSamples") (Nil :: MInt) implements composite ${ $self.data.numRows }
          infix ("numFeatures") (Nil :: MInt) implements composite ${ $self.data.numCols }
        }
      }
    }
  }
}

package ppl.dsl.forge
package dsls
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the Stringable type class for scalars, vectors, and matrices.
 * This type class is necessary because the correct toString is not callable from within Delite.
 */
trait StringableOps {
  this: OptiLADSL =>

  object TStringable extends TypeClassSignature {
    def name = "Stringable"
    def prefix = "_str"
    def wrapper = Some("strtype")
  }

  def importStringableOps() {
    val T = tpePar("T")

    val Stringable = tpeClass("Stringable", TStringable, T)

    // Stringable type class interface
    infix (Stringable) ("makeStr", T, T :: MString)

    // primitive implementations
    val DoubleStringable = tpeClassInst("StringableDouble", Nil, Stringable(MDouble))
    infix (DoubleStringable) ("makeStr", Nil, MDouble :: MString) implements composite ${ "" + $0 }

    val FloatStringable = tpeClassInst("StringableFloat", Nil, Stringable(MFloat))
    infix (FloatStringable) ("makeStr", Nil, MFloat :: MString) implements composite ${ "" + $0 }

    val IntStringable = tpeClassInst("StringableInt", Nil, Stringable(MInt))
    infix (IntStringable) ("makeStr", Nil, MInt :: MString) implements composite ${ "" + $0 }

    val BoolStringable = tpeClassInst("StringableBool", Nil, Stringable(MBoolean))
    infix (BoolStringable) ("makeStr", Nil, MBoolean :: MString) implements composite ${ "" + $0 }

    val StrStringable = tpeClassInst("StringableStr", Nil, Stringable(MString))
    infix (StrStringable) ("makeStr", Nil, MString :: MString) implements composite ${ $0 }

    // OptiLA types
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val SparseVector = lookupTpe("SparseVector")
    val SparseMatrix = lookupTpe("SparseMatrix")
    val SparseMatrixBuildable = lookupTpe("SparseMatrixBuildable")

    val DenseVectorStringable = tpeClassInst("StringableDenseVector", T withBound TStringable, Stringable(DenseVector(T)))
    infix (DenseVectorStringable) ("makeStr", Nil, DenseVector(T) :: MString) implements composite ${ $0.makeString }

    val DenseVectorViewStringable = tpeClassInst("StringableDenseVectorView", T withBound TStringable, Stringable(DenseVectorView(T)))
    infix (DenseVectorViewStringable) ("makeStr", Nil, DenseVectorView(T) :: MString) implements composite ${ $0.makeString }

    val IndexVectorStringable = tpeClassInst("StringableIndexVector", Nil, Stringable(IndexVector))
    infix (IndexVectorStringable) ("makeStr", Nil, IndexVector :: MString) implements composite ${ $0.makeString }

    val DenseMatrixStringable = tpeClassInst("StringableDenseMatrix", T withBound TStringable, Stringable(DenseMatrix(T)))
    infix (DenseMatrixStringable) ("makeStr", Nil, DenseMatrix(T) :: MString) implements composite ${ $0.makeString }

    val SparseVectorStringable = tpeClassInst("StringableSparseVector", T withBound TStringable, Stringable(SparseVector(T)))
    infix (SparseVectorStringable) ("makeStr", Nil, SparseVector(T) :: MString) implements composite ${ $0.makeString }

    val SparseMatrixBuildableStringable = tpeClassInst("StringableSparseMatrixBuildable", T withBound TStringable, Stringable(SparseMatrixBuildable(T)))
    infix (SparseMatrixBuildableStringable) ("makeStr", Nil, SparseMatrixBuildable(T) :: MString) implements composite ${ $0.makeString }

    val SparseMatrixStringable = tpeClassInst("StringableSparseMatrix", T withBound TStringable, Stringable(SparseMatrix(T)))
    infix (SparseMatrixStringable) ("makeStr", Nil, SparseMatrix(T) :: MString) implements composite ${ $0.makeString }


    // tuples of stringables
    for (arity <- 2 until maxTuples) {
      val Tup = lookupTpe("Tup"+arity)
      val pars = (0 until arity).map(i => tpePar(('A'.toInt+i).toChar.toString) withBound TStringable).toList
      val TupStringable = tpeClassInst("StringableTup"+arity, pars, Stringable(Tup))

      val makeTupStr = "\"(\"+" + (1 to arity).map(i => "t._"+i+".makeStr").mkString("+\",\"+") + "+\")\""
      infix (TupStringable) ("makeStr", pars, ("t",Tup) :: MString) implements composite ${ \$makeTupStr }
    }
  }
}

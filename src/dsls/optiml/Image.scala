package ppl.dsl.forge
package dsls
package optiml

import core.{ForgeApplication,ForgeApplicationRunner}

trait ImageOps {
  this: OptiMLDSL =>

  def importImageOps() {
    val Tuple2 = lookupTpe("Tup2")
    val DenseVector = lookupTpe("DenseVector")
    val DenseMatrix = lookupTpe("DenseMatrix")
    val GrayscaleImage = tpe("GrayscaleImage")

    data(GrayscaleImage, ("_data", DenseMatrix(MDouble)))
    static (GrayscaleImage) ("apply", Nil, DenseMatrix(MDouble) :: GrayscaleImage) implements allocates(GrayscaleImage, ${$0})

    val ImageOps = withTpe(GrayscaleImage)
    ImageOps {
      infix ("data") (Nil :: DenseMatrix(MDouble)) implements getter(0, "_data")
      infix ("numRows") (Nil :: MInt) implements redirect ${ $self.data.numRows }
      infix ("numCols") (Nil :: MInt) implements redirect ${ $self.data.numCols }

      infix ("downsample") (CurriedMethodSignature(List(List(("rowFactor", MInt), ("colFactor", MInt)), List(("sample", GrayscaleImage ==> MDouble))), GrayscaleImage)) implements composite ${
        val sampledData = (0::($self.numRows / rowFactor), 0::($self.numCols / colFactor))  { (i,j) =>          
          sample(GrayscaleImage($self.data.slice(rowFactor * i, rowFactor*i + rowFactor, colFactor*j, colFactor*j + colFactor)))
        }
        GrayscaleImage(sampledData)
      }

      infix ("convolve") (("kernel", DenseMatrix(MDouble)) :: GrayscaleImage) implements composite ${
        $self.windowedFilter(kernel.numRows, kernel.numCols) { slice => (slice.data *:* kernel).sum }        
      }

      infix ("windowedFilter") (CurriedMethodSignature(List(List(("rowDim", MInt), ("colDim", MInt)), List(("block", GrayscaleImage ==> MDouble))), GrayscaleImage)) implements composite ${
        // we want a rowDim x colDim size window, so we move rowOffset or colOffset away
        val rowOffset = (rowDim - 1) / 2
        val colOffset = (colDim - 1) / 2

        val out = (0::$self.numRows, 0::$self.numCols) { (i,j) =>
          val window = (i-rowOffset::i+rowOffset+1, j-colOffset::j+colOffset+1) { (ii, jj) =>
            // clip out of box (could use other strategies, like repeat border)
            if (ii < 0 || ii >= $self.numRows || jj < 0 || jj >= $self.numCols) {
              0.0
            }
            else {
              $self.data.apply(ii,jj)
            }
          }
          block(GrayscaleImage(window))
        }
        GrayscaleImage(out)
      }

      infix ("histogram") (Nil :: DenseVector(MInt)) implements composite ${
        val buckets = $self.data.flattenToVector.groupByReduce(e => e.toInt, e => 1, (a: Rep[Int],b: Rep[Int]) => a+b)
        (0::256) { i => if (buckets.contains(i)) buckets(i) else 0 }
      }

      // -- ported from previous version of OptiML, but it is not clear if these are general
      //    enough to be here, or were added for a particular application. commenting for now.

      // infix ("downsampleBitwiseOr") (Nil :: GrayscaleImage) implements composite ${
      //   $self.downsample(2,2) { slice => 
      //     slice(0,0).toInt | slice(1,0).toInt | slice(0,1).toInt | slice(1,1).toInt
      //   }
      // }

      // infix ("cartToPolar") ((("x",GrayscaleImage), ("y",GrayscaleImage)) :: Tuple2(GrayscaleImage, GrayscaleImage)) implements composite ${
      //   val mag = x.zip(y) { (a,b) => sqrt(a*a + b*b) }
      //   val phase = x.zip(y) { (a,b) => (atan2(b, a)*180/Pi) } map { a => if (a < 0.0) a + 360.0 else a }
      //   pack((mag, phase))
      // }

      // infix ("gradients") (("polar", MBoolean, "false") :: Tuple2(GrayscaleImage, GrayscaleImage)) {        
      //   val scharrYkernel = DenseMatrix((-3.,-10.,-3.), (0.,0.,0.), (3.,10.,3.)) 
      //   val scharrXkernel = scharrYkernel.t
      //   val a = $self.convolve(scharrXkernel)
      //   val b = $self.convolve(scharrYkernel)
      //   if (polar) cartToPolar(a,b) else pack((a,b))
      // }          
    }
  }   
}

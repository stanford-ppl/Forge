import optiml.compiler._
import optiml.library._
import optiml.shared._

object ConvLayerCompiler extends OptiMLApplicationCompiler with ConvLayer
trait ConvLayer extends OptiMLApplication with NetLib {
  def main() = {

    // below parameters are for bvlc_reference_caffenet conv1 layer
    val batch = 256
    val kSize = 11 
    val imgSize = 227
    val stride = 4
    val padding = 0
    val outSize = (imgSize + 2 * padding - kSize) / stride + 1
    val numInChannel = 3
    val numOutChannel = 96

    // create random inputs
    val X = DenseMatrix.randf(batch, numInChannel*imgSize*imgSize)
    val W = DenseMatrix.randf(numOutChannel, numInChannel*kSize*kSize)
    val b = DenseVector.randf(outSize*outSize)

    val iter = if (args.length == 0) 10 else args(0).toInt
    println(iter + " iterations")

    // convolution layer
    var totalNumRows = 0
    var i = 0
    tic(X,W,b)
    while(i < iter) {
      val O = conv_fw_gemm_m(X,numInChannel,W,b,numOutChannel,kSize*kSize,kSize,kSize/2,outSize*outSize,outSize,stride,imgSize,imgSize)
      totalNumRows += O.numRows
      i += 1
    }
    toc(totalNumRows)
  }
}


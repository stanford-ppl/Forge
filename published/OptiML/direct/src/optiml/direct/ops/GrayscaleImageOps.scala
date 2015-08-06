package optiml.direct.ops

//import scala.reflect.{Manifest,SourceContext}
import optiml.direct._
import optiml.direct.ops._
import optiml.direct.typeclass._

/**
 * Operations
 */

trait GrayscaleImageOps extends Base {
  this: OptiML => 

  object GrayscaleImage {
    def apply(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext,__imp1: Overload16) = grayscaleimage_object_apply(__arg0)(__pos)
  }

  implicit def repToGrayscaleImageGrayscaleImageOpsCls(x: Rep[GrayscaleImage])(implicit __pos: SourceContext) = new GrayscaleImageGrayscaleImageOpsCls(x)(__pos)
  implicit def varToGrayscaleImageGrayscaleImageOpsCls(x: Var[GrayscaleImage])(implicit __pos: SourceContext) = new GrayscaleImageGrayscaleImageOpsCls(readVar(x))(__pos)

  class GrayscaleImageGrayscaleImageOpsCls(val self: Rep[GrayscaleImage])(implicit __pos: SourceContext) {
    def data(implicit __pos: SourceContext,__imp1: Overload1) = grayscaleimage_data(self)(__pos)
    def numRows(implicit __pos: SourceContext,__imp1: ROverload2) = { self.data.numRows }
    def numCols(implicit __pos: SourceContext,__imp1: ROverload2) = { self.data.numCols }
    def downsample(rowFactor: Rep[Int],colFactor: Rep[Int])(sample: (Rep[GrayscaleImage]) => Rep[Double])(implicit __pos: SourceContext) = grayscaleimage_downsample(self,rowFactor,colFactor,sample)(__pos)
    def convolve(kernel: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext) = grayscaleimage_convolve(self,kernel)(__pos)
    def windowedFilter(rowDim: Rep[Int],colDim: Rep[Int])(block: (Rep[GrayscaleImage]) => Rep[Double])(implicit __pos: SourceContext) = grayscaleimage_windowedfilter(self,rowDim,colDim,block)(__pos)
    def histogram(implicit __pos: SourceContext,__imp1: Overload2) = grayscaleimage_histogram(self)(__pos)
  }



  def grayscaleimage_object_apply(__arg0: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_data(self: Rep[GrayscaleImage])(implicit __pos: SourceContext): Rep[DenseMatrix[Double]]
  def grayscaleimage_downsample(self: Rep[GrayscaleImage],rowFactor: Rep[Int],colFactor: Rep[Int],sample: (Rep[GrayscaleImage]) => Rep[Double])(implicit __pos: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_convolve(self: Rep[GrayscaleImage],kernel: Rep[DenseMatrix[Double]])(implicit __pos: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_windowedfilter(self: Rep[GrayscaleImage],rowDim: Rep[Int],colDim: Rep[Int],block: (Rep[GrayscaleImage]) => Rep[Double])(implicit __pos: SourceContext): Rep[GrayscaleImage]
  def grayscaleimage_histogram(self: Rep[GrayscaleImage])(implicit __pos: SourceContext): Rep[DenseVector[Int]]
}

package optila.compiler.ops

import scala.tools.nsc.io._
import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.datastructures._
import ppl.delite.framework.ops.{DeliteOpsExp, DeliteCollectionOpsExp}
import ppl.delite.framework.Util._

import ppl.delite.framework.Config
import ppl.delite.framework.extern.codegen.scala.ScalaGenExternalBase
import ppl.delite.framework.extern.codegen.cuda.CudaGenExternalBase
import ppl.delite.framework.extern.codegen.cpp.CGenExternalBase
import ppl.delite.framework.extern.lib.{BLAS, cuBLAS}

import optima.shared._
import optima.shared.ops._
import optima.shared.typeclass._
import optima.compiler._
import optima.compiler.ops._

/**
 * Currently, native operations are entirely specified in external files. This has more boilerplate compared to using
 * a 'native' keyword in Forge to generate the DeliteOpExternal and mirror definitions, but allows us to call native
 * libraries essentially like macros, without changing the spec at all, or worrying about specifying things like guards
 * (if useBLAS(...)) or default implementations. Instead of distinguishing 'native' calls in the spec, any call can
 * be made native by adding the external override after-the-fact.
 *
 * This also provides us the freedom to use different external implementation strategies for each back-end target, without
 * changing Forge. However, once we have settled on a native lib generation plan for each target (and know the common components
 * that must be specified for the op), we probably want to use Forge to generate the boilerplate for all targets.
 */

// BLAS calls for matrix multiply and matrix-vector multiply have been moved to Delite
// These are here as placeholders if we ever want to add other BLAS calls to OptiLA

trait BLASOpsExp extends BLASOps with DenseMatrixOpsExp

trait ScalaGenBLASOps extends ScalaGenExternalBase {
  val IR: BLASOpsExp with OptiLAExp
}

trait CudaGenBLASOps extends CudaGenExternalBase {
  val IR: BLASOpsExp with OptiLAExp
}

trait OpenCLGenBLASOps

trait CGenBLASOps extends CGenExternalBase {
  val IR: BLASOpsExp with OptiLAExp
}

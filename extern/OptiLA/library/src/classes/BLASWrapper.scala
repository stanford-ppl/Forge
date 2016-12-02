package optila.library.classes

import scala.tools.nsc.io._
import reflect.Manifest;
import org.scala_lang.virtualized.SourceContext
import scala.math.Ordering.Implicits._
import scala.math.Numeric.Implicits._
import scala.virtualization.lms.common.{Base,BaseExp,EffectExp,BaseFatExp}
import scala.virtualization.lms.common.{ScalaGenBase,ScalaGenEffect,ScalaGenFat}
import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._
import optila.shared._
import optila.shared.ops._
import optila.shared.typeclass._
import optila.library._
import optila.library.classes._

trait BLASWrapper

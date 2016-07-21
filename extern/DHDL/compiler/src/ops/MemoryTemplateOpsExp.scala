package dhdl.compiler.ops

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, CGenEffect, DotGenEffect, MaxJGenEffect, Record}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}
import scala.collection.mutable.Set

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BlockRAM[T]
trait DHDLFIFO[T]
trait DHDLCAM[K,V]
trait DHDLVector[T]
trait CACHE[T]
trait Register[T]
trait DRAM[T]

trait DHDLPipeline
trait DHDLIndices

trait MemoryTemplateTypesExp extends MemoryTemplateTypes with BaseExp {
  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type FIFO[T] = DHDLFIFO[T]
  type CAM[K,V] = DHDLCAM[K,V]
  type Vector[T] = DHDLVector[T]
  type Cache[T] = CACHE[T]
  type Reg[T] = Register[T]

  type Pipeline = DHDLPipeline
  type Indices = DHDLIndices

  def isPipeline[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[DHDLPipeline])
  def isRegister[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Register[_]])
  def isBRAM[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[BlockRAM[_]])
  def isFIFO[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[DHDLFIFO[_]])
  def isCache[T:Manifest]    = isSubtype(manifest[T].runtimeClass, classOf[CACHE[_]])
  def isCAM[T:Manifest]      = isSubtype(manifest[T].runtimeClass, classOf[DHDLCAM[_,_]])

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def fifoManifest[T:Manifest]: Manifest[FIFO[T]] = manifest[DHDLFIFO[T]]
  def camManifest[K:Manifest,V:Manifest]: Manifest[CAM[K,V]] = manifest[DHDLCAM[K,V]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[DHDLVector[T]]
  def cacheManifest[T:Manifest]: Manifest[Cache[T]] = manifest[CACHE[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def pipelineManifest: Manifest[Pipeline] = manifest[DHDLPipeline]

  // TODO: Should be refined manifest? But how to know how many fields to fill in?
  def indicesManifest: Manifest[Indices] = manifest[DHDLIndices]
}

trait MemoryTemplateOpsExp extends MemoryTemplateTypesExp with ExternPrimitiveOpsExp with EffectExp with BRAMOpsExp {
  this: DHDLExp =>

  // --- Nodes
  case class Vector_from_list[T](elems: List[Exp[T]])(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  // --- Internal API
  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = reflectPure(Vector_from_list(elems))

  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Vector_from_list(elems) => reflectPure(Vector_from_list(f(elems))(e.mT, e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Vector_from_list(elems), u, es) => reflectMirrored(Reflect(Vector_from_list(f(elems))(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }
}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect with ScalaGenControllerTemplateOps {
  val IR: ControllerTemplateOpsExp with DHDLCodegenOps
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DHDLFIFO" => "scala.collection.mutable.Queue[" + remap(m.typeArguments(0)) + "]"
    case "DHDLCAM"  => "scala.collection.mutable.HashMap[" + remap(m.typeArguments(0)) + ", " + remap(m.typeArguments(1)) + "]"
    case "DHDLVector" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "CACHE"     => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DHDLPipeline" => "Unit"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Vector_from_list(elems) =>
      emitValDef(sym, "Array" + elems.map(quote).mkString("(", ",", ")"))

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMemoryTemplateOps extends CGenEffect {
  val IR: ControllerTemplateOpsExp with DHDLIdentifiers
  import IR._

  private def bitsToIntType(bits: Int) = {
    if (bits <= 8) "8"
    else if (bits <= 16) "16"
    else if (bits <= 32) "32"
    else "64"
  }

  private def bitsToFloatType(bits: Int) = {
    if (bits <= 32) "float"
    else "double"
  }

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => remapWithRef(m.typeArguments(0))
    case "DHDLVector" => remapWithRef(m.typeArguments(0))
    // case "DHDLFIFO" => ???
    case "Register" => remapWithRef(m.typeArguments(0))
    case "DRAM"     => remapWithRef(m.typeArguments(0))

    case "DHDLCounter" => "int32_t"
    case "DHDLCounterChain" => "int32_t*"
    case "DHDLPipeline" => "void"

    case "DHDLBit" => "bool"
    case "Signed" => ""
    case "Unsign" => "u"
    case "FixedPoint" => remap(m.typeArguments(0)) + "int" + bitsToIntType(remap(m.typeArguments(1)).toInt + remap(m.typeArguments(2)).toInt) + "_t"
    case "FloatPoint" => bitsToFloatType(remap(m.typeArguments(0)).toInt + remap(m.typeArguments(1)).toInt)
    case bx(n) => n
    case _ => super.remap(m)
  }
}

trait MaxJGenMemoryTemplateOps extends MaxJGenEffect with MaxJGenControllerTemplateOps{
  val IR: LoweredPipeOpsExp with ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with PipeOpsExp with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with ExternPrimitiveOpsExp with DHDLCodegenOps with NosynthOpsExp with MemoryAnalysisExp
          with DeliteTransform
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "DHDLVector" => "DFEVector<DFEVar>"
    case _ => super.remap(m)
  }

  // Current TileLd/St templates expect that LMem addresses are
  // statically known during graph build time in MaxJ. That can be
  // changed, but until then we have to assign LMem addresses
  // statically. Assigning each OffChipArray a 384MB chunk now
  val burstSize = 384
  var nextLMemAddr = burstSize * 1024 * 1024
  def getNextLMemAddr() = {
    val addr = nextLMemAddr
    nextLMemAddr += burstSize * 1024 * 1024;
    addr/burstSize
  }

	var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
		emittedSize = Set.empty[Exp[Any]]
    nextLMemAddr = burstSize * 1024 * 1024
		super.initializeGenerator(buildDir)
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case Offchip_new(size) =>
      alwaysGen {
        emitComment(s""" Offchip_new(${quote(size)}) {""")
        emit(s"""int ${quote(sym)} = ${getNextLMemAddr()};""")
        emitComment(s""" Offchip_new(${quote(size)}) }""")
      }

		case Offchip_load_cmd(mem, fifo, ofs, len, par) =>
      emitComment("Offchip load to fifo")

		case Offchip_store_cmd(mem, fifo, ofs, len, par) =>
      emitComment("Offchip store from fifo")

		case Reg_new(init) =>
      emitComment("Reg_new {")
			val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
			regType(sym) match {
				case Regular =>
          val parent = if (parentOf(sym).isEmpty) "Top" else quote(parentOf(sym).get) //TODO
					if (isDblBuf(sym)) {
						emit(s"""DblRegFileLib ${quote(sym)}_lib = new DblRegFileLib(this, $ts, ${quote(sym)}, ${parOf(sym)});""")
            val readstr = if (parOf(sym)>1) "readv" else "read"
            emit(s"""${maxJPre(sym)} ${quote(sym)} = ${quote(sym)}_lib.${readstr}()""")
           	emit(quote(sym) + "_lib.connectWdone(" + quote(writersOf(sym).head._1) + "_done);")
            readersOf(sym).foreach { case (r, _, _) =>
           	  emit(quote(sym) +"_lib.connectRdone(" + quote(r) + "_done);")
           	}
          } else {
            emit(s"""${quote(maxJPre(sym))} ${quote(sym)} = ${quote(ts)}.newInstance(this);""")
					}
				case ArgumentIn =>  // alwaysGen
        	alwaysGen {
          	emit(s"""DFEVar ${quote(sym)} = io.scalarInput("${quote(sym)}", $ts );""")
				  }
				case ArgumentOut => //emitted in reg_write
			}
      emitComment("} Reg_new")

    case e@Reg_read(reg) =>
      val pre = maxJPre(sym)
      val suffix = if (!controlNodeStack.isEmpty) {
        val Def(EatReflect(curPipe)) = controlNodeStack.top
        curPipe match {
            case n: ParPipeReduce[_,_] => if (n.acc == reg) "_delayed" else ""   // Use the delayed (stream-offset) version inside reduce
            case _ => ""
        }
      } else {
        ""
      }
      val regStr = quote(reg) + suffix
      emit(s"""$pre ${quote(sym)} = $regStr;""")

		case e@Reg_write(reg, value) =>
      emitComment("Reg_write {")
			val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
			if (isDblBuf(reg)) {
			 	emit(s"""${quote(reg)}_lib.write(${value}, ${quote(writersOf(reg).head._1)}_done);""")
      } else {
				regType(reg) match {
					case Regular =>
      		  val parent = if (parentOf(reg).isEmpty) "top" else quote(parentOf(reg).get) //TODO
      		  val rst = quote(parent) + "_rst_en"
					  if (writersOf(reg).isEmpty)
					  	throw new Exception(s"Reg ${quote(reg)} is not written by a controller, which is not supported at the moment")
					  val enSignalStr = writersOf(reg).head._1 match {
					  	case p@Def(EatReflect(pipe:Pipe_foreach)) => styleOf(p) match {
					  		case InnerPipe => quote(pipe.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@Def(EatReflect(pipe:Pipe_fold[_,_])) => styleOf(p) match {
					  		case InnerPipe => quote(pipe.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@Def(EatReflect(pipe:ParPipeReduce[_,_])) => styleOf(p) match {
					  		case InnerPipe => quote(pipe.cc) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@_ =>
                          throw new Exception(s"Reg ${quote(reg)} is written by non Pipe node ${p}")
					  }
            emit(s"""DFEVar ${quote(value)}_real = $enSignalStr ? ${quote(value)}:${quote(reg)}_delayed; // enable""")
            emit(s"""DFEVar ${quote(reg)} = Reductions.streamHold(${quote(value)}_real, ($rst | ${quote(writersOf(reg).head._1)}_redLoop_done));""")
// If nameOf(sym) is to be used, mix in NameOpsExp. This shouldn't have to be done by hand,
// so disabling using nameOf until it is fixed.
//            Console.println(s"""controlNodeStack = ${controlNodeStack}, reg = ${nameOf(reg)}, writersOf(reg) = ${writersOf(reg)}""")
            emit(s"""${quote(reg)}_delayed <== $rst ? ${quote(resetValue(reg))} : stream.offset(${quote(reg)}, -${quote(writersOf(reg).head._1)}_offset); // reset""")
				  case ArgumentIn => new Exception("Cannot write to ArgIn " + quote(reg) + "!")
				  case ArgumentOut =>
				 	  val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done" //TODO
      	  	emit(s"""io.scalarOutput("${quote(reg)}", ${quote(value)}, $ts, $controlStr);""")
				}
			}
      emitComment("} Reg_write")

    case Bram_new(size, zero) =>
      emitComment("Bram_new {")
			val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
      //TODO: does templete assume bram has 2 dimension?
      val dims = dimsOf(sym)
      val Def(d0) = dims(0)
      val Tpes_Int_to_fix(size0) = d0
      val size1 = if (dims.size == 1) {
          1
        } else {
          val Def(d1) = dims(1)
          val Tpes_Int_to_fix(v) = d1
          v
        }
      if (isDblBuf(sym)) {
        //readers.foreach { r =>
        //  if (!readerToMemMap.contains(r)) readerToMemMap += r -> Set[MemNode]()
        //  readerToMemMap(r) += n
        //}
        //rdoneSet += n -> Set()

        //emit(s"""// ${quote(sym)} has ${readersOf(sym).size} readers - ${readersOf(sym)}""")
        emit(s"""SMIO ${quote(sym)}_sm = addStateMachine("${quote(sym)}_sm", new ${quote(sym)}_DblBufSM(this));""")
        val dims = dimsOf(sym)
        emit(s"""DblBufKernelLib ${quote(sym)} = new DblBufKernelLib(this, ${quote(sym)}_sm,
          $size0, $size1, $ts, ${banks(sym)}, stride_TODO, ${readersOf(sym).size});""")
        if (writersOf(sym).isEmpty)
          throw new Exception(s"Bram ${quote(sym)} has no writer!")
          //  If writer is a unit Pipe, wait until parent is finished
        //val doneSig = n.getWriter().getOrElse(throw new Exception(s"BRAM $n has no writer")) match {
        //  case p: Pipe if p.isUnitPipe =>
        //    var writer = p.getParent()
        //    while (!(writer.isInstanceOf[Sequential] || writer.isInstanceOf[MetaPipeline])) {
        //      writer = writer.getParent()
        //    }
        //    s"${quote(writer)}_done"
        //  case _ =>
        //    s"${quote(n.getWriter())}_done"
        //}

      val doneSig = ""
        emit(s"""${quote(sym)}.connectWdone($doneSig);""")
      } else {
        emit(s"""BramLib ${quote(sym)} = new BramLib(this, ${quote(size0)}, ${quote(size1)}, ${ts}, ${banks(sym)}, 1 /* [TODO: stride from metadata */);""") // [TODO] Raghu: Stride from metadata
      }
      emitComment("} Bram_new")

    case Bram_load(bram, addr) =>
      emitComment("Bram_load {")
			val pre = maxJPre(bram)
      emit(s"""${pre} ${quote(sym)} = ${quote(bram)}.connectRport(${quote(addr)});""")
      if (isDblBuf(bram)) {
        if (parentOf(bram).isEmpty)
          throw new Exception("Bram (DblBuf)" + quote(bram) + " does not have a parent!")
        val p = parentOf(bram).get
        if (readersOf(bram).map(_._1).contains(p)) {
          //if (!rdoneSet(mem).contains(r)) { TODO
          emit(s"""${quote(bram)}.connectRdone(${p}_done);""")
          //rdoneSet(mem) += r
          //}
        }
      }
      // Handle if loading a composite type
      //n.compositeValues.zipWithIndex.map { t =>
      //  val v = t._1
      //  val idx = t._2
      //  visitNode(v)
      //  emit(s"""${quote(v)} <== ${quote(sym)}[$idx];""")
      //}
      emitComment("} Bram_load")

    case Bram_store(bram, addr, value) =>
      emitComment("Bram_store {")
			val dataStr = quote(value)
      if (isAccum(bram)) {
        val offsetStr = quote(writersOf(bram).head._1) + "_offset"
        val parentPipe = parentOf(bram).getOrElse(throw new Exception(s"Bram ${quote(bram)} does not have a parent!"))
        val parentCtr = parentPipe match {
          case Def(EatReflect(d)) => d match {
            case d:Pipe_fold[_,_] => d.cchain
            case d:Pipe_foreach => d.cchain
          }
          case p => throw new Exception(s"Unknown parent type ${p}!")
        }
        emit(s"""$bram.connectWport(stream.offset($addr, -$offsetStr),
          stream.offset($dataStr, -$offsetStr), ${quote(parentCtr)}_en_from_pipesm, start_TODO, stride_TODO);""")
      } else {
        // [TODO] Raghu: Current assumption is that this always returns the parent
        // writing to the BRAM. Is this always true? Confirm
        val writer = quote(writersOf(bram).head._1)
        emit(s"""${quote(bram)}.connectWport(${quote(addr)}, ${dataStr}, ${quote(writer)}_en, 0 /* start */, 1 /* stride */); // TODO: Hardcoded start and stride! Change after getting proper metadata""") //TODO
      }
      emitComment("} Bram_store")

    case _ => super.emitNode(sym, rhs)
  }
}

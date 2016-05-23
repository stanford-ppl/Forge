package dhdl.compiler.ops

import java.io.{File,FileWriter,PrintWriter}
import scala.virtualization.lms.common.{BaseExp, EffectExp, ScalaGenEffect, DotGenEffect, MaxJGenEffect}
import scala.virtualization.lms.internal.{Traversal}
import scala.reflect.{Manifest,SourceContext}
import ppl.delite.framework.transform.{DeliteTransform}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BlockRAM[T]
trait DHDLVector[T]
trait Register[T]
trait DRAM[T]

trait DHDLPipeline
trait DHDLIndices

trait MemoryTemplateTypesExp extends MemoryTemplateTypes with BaseExp {
  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type Vector[T] = DHDLVector[T]
  type Reg[T] = Register[T]

  type Pipeline = DHDLPipeline
  type Indices = DHDLIndices

  def isPipeline[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[DHDLPipeline])
  def isRegister[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[Register[_]])
  def isBRAM[T:Manifest]     = isSubtype(manifest[T].runtimeClass, classOf[BlockRAM[_]])

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def vectorManifest[T:Manifest]: Manifest[Vector[T]] = manifest[DHDLVector[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def pipelineManifest: Manifest[Pipeline] = manifest[DHDLPipeline]

  // TODO: Should be refined manifest? But how to know how many fields to fill in?
  def indicesManifest: Manifest[Indices] = manifest[DHDLIndices]
}

trait MemoryTemplateOpsExp extends MemoryTemplateTypesExp with ExternPrimitiveOpsExp with EffectExp with BRAMOpsExp {
  this: DHDLExp =>

  // --- Nodes
  case class Vector_from_list[T](elems: List[Exp[T]])(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  // TODO: Can generalize to Mem[T] rather than BRAM?
  case class Bram_load_vector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    cchain: Exp[CounterChain],
    inds:   List[Sym[FixPt[Signed,B32,B0]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Vector[T]]

  case class Bram_store_vector[T](
    bram:   Exp[BRAM[T]],
    ofs:    Exp[FixPt[Signed,B32,B0]],
    vec:    Exp[Vector[T]],
    cchain: Exp[CounterChain],
    inds:   List[Sym[FixPt[Signed,B32,B0]]]
  )(implicit val mT: Manifest[T], val ctx: SourceContext) extends Def[Unit]


  // --- Internal API
  def vector_from_list[T:Manifest](elems: List[Rep[T]])(implicit ctx: SourceContext): Rep[Vector[T]] = reflectPure(Vector_from_list(elems))

  def bram_load_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], len: Rep[FixPt[Signed,B32,B0]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Vector[T]] = {
    val inds = List.fill(lenOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }
    val ofs = calcAddress(offsets, dimsOf(bram))
    val vec = reflectPure(Bram_load_vector(bram, ofs, cchain, inds))
    accessIndicesOf(vec) = offsets
    vec
  }

  def bram_store_vector[T:Manifest](bram: Rep[BRAM[T]], offsets: List[Rep[FixPt[Signed,B32,B0]]], vec: Rep[Vector[T]], cchain: Rep[CounterChain])(implicit ctx: SourceContext): Rep[Unit] = {
    val inds = List.fill(lenOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }
    val ofs = calcAddress(offsets, dimsOf(bram))
    val st = reflectEffect(Bram_store_vector(bram, ofs, vec, cchain, inds), Write(List(bram.asInstanceOf[Sym[BRAM[T]]])))
    accessIndicesOf(st) = offsets
    st
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@Vector_from_list(elems) => reflectPure(Vector_from_list(f(elems))(e.mT, e.ctx))(mtype(manifest[A]), pos)
    case Reflect(e@Vector_from_list(elems), u, es) => reflectMirrored(Reflect(Vector_from_list(f(elems))(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Bram_load_vector(b,o,c,i) => reflectPure(Bram_load_vector(f(b),f(o),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@Bram_load_vector(b,o,c,i), u, es) => reflectMirrored(Reflect(Bram_load_vector(f(b),f(o),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case e@Bram_store_vector(b,o,v,c,i) => reflectPure(Bram_store_vector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx))(mtype(manifest[A]),pos)
    case Reflect(e@Bram_store_vector(b,o,v,c,i), u, es) => reflectMirrored(Reflect(Bram_store_vector(f(b),f(o),f(v),f(c),i)(e.mT,e.ctx), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)

    case _ => super.mirror(e, f)
  }

  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => syms(e.bram) ::: syms(e.ofs) ::: syms(e.cchain)
    case e: Bram_store_vector[_] => syms(e.bram) ::: syms(e.ofs) ::: syms(e.vec) ::: syms(e.cchain)
    case _ => super.syms(e)
  }
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => readSyms(e.bram) ::: readSyms(e.ofs) ::: readSyms(e.cchain)
    case e: Bram_store_vector[_] => readSyms(e.bram) ::: readSyms(e.ofs) ::: readSyms(e.vec) ::: readSyms(e.cchain)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: Bram_load_vector[_] => freqNormal(e.bram) ::: freqNormal(e.ofs) ::: freqNormal(e.cchain)
    case e: Bram_store_vector[_] => freqNormal(e.bram) ::: freqNormal(e.ofs) ::: freqNormal(e.vec) ::: freqNormal(e.cchain)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_load_vector[_] => e.inds
    case e: Bram_store_vector[_] => e.inds
    case _ => super.boundSyms(e)
  }

  // --- Aliasing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: Bram_store_vector[_] => Nil
    case _ => super.aliasSyms(e)
  }
}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect with ScalaGenControllerTemplateOps {
  val IR: ControllerTemplateOpsExp with DHDLCodegenOps
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DHDLVector" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DHDLPipeline" => "Unit"
    case _ => super.remap(m)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Vector_from_list(elems) =>
      emitValDef(sym, "Array" + elems.map(quote).mkString("(", ",", ")"))

    // TODO: Unrolling instead of codegen rule for these two!
    case e@Bram_load_vector(bram,ofs,cchain,inds) =>
      if (dimsOf(sym).length > 1) throw new Exception("Vectors above 1 dimension unsupported")
      val len = dimsOf(sym).map{case Exact(c) => c}.reduce(_*_)
      stream.println("val " + quote(sym) + " = new Array[" + remap(e.mT) + "](" + len + ")")
      emitNestedLoop(inds, cchain){
        stream.println("val bramAddr = " + quote(ofs) + ".toInt + " + quote(inds.head) + ".toInt")
        stream.println(quote(sym) + "(" + quote(inds.head) + ".toInt) = " + quote(bram) + "(bramAddr)")
      }

    case e@Bram_store_vector(bram,ofs,vec,cchain,inds) =>
      if (dimsOf(vec).length > 1) throw new Exception("Vectors above 1 dimension unsupported")
      emitNestedLoop(inds, cchain){
        stream.println("val bramAddr = " + quote(ofs) + ".toInt + " + quote(inds.head) + ".toInt")
        stream.println(quote(bram) + "(bramAddr) = " + quote(vec) + "(" + quote(inds.head) + ".toInt)")
      }
      emitValDef(sym, "()")

    case _ => super.emitNode(sym, rhs)
  }
}




trait DotGenMemoryTemplateOps extends DotGenEffect with DotGenControllerTemplateOps{
	val IR: ControllerTemplateOpsExp with OffChipMemOpsExp with DHDLCodegenOps with RegOpsExp
            with DHDLIdentifiers with DeliteTransform

  import IR._

	var emittedSize = Set.empty[Exp[Any]]
  override def initializeGenerator(buildDir:String): Unit = {
		emittedSize = Set.empty[Exp[Any]]
		super.initializeGenerator(buildDir)
	}

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case Offchip_new(size) =>
			/* Special case to hand nodes producing size of offchip outside hardware scope. Not actual
       * codegen to Offchip_new */
			def hackGen(x: Exp[Any]): Unit = x match {
				case Def(EatReflect(_:Reg_new[_])) => // Nothing
				case ConstFix(_) => // Nothing
				case ConstFlt(_) => // Nothing
				case Def(d) =>
					alwaysGen {
						emitNode(x.asInstanceOf[Sym[Any]], d)
					}
					syms(d).foreach{ s => s match {
							case _ => hackGen(s)
						}
					}
				case _ => // Nothing
			}
			if (!emittedSize.contains(size)) {
				hackGen(size)
				emittedSize = emittedSize + size
			}
			super.emitNode(sym, rhs)

    case _ => super.emitNode(sym, rhs)
  }
}




trait MaxJGenMemoryTemplateOps extends MaxJGenEffect with MaxJGenControllerTemplateOps{
  val IR: ControllerTemplateOpsExp with TpesOpsExp with ParallelOpsExp
          with OffChipMemOpsExp with RegOpsExp with ExternCounterOpsExp
          with DHDLCodegenOps with DeliteTransform
  import IR._

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
        emit(s"""int ${quote(sym)} = ${getNextLMemAddr()};""")
      }

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
           	emit(quote(sym) + "_lib.connectWdone(" + quote(writerOf(sym).get._1) + "_done);")
            readersOf(sym).foreach { case (r, _, _) =>
           	  emit(quote(sym) +"_lib.connectRdone(" + quote(r) + "_done);")
           	}
          } else {
            emit(s"""${quote(maxJPre(sym))} ${quote(sym)} = ${quote(ts)}.newInstance(this);""")
					}
				case ArgumentIn =>  // alwaysGen
        	alwaysGen {
          	emit(s"""DFEVar ${quote(sym)} = io.scalarInput(${quote(sym)} , $ts );""")
				  }
				case ArgumentOut => //emitted in reg_write
			}
      emitComment("} Reg_new")

		case e@Reg_write(reg, value) =>
      emitComment("Reg_write {")
			val ts = tpstr(parOf(reg))(reg.tp.typeArguments.head, implicitly[SourceContext])
			if (isDblBuf(reg)) {
			 	emit(s"""${quote(reg)}_lib.write(${value}, ${quote(writerOf(reg).get._1)}_done);""")
      } else {
				regType(reg) match {
					case Regular =>
      		  val parent = if (parentOf(reg).isEmpty) "top" else quote(parentOf(reg).get) //TODO
      		  val rst = quote(parent) + "_rst_en"
					  if (writerOf(reg).isEmpty)
					  	throw new Exception(s"Reg ${quote(reg)} is not written by a controller, which is not supported at the moment")
					  val enSignalStr = writerOf(reg).get._1 match {
					  	case p@Def(EatReflect(e:Pipe_foreach)) => styleOf(p) match {
					  		case Fine => quote(e.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@Def(EatReflect(e:Pipe_fold[_,_])) => styleOf(p) match {
					  		case Fine => quote(e.cchain) + "_en_from_pipesm"
					  		case _ => quote(p) + "_en"
					  	}
					  	case p@_ => val Def(d) = p
                          throw new Exception(s"Reg ${quote(reg)} is written by non Pipe node ${p} def:${d}")
					  }
      		  emit(s"""DFEVar ${quote(value)}_real = $enSignalStr ? ${quote(value)}:${quote(reg)}; // enable""")
      		  emit(s"""DFEVar ${quote(reg)}_hold = Reductions.streamHold(${quote(value)}_real, ($rst | ${quote(writerOf(reg).get._1)}_redLoop_done));""")
      		  emit(s"""${quote(reg)} <== $rst ? constant.var($ts, ${quote(resetValue(reg))}):stream.offset(${quote(reg)}_hold, -${quote(writerOf(reg).get._1)}_offset); // reset""")
				  case ArgumentIn => new Exception("Cannot write to Argument Out! " + quote(reg))
				  case ArgumentOut =>
				 	  val controlStr = if (parentOf(reg).isEmpty) s"top_done" else quote(parentOf(reg).get) + "_done" //TODO
      	  	emit(s"""io.scalarOutput(${quote(reg)}, ${quote(value)}, $ts, $controlStr);""")
				}
			}
      emitComment("} Reg_write")

    case Bram_new (size, zero) =>
      emitComment("Bram_new {")
			val ts = tpstr(parOf(sym))(sym.tp.typeArguments.head, implicitly[SourceContext])
      //TODO: does templete assume bram has 2 dimension?
      val dims = dimsOf(sym)
      val size0 = dims(0)
      val size1 = if (dims.size==1) 1 else dims(1)
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
        if (writerOf(sym).isEmpty)
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
        emit(s"""BramLib ${quote(sym)} = new BramLib(this, ${quote(size0)}, ${size1}, ${ts}, ${banks(sym)}, stride_TODO);""")
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
        val offsetStr = quote(writerOf(bram).get._1) + "_offset"
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
         emit(s"""${quote(bram)}.connectWport(${quote(addr)}, ${dataStr}, ${quote(parentOf(bram).get)}_en, start_TODO, stride_TODO ;""") //TODO
      }
      emitComment("} Bram_store")

    case _ => super.emitNode(sym, rhs)
  }
}

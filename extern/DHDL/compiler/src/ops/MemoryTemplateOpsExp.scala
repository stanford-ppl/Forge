package dhdl.compiler.ops

import scala.virtualization.lms.common.{EffectExp, ScalaGenEffect ,DotGenEffect, MaxJGenEffect, Record}
import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

trait BlockRAM[T]
trait Register[T]
trait DRAM[T]

trait DHDLCounter
trait DHDLCounterChain
trait DHDLPipeline

trait DHDLBit
trait FixedPoint[SIGN,INT,FRAC]
trait FloatPoint[SIG,EXP]
trait DHDLIndices

// Stub (nothing here for now)
trait TypeInspectionOpsExp extends TypeInspectionCompilerOps {
  this: DHDLExp =>

  def isFixPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FixedPoint[_,_,_]])
  def isFltPtType[T:Manifest] = isSubtype(manifest[T].runtimeClass, classOf[FloatPoint[_,_]])
  def isBitType[T:Manifest]   = isSubtype(manifest[T].runtimeClass, classOf[DHDLBit])
}

trait MemoryTemplateTypesExp extends MemoryTemplateTypes {
  type OffChipMem[T] = DRAM[T]
  type BRAM[T] = BlockRAM[T]
  type Reg[T] = Register[T]

  type Counter = DHDLCounter
  type CounterChain = DHDLCounterChain
  type Pipeline = DHDLPipeline
  type Indices = DHDLIndices

  type Bit = DHDLBit
  type FixPt[SIGN,INT,FRAC] = FixedPoint[SIGN,INT,FRAC]
  type FltPt[SIG,EXP]       = FloatPoint[SIG,EXP]

  def offchipMemManifest[T:Manifest]: Manifest[OffChipMem[T]] = manifest[DRAM[T]]
  def bramManifest[T:Manifest]: Manifest[BRAM[T]] = manifest[BlockRAM[T]]
  def regManifest[T:Manifest]: Manifest[Reg[T]] = manifest[Register[T]]
  def counterManifest: Manifest[Counter] = manifest[DHDLCounter]
  def counterChainManifest: Manifest[CounterChain] = manifest[DHDLCounterChain]
  def pipelineManifest: Manifest[Pipeline] = manifest[DHDLPipeline]

  // TODO: Should be refined manifest? But how to know how many fields to fill in?
  def indicesManifest: Manifest[Indices] = manifest[DHDLIndices]

  def fixManifest[S:Manifest,I:Manifest,F:Manifest] = manifest[FixedPoint[S,I,F]]
  def fltManifest[G:Manifest,E:Manifest] = manifest[FloatPoint[G,E]]
  def bitManifest: Manifest[Bit] = manifest[DHDLBit]
}

trait MemoryTemplateOpsExp extends MemoryTemplateCompilerOps with MemoryTemplateTypesExp with EffectExp {
  this: DHDLExp =>

  // --- Nodes
  case class TileTransfer[T:Manifest](
    mem:      Rep[OffChipMem[T]], // Offchip memory array
    local:    Rep[BRAM[T]],       // Local memory (BRAM)
    strides:  List[Rep[FixPt[Signed,B32,B0]]],   // Dimensions converted to strides for offchip memory
    memOfs:   Rep[FixPt[Signed,B32,B0]],         // Offset into offchip memory
    tileDims: List[Int],          // Tile dimensions
    cchain:   Rep[CounterChain],  // Counter chain for copy
    iters:    List[Sym[FixPt[Signed,B32,B0]]],   // Bound iterator variables
    store:    Boolean             // Is this transfer a store (true) or a load (false)
  )(implicit ctx: SourceContext) extends Def[Unit] {
    val mT = manifest[T]
  }

  // --- Internal API
  def tile_transfer[T:Manifest](mem: Rep[OffChipMem[T]], local: Rep[BRAM[T]], strides: List[Rep[FixPt[Signed,B32,B0]]], memOfs: Rep[FixPt[Signed,B32,B0]], tileDims: List[Int], cchain: Rep[CounterChain], store: Boolean)(implicit ctx: SourceContext): Rep[Unit] = {
    val iters = List.fill(sizeOf(cchain)){ fresh[FixPt[Signed,B32,B0]] }

    if (store) reflectWrite(mem)(TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters,store))
    else       reflectWrite(local)(TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters,store))
  }


  // --- Mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case e@TileTransfer(m,l,s,o,t,c,i,st) => reflectPure(TileTransfer(f(m),f(l),f(s),f(o),t,f(c),i,st)(e.mT,pos))(mtype(manifest[A]), pos)
    case Reflect(e@TileTransfer(m,l,s,o,t,c,i,st), u, es) => reflectMirrored(Reflect(TileTransfer(f(m),f(l),f(s),f(o),t,f(c),i,st)(e.mT,pos), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
    case _ => super.mirror(e, f)
  }

  // --- Dependencies
  override def syms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => syms(e.mem) ::: syms(e.local) ::: syms(e.strides) ::: syms(e.memOfs) ::: syms(e.cchain)
    case _ => super.syms(e)
  }

  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case Pipe_foreach(chain, func, _) => readSyms(chain) ::: readSyms(func)
    case _ => super.readSyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case e: TileTransfer[_] => freqNormal(e.mem) ::: freqNormal(e.local) ::: freqNormal(e.strides) ::: freqNormal(e.memOfs) ::: freqNormal(e.cchain)

    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => e.iters
    case _ => super.boundSyms(e)
  }

  // --- Aliasing
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case e: TileTransfer[_] => Nil
    case _ => super.aliasSyms(e)
  }

}

// Defines type remappings required in Scala gen (should be same as in library)
trait ScalaGenMemoryTemplateOps extends ScalaGenEffect with ScalaGenPipeTemplateOps {
  val IR: PipeTemplateOpsExp with DHDLIdentifiers
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"

    case "DHDLCounter" => "FixedPointRange"
    case "DHDLCounterChain" => "Array[FixedPointRange]"
    case "DHDLPipeline" => "Unit"

    case "DHDLBit" => "Boolean"
    case "FixedPoint" => "FixedPoint"
    case "FloatPoint" => "FloatPoint"
    case _ => super.remap(m)
  }

  // Note that tileDims are not fixed point values yet - they're just integers
  private def localDimsToStrides(dims: List[Int]) = List.tabulate(dims.length){d =>
    if (d == dims.length - 1) 1
    else dims.drop(d + 1).reduce(_*_)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters, store) => // Load
      emitNestedLoop(iters, cchain) {
        val offaddr = (iters.zip(strides).map{case (i,s) => quote(i) + "*" + quote(s) } :+ quote(memOfs)).mkString(" + ")
        stream.println("val offaddr = " + offaddr)

        val localStrides = localDimsToStrides(tileDims).map(k => "FixedPoint[Signed,B32,B0](" + k + ")")
        val localAddr = iters.zip(localStrides).map{ case (i,s) => quote(i) + "*" + quote(s) }.mkString(" + ")
        stream.println("val localaddr = " + localAddr)

        if (store)
          stream.println(quote(mem) + "(offaddr.toInt) = " + quote(local) + "(localaddr.toInt)")
        else
          stream.println(quote(local) + "(localaddr.toInt) = " + quote(mem) + "(offaddr.toInt)")
      }

    case _ => super.emitNode(sym, rhs)
  }
}

trait DotGenMemoryTemplateOps extends DotGenEffect with DotGenPipeTemplateOps {
  val IR: PipeTemplateOpsExp with DHDLIdentifiers
  import IR._

  // Note that tileDims are not fixed point values yet - they're just integers
  private def localDimsToStrides(dims: List[Int]) = List.tabulate(dims.length){d =>
    if (d == dims.length - 1) 1
    else dims.drop(d + 1).reduce(_*_)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters, store) => // Load
			val l = "offld_" + quote(sym).split("_")(1)
      emit(s"""${quote(sym)} [ label=$l shape="rectangle" style="rounded, filled" fillcolor="white"
				color="black"]""")
			emit(s"""offchip -> $sym""")
			emit(s"""sym -> $bram""")
			/*
			tileDims.foreach{dim => 
			 $dim -> $sym [label="offdim"]
			 }
			 start.foreach{s =>
			 $s -> $sym [label="start"]
			}
			*/
			//TODO:
			/*
      emitNestedLoop(iters, cchain) {
        val offaddr = (iters.zip(strides).map{case (i,s) => quote(i) + "*" + quote(s) } :+ quote(memOfs)).mkString(" + ")
        stream.println("val offaddr = " + offaddr)

        val localStrides = localDimsToStrides(tileDims).map(k => "FixedPoint[Signed,B32,B0](" + k + ")")
        val localAddr = iters.zip(localStrides).map{ case (i,s) => quote(i) + "*" + quote(s) }.mkString(" + ")
        stream.println("val localaddr = " + localAddr)

        if (store)
          stream.println(quote(mem) + "(offaddr.toInt) = " + quote(local) + "(localaddr.toInt)")
        else
          stream.println(quote(local) + "(localaddr.toInt) = " + quote(mem) + "(offaddr.toInt)")
				*/
      }

    case _ => super.emitNode(sym, rhs)
  }
}

trait MaxJGenMemoryTemplateOps extends MaxJGenEffect with MaxJGenPipeTemplateOps {
  val IR: PipeTemplateOpsExp with DHDLIdentifiers
  import IR._

  override def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "BlockRAM" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "Register" => "Array[" + remap(m.typeArguments(0)) + "]"
    case "DRAM"     => "Array[" + remap(m.typeArguments(0)) + "]"

    case "DHDLCounter" => "FixedPointRange"
    case "DHDLCounterChain" => "Array[FixedPointRange]"
    case "DHDLPipeline" => "Unit"

    case "DHDLBit" => "Boolean"
    case "FixedPoint" => "FixedPoint"
    case "FloatPoint" => "FloatPoint"
    case _ => super.remap(m)
  }

  // Note that tileDims are not fixed point values yet - they're just integers
  private def localDimsToStrides(dims: List[Int]) = List.tabulate(dims.length){d =>
    if (d == dims.length - 1) 1
    else dims.drop(d + 1).reduce(_*_)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case TileTransfer(mem,local,strides,memOfs,tileDims,cchain,iters, store) => // Load
    case _ => super.emitNode(sym, rhs)
  }
}

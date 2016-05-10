package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._

// Struct for passing around completed FPGA area analysis results
case class FPGAResourceSummary(
  alms: Int = 0,
  regs: Int = 0,
  dsps: Int = 0,
  bram: Int = 0,
  streams: Int = 0
) {
  def <(that: FPGAResourceSummary)  = this.alms <  that.alms && this.regs <  that.regs && this.dsps <  that.dsps && this.bram <  that.bram && this.streams <  that.streams
  def <=(that: FPGAResourceSummary) = this.alms <= that.alms && this.regs <= that.regs && this.dsps <= that.dsps && this.bram <= that.bram && this.streams <= that.streams
  def >(that: FPGAResourceSummary)  = this.alms >  that.alms && this.regs >  that.regs && this.dsps >  that.dsps && this.bram >  that.bram && this.streams >  that.streams
  def >=(that: FPGAResourceSummary) = this.alms >= that.alms && this.regs >= that.regs && this.dsps >= that.dsps && this.bram >= that.bram && this.streams >= that.streams
}

// Class for operating on intermediate FPGA resource counts
case class FPGAResources(
  lut7: Int = 0,
  lut6: Int = 0,
  lut5: Int = 0,
  lut4: Int = 0,
  lut3: Int = 0,
  mem64: Int = 0,
  mem32: Int = 0,
  mem16: Int = 0,
  regs: Int = 0,
  dsps: Int = 0,
  bram: Int = 0,
  mregs: Int = 0,
  streams: Int = 0
) {
  def +(that: FPGAResources) = FPGAResources(
    lut7 = this.lut7 + that.lut7,
    lut6 = this.lut6 + that.lut6,
    lut5 = this.lut5 + that.lut5,
    lut4 = this.lut4 + that.lut4,
    lut3 = this.lut3 + that.lut3,
    mem64 = this.mem64 + that.mem64,
    mem32 = this.mem32 + that.mem32,
    mem16 = this.mem16 + that.mem16,
    regs = this.regs + that.regs,
    dsps = this.dsps + that.dsps,
    bram = this.bram + that.bram,
    mregs = this.mregs + that.mregs,
    streams = this.streams + that.streams
  )

  // HACK: Don't duplicate BRAM in inner loops (bank instead)
  def replicated(x: Int, inner: Boolean) = {
    val bramNew = if (inner) bram else x*bram
    val mregsNew = if (inner) mregs else x*mregs
    FPGAResources(x*lut7,x*lut6,x*lut5,x*lut4,x*lut3,x*mem64,x*mem32,x*mem16,x*regs,x*dsps,bramNew,mregsNew,x*streams)
  }

  def isNonzero: Boolean = lut7 > 0 || lut6 > 0 || lut5 > 0 || lut4 > 0 || lut3 > 0 ||
                           mem64 > 0 || mem32 > 0 || mem16 > 0 || regs > 0 || dsps > 0 || bram > 0 || mregs > 0 || streams > 0

  override def toString() = s"lut3=$lut3, lut4=$lut4, lut5=$lut5, lut6=$lut6, lut7=$lut7, mem16=$mem16, mem32=$mem32, mem64=$mem64, regs=$regs, dsps=$dsps, bram=$bram, mregs=$mregs, streams=$streams"

  def toArray: Array[Int] = Array(lut7,lut6,lut5,lut4,lut3,mem64,mem32,mem16,regs+mregs,dsps,bram)
}

object NoArea extends FPGAResources()


// TODO: Should get some of this from loading a file rather than hardcoding
// All numbers here are from Stratix V profiling
trait AreaModel extends PipeStageToolsExp {
  this: DHDLExp =>

  private var silentModel = false
  private def warn(x: => String) { if (!silentModel) stageWarn(x) }
  def silenceAreaModel() { silentModel = true }

  /**
   * Returns the area resources for a delay line with the given width (in bits) and length (in cycles)
   * Models delays as registers for short delays, BRAM for long ones
   **/
  def areaOfDelayLine(width: Int, length: Int, par: Int): FPGAResources = {
    //System.out.println(s"Delay line: w = $width x l = $length (${width*length}) ")
    val nregs = width*length
    if (nregs < 256) FPGAResources(regs = nregs*par)
    else             areaOfBRAM(width*par, length, 1, false)
  }
  def areaOfDelayLine(width: Int, length: Long, par: Int): FPGAResources = {
    if(length > Int.MaxValue) throw new Exception(s"Casting delay line length to Int would result in overflow")
    areaOfDelayLine(width, length.toInt, par)
  }

  private def areaOfMemWord(nbits: Int) = {
    val m64 = nbits/64
    val m32 = (nbits - m64*64)/32
    val m16 = (nbits - m64*64 - m32*32)/16
    FPGAResources(mem64=m64, mem32=m32, mem16=m16)
  }

  private def areaOfArg(nbits: Int) = FPGAResources(regs=nbits) //3*nbits/2)

  // Set to 0 or lower to disable
  val REG_RAM_DEPTH = 5  // Non-inclusive

  /**
   * Area resources required for a BRAM with word size nbits, and with given depth,
   * number of banks, and double buffering
   **/
  private def areaOfBRAM(nbits: Int, depth: Int, banks: Int, dblBuf: Boolean) = {
    // Physical depth for given word size for horizontally concatenated RAMs
    val wordDepth = if      (nbits == 1)  16384
                    else if (nbits == 2)  8192
                    else if (nbits <= 5)  4096
                    else if (nbits <= 10) 2048
                    else if (nbits <= 20) 1024
                    else                  512

    // Number of horizontally concatenated RAMs required to implement given word
    val width = if (nbits > 40) Math.ceil( nbits / 40.0 ).toInt else 1

    val bankDepth = Math.ceil(depth.toDouble/banks)       // Word depth per bank

    if (bankDepth < REG_RAM_DEPTH) {
      //System.out.println("RAM can be implemented using registers")

      val regs = width * bankDepth.toInt * banks
      if (dblBuf) FPGAResources(lut3=2*nbits, regs=2*nbits, mregs = regs*2)
      else        FPGAResources(mregs = regs)
    }
    else {
      val ramDepth = Math.ceil(bankDepth/wordDepth).toInt   // Number of rams needed per bank
      val rams = width * ramDepth * banks

      if (dblBuf) FPGAResources(lut3=2*nbits, regs=2*nbits, bram = rams*2)
      else        FPGAResources(bram = rams)
    }
  }

  def areaOfMetapipe(n: Int) = FPGAResources(
    lut4 = (11*n*n + 45*n)/2 + 105,  // 0.5(11n^2 + 45n) + 105
    regs = (n*n + 3*n)/2 + 35        // 0.5(n^2 + 3n) + 35
  )
  def areaOfSequential(n: Int) = FPGAResources(lut4=7*n+40, regs=2*n+35)


  /**
   * Returns the area of the given expression, e.
   * Note that this does not include the area of any function bodies
   *   inReduce  - in a tight reduce-accumulate loop (generated hardware often optimized for latency)
   *   inHwScope - used to ignore nodes outside the block being implemented in hardware
   **/
  def areaOf(e: Exp[Any], inReduce: Boolean, inHwScope: Boolean) = {
    val area = e match {
      case Def(d) if !inHwScope => areaOfNodeOutOfScope(e, d)
      case Def(d) if inReduce  => areaOfNodeInReduce(e, d)
      case Def(d) if !inReduce => areaOfNode(e, d)
      case _ => NoArea  // Bound args and constants accounted for elsewhere
    }
    //if (area.bram > 0) System.out.println(s"  $e (reduce = $inReduce): $area")

    area
  }


  private def areaOfNodeOutOfScope(s: Exp[Any], d: Def[Any]): FPGAResources = d match {
    case _:Reg_new[_] if regType(s) != Regular => areaOfNode(s, d)
    case Reflect(d,_,_) => areaOfNodeOutOfScope(s,d)
    case _ => NoArea
  }

  /**
   * Returns the area resources for the given node when specialized for tight update cycles
   * Accumulator calculation+update is often generated as a special case to minimize latencies in tight cycles
   **/
  private def areaOfNodeInReduce(s: Exp[Any], d: Def[Any]): FPGAResources = d match {
    case FltPt_Add(_,_) =>
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=1606,mem16=50) // More registers

    case Reflect(d,_,_) => areaOfNodeInReduce(s,d)
    case _ => areaOfNode(s, d)
  }


  def areaOfCounterRegs(lhs: Exp[Any], cchain: Exp[CounterChain]): FPGAResources = {
    if (isOuterLoop(lhs) && styleOf(lhs) == Coarse) {
      val N = nStages(lhs) - 1          // Number of stages needed for delay
      val P = parOf(cchain).reduce(_+_) // Number of duplications per counter
      FPGAResources(lut3=N*P*32, regs = 4*N*P*32) // TODO: Hardcoded 32 bit index sizes
    }
    else NoArea
  }

  // HACK: Need better way of checking const multiplications here
  def areaOfConstMult(c: Int, nbits: Int) = {
    FPGAResources(lut3 = nbits, regs = 2*nbits)
  }

  /**
   * Returns the area resources for the given node
   **/
  private def areaOfNode(s: Exp[Any], d: Def[Any]): FPGAResources = s match {
    case Fixed(_) => areaOfMemWord(nbits(s))
    case Exact(_) => areaOfMemWord(nbits(s))
    case _ => d match {
    case ConstBit(_) => FPGAResources(lut3=1,regs=1)
    case ConstFix(_) => areaOfMemWord(nbits(s))
    case ConstFlt(_) => areaOfMemWord(nbits(s))

    case Reg_new(_) if regType(s) == ArgumentIn  => areaOfArg(nbits(s))
    case Reg_new(_) if regType(s) == ArgumentOut => areaOfArg(nbits(s))

    case Reg_new(_) if regType(s) == Regular =>
      if (isDblBuf(s)) FPGAResources(lut3 = nbits(s), regs = 4*nbits(s)) // TODO: Why 4?
      else             FPGAResources(regs = nbits(s))

    case e@Bram_new(depth, _) =>
      val depBound = bound(depth).getOrElse{stageError("Cannot resolve bound of BRAM size"); 1.0}.toInt
      areaOfBRAM(nbits(e._mT), depBound, banks(s), isDblBuf(s))

    // TODO: These are close but still need some refining
    case e@Bram_load(ram, _) =>
      val decode = if (isPow2(banks(ram))) 0 else Math.ceil(Math.log(banks(ram))).toInt
      val bits = nbits(e._mT)
      FPGAResources(lut3=decode+bits, regs=decode+bits)

    case e@Bram_store(ram, _, _) =>
      val decode = if (isPow2(banks(ram))) 0 else Math.ceil(Math.log(banks(ram))).toInt
      val bits = nbits(e._mT)
      FPGAResources(lut3=decode+bits, regs=decode+bits)

    // TODO: Seems high, confirm
    case _:Counter_new => FPGAResources(lut3=106,regs=67)
    case _:Counterchain_new => NoArea

    // TODO: Have to get numbers for non-32 bit multiplies and divides
    case FixPt_Neg(_)   => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Add(_,_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Sub(_,_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))

    case FixPt_Mul(Exact(c),_) => areaOfConstMult(c.toInt, nbits(s)) // HACK
    case FixPt_Mul(_,Exact(c)) => areaOfConstMult(c.toInt, nbits(s)) // HACK
    case FixPt_Mul(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(dsps = 2)

    case FixPt_Div(Exact(_),_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Div(_,Exact(_)) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      if (sign(s)) FPGAResources(lut3=1192,lut5=2,regs=2700)
      else         FPGAResources(lut3=1317,lut5=6,regs=2900)

    case FixPt_Mod(Exact(_),_) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Mod(_,Exact(_)) => FPGAResources(lut3 = nbits(s), regs = nbits(s))
    case FixPt_Mod(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      if (sign(s)) FPGAResources(lut3=1192,lut5=2,regs=2700)
      else         FPGAResources(lut3=1317,lut5=6,regs=2900)

    case FixPt_Lt(_,_)  => FPGAResources(lut4=nbits(s), regs=nbits(s))
    case FixPt_Leq(_,_) => FPGAResources(lut4=nbits(s), regs=nbits(s))
    case FixPt_Neq(_,_) => FPGAResources(lut4=nbits(s)/2, lut5=nbits(s)/8, regs=nbits(s))
    case FixPt_Eql(_,_) => FPGAResources(lut4=nbits(s)/2, lut5=nbits(s)/8, regs=nbits(s))
    case FixPt_And(_,_) => FPGAResources(lut3=nbits(s), regs=nbits(s))
    case FixPt_Or(_,_)  => FPGAResources(lut3=nbits(s), regs=nbits(s))

    //case FixPt_Lsh(_,_) => // ??? nbits(s)*nbits(s) ?
    //case FixPt_Rsh(_,_) => // ???

    // TODO: Floating point for things besides single precision
    case FltPt_Neg(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50)

    case FltPt_Add(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50) // ~372 ALMs, 1 DSP (around 564)

    case FltPt_Sub(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=397,lut4=29,lut5=125,lut6=34,lut7=5,regs=606,mem16=50)

    case FltPt_Mul(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=152,lut4=10,lut5=21,lut6=2,dsps=1,regs=335,mem16=43) // ~76 ALMs, 1 DSP (around 1967)

    case FltPt_Div(_,_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=2384,lut4=448,lut5=149,lut6=385,lut7=1,regs=3048,mem32=25,mem16=9)

    case FltPt_Lt(a,_)  =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Leq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Neq(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)

    case FltPt_Eql(a,_) =>
      if (nbits(a) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=42,lut6=26,regs=33)


    case Bit_Not(_)   => FPGAResources(lut3=1,regs=1)
    case Bit_And(_,_) => FPGAResources(lut3=1,regs=1)
    case Bit_Or(_,_)  => FPGAResources(lut3=1,regs=1)
    case Bit_Xor(_,_)  => FPGAResources(lut3=1,regs=1)
    case Bit_Xnor(_,_)  => FPGAResources(lut3=1,regs=1)

    case FixPt_Abs(_) => FPGAResources(lut3=nbits(s),regs=nbits(s))
    case FltPt_Abs(_) => FPGAResources(regs=nbits(s)-1)
    case FltPt_Log(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=472,lut4=47,lut5=74,lut6=26,lut7=3,mem16=42,regs=950,dsps=7,bram=3)

    case FltPt_Exp(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=368,lut4=102,lut5=137,lut6=38,mem16=24,regs=670,dsps=5,bram=2)

    case FltPt_Sqrt(_) =>
      if (nbits(s) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut3=476,lut4=6,lut5=6,mem32=11,regs=900)

    case Mux2(_,_,_) => FPGAResources(regs = nbits(s))

    case Convert_fixpt(_) => FPGAResources(regs=nbits(s))
    //case Convert_fltpt(_) => // ???
    case Fixpt_to_fltpt(x) =>
      if (nbits(s) != 32 && nbits(x) != 32) warn(s"Don't know area for $d - using default")
      FPGAResources(lut4=50,lut6=132,regs=238)

    case Fltpt_to_fixpt(_) =>
      FPGAResources(lut4=160,lut6=96,regs=223+nbits(s))


    // TODO: New templates - needs recharacterization
    // Tile Store
    case Offchip_store_vector(mem,ofs,vec) =>
      //val nonConstDims = (dimsOf(tt.mem) ++ tt.memOfs).filterNot{case Fixed(_) => true; case _ => false}.length
      //val dsp = if (nonConstDims > 1) 3 else 0
      //val p = parOf(cc).reduce{_*_}

      //val brm = if (p < 25) 46 + Math.floor(0.75*(p-1)).toInt else if (p < 49) 62 - Math.floor(0.75*(p-24)).toInt else 42

      //System.out.println(s"Tile store $tt: $brm")
      // Old template
      //FPGAResources(lut3=1900,lut4=167,lut5=207,lut6=516,lut7=11,regs=5636,dsps=dsp,bram=46,streams = 1)
      // New template
      FPGAResources(lut3=893,lut4=91,lut5=96,lut6=618,lut7=10, regs=4692, dsps=0, bram=0, streams=1)  // ~1206 ALMs

      //FPGAResources(lut3=378,lut4=38,lut5=58,lut6=569,lut7=4, regs=3878, dsps=dsp, bram=46, streams=1)

    // Tile Load
    case Offchip_load_vector(mem,ofs,len) =>
      //val p = parOf(cc).reduce{_*_}
      //val nonConstDims = (dimsOf(tt.mem) ++ tt.memOfs).filterNot{case Fixed(_) => true; case _ => false}.length
      //val dsp = if (nonConstDims > 1) 4 else 0
      // New template
      // FPGAResources(lut3=453, lut4=60, lut5=131,lut6=522,regs=1377,dsps=dsp,bram=46, streams=1)
      //val brams = 12 - p/8
      // New template
      FPGAResources(lut3=410, lut4=50, lut5=70, lut6=53, regs=920, dsps=0, bram=0, streams=1) // ~353 ALMs

    // TODO: New templates - needs recharacterization
    case Bram_store_vector(bram,ofs,vec,cchain) => NoArea
    case Bram_load_vector(bram,ofs,len,cchain) => NoArea


    case _:Pipe_parallel => FPGAResources(lut4=9*nStages(s)/2, regs = nStages(s) + 3)

    case e:Pipe_foreach if styleOf(s) == Coarse => areaOfMetapipe(nStages(s)) + areaOfCounterRegs(s, e.cchain)
    case e:Pipe_fold[_,_] if styleOf(s) == Coarse => areaOfMetapipe(nStages(s)) + areaOfCounterRegs(s, e.cchain)
    case e:Accum_fold[_,_] if styleOf(s) == Coarse => areaOfMetapipe(nStages(s)) + areaOfCounterRegs(s, e.ccOuter)

    case _:Pipe_foreach if styleOf(s) == Disabled => areaOfSequential(nStages(s))
    case _:Pipe_fold[_,_] if styleOf(s) == Disabled => areaOfSequential(nStages(s))
    case _:Accum_fold[_,_] if styleOf(s) == Disabled => areaOfSequential(nStages(s))
    case _:Unit_pipe if styleOf(s) == Disabled => areaOfSequential(nStages(s))

    // Nodes with known zero area cost
    case Reg_read(_)    => NoArea
    case Reg_write(_,_) => NoArea
    case Reg_reset(_)   => NoArea
    case Offchip_new(_) => NoArea
    case _:Pipe_foreach if styleOf(s) == Fine => NoArea
    case _:Pipe_fold[_,_] if styleOf(s) == Fine => NoArea
    case _:Unit_pipe if styleOf(s) == Fine => NoArea

    // Effects
    case Reflect(d,_,_) => areaOfNode(s,d)
    case Reify(_,_,_) => NoArea
    case _ =>
      warn(s"Don't know area for $d")
      NoArea
  }}
}

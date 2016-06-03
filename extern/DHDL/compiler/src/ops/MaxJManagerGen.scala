package dhdl.compiler.ops
import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import scala.collection.mutable.Set
import scala.reflect.SourceContext
import java.io.{PrintWriter}

trait MaxJManagerGen {
	val IR:DHDLExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

	var stream:PrintWriter = _
	def emit(str: String):Unit = {
		stream.println(str)
	}
  def quote(x: Exp[Any]):String = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName().replace("DHDL","") +
										(if (nameOf(s)!="") "_" else "") + nameOf(s) + n
    case _ => ""
  }

  /**
   * The following three methods are largely duplicated in CGenMemoryTemplateOps.
   * The only differences are that BlockRAM and DRAM aren't remapped in the Manager,
   * and 'float' and 'double' types are remapped to capitalized versions (non-caps
   * versions are reserved keywords)
   * must ideally share the remap methods from CCodegen
   */
  private def bitsToIntType(bits: Int) = {
    if (bits <= 8) "8"
    else if (bits <= 16) "16"
    else if (bits <= 32) "32"
    else "64"
  }

  private def bitsToFloatType(bits: Int) = {
    if (bits <= 32) "FLOAT"
    else "DOUBLE"
  }

  def remap[A](m: Manifest[A]): String = m.erasure.getSimpleName match {
    case "Register" => remap(m.typeArguments(0))
    case "DHDLBit" => "uint8_t"
    case "Signed" => ""
    case "Unsign" => "u"
    case "FixedPoint" => remap(m.typeArguments(0)) + "int" + bitsToIntType(remap(m.typeArguments(1)).toInt + remap(m.typeArguments(2)).toInt) + "_t"
    case "FloatPoint" => bitsToFloatType(remap(m.typeArguments(0)).toInt + remap(m.typeArguments(1)).toInt)
    case bx(n) => n
    case _ => throw new Exception(s"""No remap rule in MaxJManagerGen for ${m.erasure.getSimpleName}""")
  }



  val mImportPrefix = "com.maxeler.maxcompiler.v2"
  val streamClock = 150
  val memClock = 400
  val mImports = List(
    "build.EngineParameters",
    "kernelcompiler.Kernel",
    "managers.standard.Manager",
    "managers.standard.Manager.MemAccessPattern",
    "managers.standard.Manager.IOType",
    "managers.engine_interfaces.EngineInterface",
    "managers.engine_interfaces.EngineInterface.Direction",
    "managers.engine_interfaces.InterfaceParam",
    "managers.engine_interfaces.CPUTypes",
    "managers.custom.CustomManager",
    "managers.custom.DFELink",
    "managers.custom.blocks.KernelBlock",
    "managers.custom.stdlib.DebugLevel",
    "managers.BuildConfig",
    "managers.custom.stdlib.MemoryControllerConfig",
    "kernelcompiler.KernelConfiguration",
    "kernelcompiler.KernelConfiguration.OptimizationOptions",
    "kernelcompiler.KernelConfiguration.OptimizationOptions.OptimizationTechnique"
  )

  val oldMemImports = List(
    "managers.custom.stdlib.MemoryControlGroup"
    )

  val newMemImports = List(
    "managers.custom.stdlib.LMemCommandGroup",
    "managers.custom.stdlib.LMemInterface"
  )

  val mPreamble =
	s"""
	class TopManager extends CustomManager {
	  private static final CPUTypes int8_t =    CPUTypes.INT8;
	  private static final CPUTypes int16_t =    CPUTypes.INT16;
	  private static final CPUTypes int32_t =    CPUTypes.INT32;
	  private static final CPUTypes int64_t =    CPUTypes.INT64;
	  private static final CPUTypes uint8_t =    CPUTypes.UINT8;
	  private static final CPUTypes uint16_t =    CPUTypes.UINT16;
	  private static final CPUTypes uint32_t =    CPUTypes.UINT32;
	  private static final CPUTypes uint64_t =    CPUTypes.UINT64;
	  private static final CPUTypes FLOAT = CPUTypes.FLOAT;
	  private static final CPUTypes DOUBLE = CPUTypes.DOUBLE;
	"""

	  val mEpilogue =
	s"""
	  public static void main(String[] args) {
	    TopManager m = new TopManager(new EngineParameters(args));

	    BuildConfig c = new BuildConfig(BuildConfig.Level.FULL_BUILD);
	    c.setBuildEffort(BuildConfig.Effort.HIGH);
	    c.setEnableTimingAnalysis(true);
	    m.setBuildConfig(c);

	    m.createSLiCinterface(interfaceRead("readLMem"));
	    m.createSLiCinterface(interfaceWrite("writeLMem"));
	    m.createSLiCinterface(interfaceDefault());
	    m.build();
	  }
	}
	"""

	val mReadIntf =
	s"""
	  // CPU -> LMEM (read interface)
	  private static EngineInterface interfaceRead(String name) {
	    EngineInterface ei = new EngineInterface(name);
	    InterfaceParam size = ei.addParam("size", uint32_t);
	    InterfaceParam start = ei.addParam("start", uint32_t);
	    InterfaceParam sizeInBytes = size;

	    // Stop the kernel from running
	    ei.setScalar("TopKernel", "en", 0);

	    // Setup address map and access pattern
	    ei.setLMemLinear("fromlmem", start, sizeInBytes);
	    ei.setStream("tocpu", uint32_t, sizeInBytes);
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val mWriteIntf =
	s"""
	  // LMEM -> CPU (write interface)
	  private static EngineInterface interfaceWrite(String name) {
	    EngineInterface ei = new EngineInterface(name);
	    InterfaceParam size = ei.addParam("size", uint32_t);
	    InterfaceParam start = ei.addParam("start", uint32_t);
	    InterfaceParam sizeInBytes = size;

	    // Stop the kernel from running
	    ei.setScalar("TopKernel", "en", 0);

	    // Setup address map and access pattern
	    ei.setLMemLinear("tolmem", start, sizeInBytes);
	    ei.setStream("fromcpu", uint32_t, sizeInBytes);
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val mDefaultIntfPreamble =
	s"""
	  // Interface to run DFE (default interface)
	  private static EngineInterface interfaceDefault() {
	    EngineInterface ei = new EngineInterface();
	    ei.setTicks("TopKernel", Long.MAX_VALUE);
	    ei.setScalar("TopKernel", "en", 1);
	"""

	val mDefaultIntfEpilogue =
	s"""
	    ei.setLMemInterruptOn("intrStream");
	    ei.ignoreAll(Direction.IN_OUT);
	    return ei;
	  }
	"""

	val cpuIntfOld =
	s"""
	    // Setup CPU <-> FPGA stream
	    DFELink fromcpu = addStreamFromCPU("fromcpu");
	    DFELink tocpu = addStreamToCPU("tocpu");
	    DFELink fromlmem = addStreamFromOnCardMemory("fromlmem", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);
	    DFELink tolmem = addStreamToOnCardMemory("tolmem", MemoryControlGroup.MemoryAccessPattern.LINEAR_1D);
	    tolmem <== fromcpu;
	    tocpu <== fromlmem;
	"""
	val cpuIntfNew =
	s"""
	    // Setup CPU <-> FPGA stream
	    DFELink fromcpu = addStreamFromCPU("fromcpu");
	    DFELink tocpu = addStreamToCPU("tocpu");
	    DFELink fromlmem = addStreamFromLMem("fromlmem", LMemCommandGroup.MemoryAccessPattern.LINEAR_1D);
	    DFELink tolmem = addStreamToLMem("tolmem", LMemCommandGroup.MemoryAccessPattern.LINEAR_1D);
	    tolmem <== fromcpu;
	    tocpu <== fromlmem;
	"""

	val intrIntfOld =
	s"""
	    // Setup interrupt stream
	    DFELink intrStream = addStreamToOnCardMemory("intrStream", k.getOutput("intrCmd"));
	    intrStream <== k.getOutput("intrStream");
	"""
	val intrIntfNew =
	s"""
	    // Setup interrupt stream
	    DFELink intrStream = addStreamToLmem("intrStream", k.getOutput("intrCmd"));
	    intrStream <== k.getOutput("intrStream");
	"""
	//val intrIntf = if (Config.newMemAPI) intrIntfNew else intrIntfOld
	val intrIntf = intrIntfOld
	//val cpuIntf = if (Config.newMemAPI) cpuIntfNew else cpuIntfOld
	val cpuIntf = cpuIntfOld

	val mConstructorPreamble =
	s"""
	  TopManager(EngineParameters engineParameters) {
	    super(engineParameters);

	    // Disable stream status blocks
	    DebugLevel debugLevel = new DebugLevel();
	    debugLevel.setHasStreamStatus(false);
	    debug.setDebugLevel(debugLevel);

	    // Setup stream clock and memory clock
	    config.setDefaultStreamClockFrequency($streamClock);
	    config.setOnCardMemoryFrequency(LMemFrequency.MAX4MAIA_$memClock);
	    config.setEnableAddressGeneratorsInSlowClock(true);

	    // Setup memory controller clock and config
	//    MemoryControllerConfig mem_cfg = new MemoryControllerConfig();
	////    mem_cfg.setBurstSize(4); //MAX3: 4 = 4*384 bits, 8 = 8*384 bits
	//    mem_cfg.setDataReadFIFOExtraPipelineRegInFabric(true);
	//    mem_cfg.setDataFIFOExtraPipelineRegInFabric(true); //timing-23may
	//    //mem_cfg.setDataFIFOPrimitiveWidth(5*72);
	//    config.setMemoryControllerConfig(mem_cfg);

	    // Create a KernelConfiguration object that sets the OptimizationTechnique
	    // to optimize for area, which is the default in the 2014.1 compiler
	    // TODO: This causes build failures with MaxJ during source annotation. Investigate why
	    // KernelConfiguration kernelConfig = getCurrentKernelConfig();
	    // kernelConfig.optimization.setOptimizationTechnique(OptimizationTechnique.AREA);
	    // KernelBlock k = addKernel(new TopKernel(makeKernelParameters("TopKernel", kernelConfig)));

	    KernelBlock k = addKernel(new TopKernel(makeKernelParameters("TopKernel")));

	    $cpuIntf

	    $intrIntf
	"""

val mConstructorEpilogue =
s"""
  }
"""

  def emitImports() = {
    mImports.foreach(i => emit(s"import $mImportPrefix.$i;"))
    //if (Config.newMemAPI) {
      oldMemImports.foreach { i => emit(s"import $mImportPrefix.$i;") }
    //} else {
      //oldMemImports.map { i => emit(s"import $mImportPrefix.$i;") }
    //}
  }

  def emitConstructor(memStreams: Set[Sym[Any]]) = {
    emit(mConstructorPreamble)
    emit("    // Setup LMEM -> DFE streams (input streams to DFE)")
    emit("    // Setup DFE -> LMEM (output streams from DFE)")
    memStreams.foreach{
      case tt@Def(EatReflect(Offchip_store_vector(mem,ofs,vec))) =>
        val streamName = s"${quote(mem)}_${quote(tt)}"
        emit(s"""    DFELink ${streamName}_out = addStreamToOnCardMemory("${streamName}_out", k.getOutput("${streamName}_out_cmd"));""")
        emit(s"""    ${streamName}_out <== k.getOutput("${streamName}_out");""")

      case tt@Def(EatReflect(Offchip_load_vector(mem,ofs,len))) =>
     	  val streamName = s"${quote(mem)}_${quote(tt)}"
        emit(s"""    DFELink ${streamName}_in = addStreamFromOnCardMemory("${streamName}_in", k.getOutput("${streamName}_in_cmd"));""")
        emit(s"""    k.getInput("${streamName}_in") <== ${streamName}_in;""")
    }
    emit(mConstructorEpilogue)
  }

  def emitRWInterface() = {
    emit(mReadIntf)
    emit(mWriteIntf)
  }

  def emitDefaultInterface(argInOuts: Set[Sym[Register[_]]]) = {
    emit(mDefaultIntfPreamble)
    argInOuts.foreach { a =>
			regType(a) match {
				case Regular =>
				case ArgumentIn =>
					val ts =  remap(a.tp)
      		emit(s"""    InterfaceParam ${quote(a)} = ei.addParam("${quote(a)}", ${ts});""")
      		emit(s"""    ei.setScalar("TopKernel", "${quote(a)}", ${quote(a)});""")
        case ArgumentOut =>
      		emit(s"""    ei.unignoreScalar("TopKernel", "${quote(a)}");""")
			}
    }
    emit(s"""    ei.unignoreScalar("TopKernel", "cycles");""")
    emit(mDefaultIntfEpilogue)
  }

  def emitManager(stream:PrintWriter, argInOuts:Set[Sym[Register[_]]], memStreams:Set[Sym[Any]]) = {
		this.stream = stream
    initPass()
    //println(s"""tileTransfers: """)
		//tileTsfs.foreach { tt => println(quote(tt)) }
    //println(s"""argIns and argOuts: """)
		//argInOuts.foreach { a => println(quote(a)) }
    emitConstructor(memStreams)
    emitRWInterface()
    emitDefaultInterface(argInOuts)

    finPass()
  }

  def initPass() = {
    emit("package engine;")
    emitImports()
    emit(mPreamble)
  }

  def finPass() = {
    emit(mEpilogue)
  }


}

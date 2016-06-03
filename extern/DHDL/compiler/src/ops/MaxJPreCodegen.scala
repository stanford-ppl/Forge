package dhdl.compiler.ops

import scala.reflect.{Manifest,SourceContext}
import scala.virtualization.lms.internal.{MaxJCodegen}
import scala.virtualization.lms.internal.{Traversal}
import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

import dhdl.shared._
import dhdl.shared.ops._
import dhdl.compiler._
import dhdl.compiler.ops._
import scala.collection.mutable.Set

import ppl.delite.framework.DeliteApplication

trait MaxJPreCodegen extends Traversal  {
	val IR:DHDLExp
	import IR.{infix_until => _, looprange_until => _, println => _, _}

	var buildDir:String = _

  //debugMode = false
  override val name = "MaxJPreCodegen"

	lazy val maxJManagerGen = new MaxJManagerGen {
		val IR: MaxJPreCodegen.this.IR.type = MaxJPreCodegen.this.IR
	}

  def quote(x: Exp[Any]):String = x match {
		case s@Sym(n) => s.tp.erasure.getSimpleName().replace("DHDL","") + nameOf(s).map(nm=>"_"+nm).getOrElse("") + "_x" + n
    case _ => ""
  }

	val argInOuts  = Set.empty[Sym[Register[_]]]
	val memStreams = Set.empty[Sym[Any]]

  override def preprocess[A:Manifest](b: Block[A]): Block[A] = {
		argInOuts.clear
		memStreams.clear
		b
	}
  override def postprocess[A:Manifest](b: Block[A]): Block[A] = {
		withStream(newStream("MaxJManager")) {
			maxJManagerGen.emitManager(stream, argInOuts, memStreams)
		}
		b
	}

	def newStream(fileName:String):PrintWriter = {
		val path = buildDir + java.io.File.separator + fileName + ".maxj"
		val pw = new PrintWriter(path)
		pw
	}

	var stream:PrintWriter = _

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush; stream.close; stream = save }
  }

	def emit(str: String):Unit = {
		stream.println(str)
	}

  override def traverseStm(stm: Stm): Unit = stm match { // override this to implement custom traversal
    case TP(sym, rhs) => {
			preGenNodes(sym,rhs)
			super.traverseStm(stm)
		}
    case _ => super.traverseStm(stm)
	}

  def preGenNodes(sym: Sym[Any], rhs: Def[Any]):Unit = rhs match {
		case e@Pipe_parallel(func: Block[Unit]) =>
			withStream(newStream("parallel_" + quote(sym))) {
				emitParallelSM(quote(sym), childrenOf(sym).length)
			}
    case e@Pipe_foreach(cchain, func, inds) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse =>
					withStream(newStream("metapipe_" + quote(sym))) {
    				emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case Fine =>
				case Disabled =>
					withStream(newStream("sequential_" + quote(sym))) {
    				emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}
    case e@Pipe_fold(cchain, accum, foldAccum, iFunc, ldFunc, stFunc, func, rFunc, inds, idx, acc, res, rV) =>
			styleOf(sym.asInstanceOf[Rep[Pipeline]]) match {
				case Coarse =>
					withStream(newStream("metapipe_" + quote(sym))) {
    				emitMPSM(s"${quote(sym)}", childrenOf(sym).size)
					}
				case Fine =>
				case Disabled =>
					withStream(newStream("sequential_" + quote(sym))) {
    				emitSeqSM(s"${quote(sym)}", childrenOf(sym).size)
					}
			}
		case e:Reg_new[_] if regType(sym) != Regular => argInOuts += sym.asInstanceOf[Sym[Register[_]]]

    case _:Offchip_store_vector[_] => memStreams += sym
    case _:Offchip_load_vector[_] => memStreams += sym

    case e@EatReflect(Bram_new(size, zero)) =>
			withStream(newStream("bram_" + quote(sym))) {
				emitDblBufSM(quote(sym), readersOf(sym).length)
			}
    case Reflect(s, u, effects) =>
      preGenNodes(sym, s)
    case Reify(s, u, effects) =>
		case _ => {
			//println("tp:" + sym.tp.erasure.getSimpleName() + "rhs:" + rhs)
		}
	}

  def emitDblBufSM(name: String, numReaders: Int) = {
  emit(s"""
  package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;

  class ${name}_DblBufSM extends KernelStateMachine {

    // States
    enum States {
      W, R, RW, SWAP
    }

    // State IO
    private final DFEsmInput w_done;
    private final DFEsmOutput curBuf;""");

    for(i <- 0 until numReaders) {
  emit(s"""
  private final DFEsmInput r_done_$i;
  """)
    }

  emit(s"""
    // State storage
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue curBufFF;
    private final DFEsmStateValue numRdoneFF;

    private final DFEsmValue allRdone;
    private final DFEsmValue anyRdone;

    // Initialize state machine in constructor
    public ${name}_DblBufSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      w_done = io.input("w_done", wireType);
  """)

  for(i <- 0 until numReaders) {
      emit(s"""
        r_done_${i} = io.input("r_done_${i}", wireType);
      """)
    }

  emit(s"""
      curBuf = io.output("curBuf", wireType);

      // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.W);
      curBufFF = state.value(wireType, 0);
      numRdoneFF = state.value(wireType, 0);""")

  val anydoneStr = (0 until numReaders).map { "r_done_"+_ }.mkString(" | ")
  emit(s"""
      anyRdone = ($anydoneStr);
      allRdone = numRdoneFF & anyRdone;
    }

  @Override
  protected void nextState() {
    numRdoneFF.next <== anyRdone | numRdoneFF;
    SWITCH(stateFF) {
      CASE(States.W) {
        IF (w_done) {
          stateFF.next <== States.SWAP;
        }
      }
      CASE(States.RW) {
        IF (allRdone & w_done) {
          stateFF.next <== States.SWAP;
        } ELSE {
          IF (allRdone) {
          stateFF.next <== States.W;
          } ELSE {
            IF (w_done) {
              stateFF.next <== States.R;
            }
          }
        }
      }
      CASE(States.R) {
        IF (allRdone) {
          stateFF.next <== States.SWAP;
        }
      }
      CASE(States.SWAP) {
        curBufFF.next <== ~curBufFF;
        stateFF.next <== States.RW;
        numRdoneFF.next <== 0;
      }
      OTHERWISE {
        stateFF.next <== stateFF;
      }
    }
  }

  @Override
  protected void outputFunction() {
    curBuf <== curBufFF;
  }
  }
  """)
  }

  private def stateTextSeq(state: Int, N: Int) = {
    val condStr = s"bitVector[ $state ]"
    val max = N-1

    emit(s"""IF($condStr) {
      resetBitVector();""")
    if (state == max) {
      emit(s"""
      counterFF.next <== counterFF + 1;
      IF (counterFF === sizeFF-1) {
        stateFF.next <== States.DONE;
      } ELSE {
        stateFF.next <== States.S0;
      }""")
      emit("}")
    } else {
      emit(s"stateFF.next <== States.S${state+1};")
      emit("}")
    }
  }

  def emitSeqSM(name: String, numStates: Int):Unit = {
		if (numStates==0) {
			emit(s"""//Number of stages = 0 for ${name}. Nothing is emitted""")
			return
		}
    emit("""
package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;
""")

  val smName = name
  val states = (0 until numStates).map(List(_)).toList
  emit(s"""class ${smName}_SeqSM extends KernelStateMachine {""")

  val stateNames = states.map(stateStr(_))
  emit(s"""
    // States
    enum States {
      INIT,
      RSET,
      ${stateNames.reduce(_ + ",\n" + _) + ",\nDONE"}
    }
  """)

  emit("""
    // State IO
    private final DFEsmOutput sm_done;
//    private final DFEsmOutput sm_last;
    private final DFEsmInput sm_en;
    private final DFEsmInput sm_numIter;
    private final DFEsmOutput rst_en;
  """)

  for(i <- 0 until numStates) {
    emit(s"""
    private final DFEsmInput s${i}_done;
    private final DFEsmOutput s${i}_en;
    """)
  }

  emit(s"""
    // State storage
    private final DFEsmStateValue sizeFF;
//    private final DFEsmStateValue lastFF;
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue counterFF;
    private final DFEsmStateValue rstCounterFF;
    private final DFEsmStateValue[] bitVector;

    private final int numStates = ${numStates};
    private final int rstCycles = 10; // <-- hardcoded
    // Initialize state machine in constructor
    public ${smName}_SeqSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      sm_done = io.output("sm_done", wireType);
//      sm_last = io.output("sm_last", wireType);
      sm_en = io.input("sm_en", wireType);
      sm_numIter = io.input("sm_numIter", counterType);
      rst_en = io.output("rst_en", wireType);
  """)

  for(i <- 0 until numStates) {
    emit(s"""
      s${i}_done = io.input("s${i}_done", wireType);
      s${i}_en = io.output("s${i}_en", wireType);
    """)
  }

  emit("""
    // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.INIT);
      counterFF = state.value(counterType, 0);
      rstCounterFF = state.value(counterType, 0);
      sizeFF = state.value(counterType, 0);
//      lastFF = state.value(wireType, 0);

      // Bitvector keeps track of which kernels have finished execution
      // This is a useful hardware synchronization structure to keep
      // track of which kernels have executed/finished execution
      bitVector = new DFEsmStateValue[numStates];
      for (int i=0; i<numStates; i++) {
        bitVector[i] = state.value(wireType, 0);
      }
    }

    private void resetBitVector() {
      for (int i=0; i<numStates; i++) {
        bitVector[i].next <== 0;
      }
    }
      """)

  emit(s"""
    @Override
    protected void nextState() {
      IF(sm_en) {
        // State-agnostic update logic for bitVector
    """)
  for(i <- 0 until numStates) {
    emit(s"""
        IF (s${i}_done) {
          bitVector[$i].next <== 1;
        }""")
  }

  emit(s"""
        SWITCH(stateFF) {
          CASE (States.INIT) {
            sizeFF.next <== sm_numIter;
            stateFF.next <== States.RSET;
            counterFF.next <== 0;
            rstCounterFF.next <== 0;
//            lastFF.next <== 0;
          }

          CASE (States.RSET) {
            rstCounterFF.next <== rstCounterFF + 1;
            IF (rstCounterFF === rstCycles) {
              stateFF.next <== States.S0;
            } ELSE {
              stateFF.next <== States.RSET;
            }
          }
          """)

  for(i <- 0 until states.size) {
    val state = states(i)
    val name = stateNames(i)
    emit(s"""
          CASE (States.${name}) {""")
      stateTextSeq(state(0), numStates)
    emit(s"""
          }""")
  }

  emit(s"""
         CASE (States.DONE) {
           resetBitVector();
           stateFF.next <== States.INIT;
         }

         OTHERWISE {
           stateFF.next <== stateFF;
         }
        }
      }
    }""")

  emit(s"""
  @Override
    protected void outputFunction() {
      sm_done <== 0;
      rst_en <== 0;
//      sm_last <== 0;
      """)

  for (i <- 0 until numStates) {
    emit(s"""
      s${i}_en <== 0;""")
  }

  emit(s"""
     IF (sm_en) {
//        IF (counterFF === sizeFF-1) {
//          sm_last <== 1;
//        } ELSE {
//          sm_last <== 0;
//        }
       SWITCH(stateFF) {
            CASE (States.RSET) {
              rst_en <== 1;
            }""")
        for(i <- 0 until states.size) {
          val state = states(i)
          val name = stateNames(i)
          emit(s"""
            CASE (States.$name) {""")
             for (s <- state) {
               emit(s"""s${s}_en <== ~(bitVector[$s] | s${s}_done);""")
             }
          emit(s"""
                }""")
        }

        emit(s"""
          CASE (States.DONE) {
            sm_done <== 1;
          }""")

  emit("""
      }
    }
  }
}
  """)

  }

  private def stateStr(state:List[Int]) = {
    "S" + state.map( _.toString).reduce(_+_)
  }

	def emitParallelSM(name: String, numParallel: Int):Unit = {
		if (numParallel==0) {
			emit(s"""//Number of parallel stages = 0 for ${name}. Nothing is emitted""")
			return
		}
		emit(s"""
			package engine;
			import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
			import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
			import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
			import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;

			class ${name}_ParSM extends KernelStateMachine {

				// States
				enum States {
					INIT,
					RUN,
					DONE
				}

				// State IO
				private final DFEsmOutput sm_done;
				private final DFEsmInput sm_en;""");

		for(i <- 0 until numParallel) {
			emit(s"""
				private final DFEsmInput s${i}_done;
				private final DFEsmOutput s${i}_en;
				""")
		}

		emit(s"""
			// State storage
			private final DFEsmStateEnum<States> stateFF;
			private final DFEsmStateValue[] bitVector;

			private final int numParallel = $numParallel;
			// Initialize state machine in constructor
			public ${name}_ParSM(KernelLib owner) {
				super(owner);

				// Declare all types required to wire the state machine together
				DFEsmValueType counterType = dfeUInt(32);
				DFEsmValueType wireType = dfeBool();
				// Define state machine IO
				sm_done = io.output("sm_done", wireType);
				sm_en = io.input("sm_en", wireType);
				""")
		for(i <- 0 until numParallel) {
			emit(s"""
				s${i}_done = io.input("s${i}_done", wireType);
				s${i}_en = io.output("s${i}_en", wireType);
				""")
		}

		emit(s"""
			// Define state storage elements and initial state
			stateFF = state.enumerated(States.class, States.INIT);

			bitVector = new DFEsmStateValue[numParallel];
			for (int i=0; i<numParallel; i++) {
				bitVector[i] = state.value(wireType, 0);
			}
			}

			private void resetBitVector() {
				for (int i=0; i<numParallel; i++) {
					bitVector[i].next <== 0;
				}
			}

			@Override
			protected void nextState() {
				IF(sm_en) {
					""")

		for(i <- 0 until numParallel) {
			emit(s"""
				IF (s${i}_done) {
					bitVector[$i].next <== 1;
				}""")
		}

		emit(s"""
			SWITCH(stateFF) {
				CASE (States.INIT) {
					stateFF.next <== States.RUN;
				}
				""")

		emit(s"""
			CASE (States.RUN) {""")
				val condStr = (0 until numParallel).map("bitVector[" + _ + "]").reduce(_ + " & " + _)
				emit(s"""
					IF($condStr) {
						resetBitVector();
						stateFF.next <== States.DONE;
					}
			}

			CASE (States.DONE) {
				resetBitVector();
				stateFF.next <== States.INIT;
			}
			OTHERWISE {
				stateFF.next <== stateFF;
			}
			}
				}
			}
			""")

				emit("""
					@Override
					protected void outputFunction() {
						sm_done <== 0;""")
				for (i <- 0 until numParallel) {
					emit(s"""
						s${i}_en <== 0;""")
				}

				emit("""
					IF (sm_en) {
						SWITCH(stateFF) {
							CASE(States.RUN) {""")
								for (i <- 0 until numParallel) {
									emit(s"""s${i}_en <== ~(bitVector[${i}] | s${i}_done);""")
								}
								emit(s"""
							}
							CASE(States.DONE) {
								sm_done <== 1;
							}
						}
					}
					}
			}""")
	}

  private def getStates(N: Int) = {
    val l = 0.until(N).toList
    val lb_total = ListBuffer[List[Int]]()
    for (i <- 0 until l.size) {
         val lb = ListBuffer[List[Int]]()
         lb.append(List(i))
         for (j <- i+1 until l.size) {
           lb.append((lb.last ++ List(j)))
         }
         lb_total.appendAll(lb)
    }
    lb_total.toList
  }

  private def stateText(state: List[Int], N: Int) = {
    val condStr = state.map("bitVector[" + _ + "]").reduce(_ + " & " + _)
    val max = N-1

    emit(s"""IF($condStr) {
      resetBitVector();""")
    if (state.size == 1 && state.max == max && !state.contains(0)) {
      emit("stateFF.next <== States.DONE;")
    } else {
      if (state.contains(0)) {
        emit("counterFF.next <== counterFF + 1;")
        emit("IF (counterFF === sizeFF-1) {")
        stream.print("stateFF.next <== States.")
        if (state.max == max) {
          if (state.size == 1) {  // Only state 0
            stream.print("DONE")
          } else {
            stream.print(stateStr(state.drop(1)))
          }
        } else {
          stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
        }
          emit(";")
          emit("} ELSE {")
        stream.print("stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state)) else stream.print(stateStr(state ++ List(state.max+1)))
          emit(";")
          emit("}")
      } else {
        stream.print("stateFF.next <== States.")
        if (state.max == max) stream.print(stateStr(state.drop(1))) else stream.print(stateStr(state.drop(1) ++ List(state.max+1)))
        emit(";")
      }
    }
    emit("}")
  }

  def emitMPSM(name: String, numStates: Int):Unit = {
		if (numStates==0) {
			emit(s"""//Number of stages = 0 for ${name}. Nothing is emitted""")
			return
		}
    emit("""
package engine;
  import com.maxeler.maxcompiler.v2.kernelcompiler.KernelLib;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmInput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmOutput;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateEnum;
  import com.maxeler.maxcompiler.v2.statemachine.DFEsmStateValue;
  import com.maxeler.maxcompiler.v2.statemachine.kernel.KernelStateMachine;
  import com.maxeler.maxcompiler.v2.statemachine.types.DFEsmValueType;
""")

  val smName = name
  val states = getStates(numStates)
  emit(s"""class ${smName}_MPSM extends KernelStateMachine {""")


  // val stateNames = states.map("S" + _.map( _.toString).reduce(_+_))
  val stateNames = states.map(stateStr(_))
  emit(s"""
    // States
    enum States {
      INIT,
      RSET,
      ${stateNames.reduce(_ + ",\n" + _) + ",\nDONE"}
    }
  """)

  emit("""

    // State IO
    private final DFEsmOutput sm_done;
    private final DFEsmOutput sm_last;
    private final DFEsmInput sm_en;
    private final DFEsmInput sm_numIter;
    private final DFEsmOutput rst_en;
  """)

  for(i <- 0 until numStates) {
    emit(s"""
    private final DFEsmInput s${i}_done;
    private final DFEsmOutput s${i}_en;
    """)
  }

  emit(s"""
    // State storage
    private final DFEsmStateValue sizeFF;
    private final DFEsmStateValue lastFF;
    private final DFEsmStateEnum<States> stateFF;
    private final DFEsmStateValue counterFF;
    private final DFEsmStateValue rstCounterFF;
    private final DFEsmStateValue[] bitVector;

    private final int numStates = ${numStates};
    private final int rstCycles = 10; // <-- hardcoded
    // Initialize state machine in constructor
    public ${smName}_MPSM(KernelLib owner) {
      super(owner);

      // Declare all types required to wire the state machine together
      DFEsmValueType counterType = dfeUInt(32);
      DFEsmValueType wireType = dfeBool();

      // Define state machine IO
      sm_done = io.output("sm_done", wireType);
      sm_last = io.output("sm_last", wireType);
      sm_en = io.input("sm_en", wireType);
      sm_numIter = io.input("sm_numIter", counterType);
      rst_en = io.output("rst_en", wireType);
  """)

  for(i <- 0 until numStates) {
    emit(s"""
      s${i}_done = io.input("s${i}_done", wireType);
      s${i}_en = io.output("s${i}_en", wireType);
    """)
  }

  emit("""
    // Define state storage elements and initial state
      stateFF = state.enumerated(States.class, States.INIT);
      counterFF = state.value(counterType, 0);
      rstCounterFF = state.value(counterType, 0);
      sizeFF = state.value(counterType, 0);
      lastFF = state.value(wireType, 0);

      // Bitvector keeps track of which kernels have finished execution
      // This is a useful hardware synchronization structure to keep
      // track of which kernels have executed/finished execution
      bitVector = new DFEsmStateValue[numStates];
      for (int i=0; i<numStates; i++) {
        bitVector[i] = state.value(wireType, 0);
      }
    }

    private void resetBitVector() {
      for (int i=0; i<numStates; i++) {
        bitVector[i].next <== 0;
      }
    }
      """)

  emit(s"""
    @Override
    protected void nextState() {
      IF(sm_en) {
        // State-agnostic update logic for bitVector
    """)
  for(i <- 0 until numStates) {
    emit(s"""
        IF (s${i}_done) {
          bitVector[$i].next <== 1;
        }""")
  }

emit("""
        IF (counterFF === sizeFF-2) {
          lastFF.next <== 1;
        }""")

  emit(s"""
        SWITCH(stateFF) {
          CASE (States.INIT) {
            sizeFF.next <== sm_numIter;
            stateFF.next <== States.RSET;
            counterFF.next <== 0;
            rstCounterFF.next <== 0;
            lastFF.next <== 0;
          }

          CASE (States.RSET) {
            rstCounterFF.next <== rstCounterFF + 1;
            IF (rstCounterFF === rstCycles) {
              stateFF.next <== States.S0;
            } ELSE {
              stateFF.next <== States.RSET;
            }
          }
          """)

  for(i <- 0 until states.size) {
    val state = states(i)
    val name = stateNames(i)
    emit(s"""
          CASE (States.${name}) {""")
      stateText(state, numStates)
    emit(s"""
          }""")
  }

  emit(s"""
         CASE (States.DONE) {
           resetBitVector();
           stateFF.next <== States.INIT;
         }

         OTHERWISE {
           stateFF.next <== stateFF;
         }
        }
      }
    }""")

  emit(s"""
  @Override
    protected void outputFunction() {
      sm_done <== 0;
      sm_last <== 0;
      rst_en <== 0;
      """)

  for (i <- 0 until numStates) {
    emit(s"""
      s${i}_en <== 0;""")
  }

  emit(s"""
     IF (sm_en) {
        IF (counterFF === sizeFF-1) {
          sm_last <== 1;
        } ELSE {
          sm_last <== 0;
        }
       SWITCH(stateFF) {
            CASE (States.RSET) {
              rst_en <== 1;
            }""")

        for(i <- 0 until states.size) {
          val state = states(i)
          val name = stateNames(i)
          emit(s"""
            CASE (States.$name) {""")
             for (s <- state) {
               emit(s"""s${s}_en <== ~(bitVector[$s] | s${s}_done);""")
             }
          emit(s"""
                }""")
        }

        emit(s"""
          CASE (States.DONE) {
            sm_done <== 1;
          }""")

  emit("""
      }
    }
  }
  }
  """)
  }

}

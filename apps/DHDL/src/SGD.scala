// import dhdl.compiler._
// import dhdl.library._
// import dhdl.shared._
// import scala.util.Random
// import java.io._
// import scala.io.Source

// object SGDCompiler extends DHDLApplicationCompiler with SGD
// object SGDInterpreter extends DHDLApplicationInterpreter with SGD
// trait SGD extends DHDLApplication {

//   type Elem = Flt

//   override def stageArgNames = List("weightsDim", "batchDim", "numBatches")
//   lazy val weightsDim = param("weightsDim", 5)
//   lazy val batchDim = param("batchDim", 2)
//   lazy val numBatches = param("numBatches", 2)
//   lazy val epochs = ArgIn[SInt]("epochs")
//   lazy val alpha = ArgIn[Elem]("alpha")

//   def main() {
//     val WD = args(unit(0)).to[SInt]
//     val BD = args(unit(1)).to[SInt]
//     val NB = args(unit(2)).to[SInt]
//     val EP = args(unit(3)).to[SInt]
//     val AL = args(unit(4)).to[Elem]

//     // bound(WD) = 1536
//     // bound(BD) = 1536
//     // bound(NB) = 1563
//     // domainOf(weightsDim) = (1,1536,1)
//     // domainOf(batchDim) = (96,1536,96)
//     // domainOf(numBatches) = (96,1536,96)

//     val offchipWeights = OffChipMem[Elem]("OffchipWeights", WD)
//     val offchipX = OffChipMem[Elem]("OffchipX", BD*NB, WD)
//     val offchipY = OffChipMem[Elem]("OffchipY", BD, NB)
//     val offchipOut = OffChipMem[Elem]("OffchipOut", WD)

//     val weights = Array.fill(WD){random[Elem](1)}
//     val X = Array.fill(BD*NB){ Array.fill(WD){random[Elem](1)}}
//     val Y = Array.fill(BD){ Array.fill(NB){random[Elem](20)}}

//     setArg(epochs, EP)
//     setArg(alpha, AL)
//     setMem(offchipWeights, weights)
//     setMem(offchipX, X.flatten)
//     setMem(offchipY, Y.flatten)

//     Accel {

//       Sequential(1 by 1) {kk => 
//         val tileWeights = BRAM[Elem]("tileWeights", weightsDim)
//         val tileY = BRAM[Elem]("tileY", batchDim, numBatches) // Because can't multiply... T_T
//         val tileYErr = BRAM[Elem]("tileYErr", batchDim)
//         val tileUpdated = BRAM[Elem]("tileUpdated", weightsDim)
//         Parallel {
//           tileWeights := offchipWeights(0::weightsDim, param(1))
//           tileY := offchipY(0::batchDim, 0::numBatches, param(1))
//         }
//         Sequential(EP by 1){epoch => 
//           Sequential((BD*NB) by BD){ i =>
//             // Load data
//             val tileX = BRAM[Elem]("tileX", batchDim, weightsDim)
//             Parallel {
//               tileX := offchipX(i::i+batchDim, 0::weightsDim)
//             }
//             // Get error
//             Sequential(BD by 1){ row => 
//               val accum = Reg[Elem]
//               val logic = (epoch == 0) && (i == 0)
//               Pipe.reduce((WD by 1))(accum){ col =>
//                 val use = mux(logic, tileWeights(col), tileUpdated(col))
//                 tileX(row, col) * use }
//                 {_+_}
//               Pipe {
//                 tileYErr(row) = tileY(row, 0) - accum.value
//               }
//             }
//             // // Update
//             // Sequential(WD by 1){ col =>
//             //   val accum = Reg[Elem]
//             //   Pipe.reduce((BD by 1))(accum){ row =>
//             //     tileYErr(row) * tileX(row, col)}
//             //     {_+_}
//             //   Pipe{
//             //     val logic = (epoch == 0) && (i == 0)
//             //     val use = mux(logic, tileWeights(col), tileUpdated(col))
//             //     tileUpdated(col) = use + accum.value //* AL.value / BD.to[Elem]
//             //     ()
//             //   }
//             // }
//           }
//         }
//         // offchipOut(0::WD) := tileUpdated
//       }
//     }

//     val result = getMem(offchipOut)
//     println("expected: ")
//     println("result: " + result.mkString(", "))




//   }
// }






























// //   // type Elem = FixPt[Signed,B16,B16]
// //   type Elem = FltPt[B24, B8]

// //   val weightsDim = 2
// //   val batchDim = 2
// //   val numBatches = 1

// //   override def stageArgNames = List("weightsDim", "batchDim", "numBatches")
// //   lazy val W = param("weightsDim", weightsDim) //modelDim
// //   lazy val B = param("batchDim", batchDim) //batchDim
// //   lazy val NB = param("numBatches", numBatches) //numBatches
// //   lazy val A = ArgIn[Elem]("A") //alpha
// //   lazy val E = ArgIn[SInt]("E") //epochs

// //   def SGD(weights: Rep[OffChipMem[Elem]], X: Rep[OffChipMem[Elem]], 
// //           Y: Rep[OffChipMem[Elem]], out: Rep[OffChipMem[Elem]]) {
// //     val tileWeights = BRAM[Elem](W)
// //     val tileX = BRAM[Elem](B, W)
// //     val tileY = BRAM[Elem](B)
// //     val tileUpdated = BRAM[Elem](W)
// //     val tileYErr = BRAM[Elem](B)

// //     Sequential(1 by 1) {kk => 
// //       Parallel {
// //         tileWeights := weights(0::W)
// //         tileY := Y(0::B)
// //       }
// //       Sequential(E by 1){epoch => 
// //         Sequential((B.value*NB.value) by B.value){ i =>
// //           // Load data
// //           Parallel {
// //             tileX := X(i::i+B, 0::W)
// //           }
// //           // Get error
// //           Sequential(B by 1){ row => 
// //             val accum = Reg[Elem]
// //             val logic = (epoch == 0) && (i == 0)
// //             Pipe.reduce((W by 1))(accum){ col =>
// //               val use = mux(logic, tileWeights(col), tileUpdated(col))
// //               tileX(row, col) * use }
// //               {_+_}
// //             Pipe {
// //               tileYErr(row) = tileY(row) - accum.value
// //               () 
// //             }
// //           }
// //           // Update
// //           Sequential(W by 1){ col =>
// //             val accum = Reg[Elem]
// //             Pipe.reduce((B by 1))(accum){ row =>
// //               tileYErr(row) * tileX(row, col)}
// //               {_+_}
// //             Pipe{
// //               val logic = (epoch == 0) && (i == 0)
// //               val use = mux(logic, tileWeights(col), tileUpdated(col))
// //               tileUpdated(col) = use + accum.value * A.value.to[Elem] / B.value.to[Elem]
// //               ()
// //             }
// //           }
// //         }
// //       }
// //       out(0::W) := tileUpdated
// //     }
// //   }

// //   // TODO: Why does cost creep back up after it converges?
// //   //       Make global variables to set args in main
// //   //       Fix DHDL implementation

// //   def main() = {
// //     val WD = weightsDim
// //     val BD = batchDim
// //     val nNB = numBatches
// //     val AL = args(unit(0)).to[Elem]
// //     val EP = args(unit(1)).to[SInt]

// //     // Set test data bounds
// //     val maxWnoise = 0.25
// //     val maxW = 3
// //     val maxX = 1
// //     val maxYnoise = 0.1

// //     // Set offchip mems
// // 		val offchipWeights = OffChipMem[Elem]("ocWeights", WD)
// // 		val offchipX = OffChipMem[Elem]("ocX", BD*nNB, WD)
// // 		val offchipY = OffChipMem[Elem]("ocY", BD*nNB)
// //     val offchipResult = OffChipMem[Elem]("ocResult", WD)

// //     // Create test data
// //     val realW = Array.fill(WD){random[Elem](maxW)}
// //     val noiseW = Array.fill(WD){random[Elem](maxWnoise*2)}
// //     val sW = realW.zip(noiseW){_+_-maxWnoise}
// //     val sX = Array.fill(BD*nNB){ Array.fill(WD){random[Elem](maxX)} }
// //     val realY = sX.map{row => row.zip(realW){_*_}.reduce(_+_)}
// //     val noiseY = Array.fill(BD*nNB){random[Elem](maxYnoise*2)}
// //     val sY = realY.zip(noiseY){_+_-maxYnoise}

// //     setArg(A, AL)
// //     setArg(E, EP)

// //     setMem(offchipWeights, sW)
// //     setMem(offchipX, sX.flatten)
// //     setMem(offchipY, sY)
// //     Accel{ SGD(offchipWeights, offchipX, offchipY, offchipResult) }

// //     // SGD in Scala
// //     var updated = Array.fill(WD)(1.as[Elem])
// //     var y_err = Array.fill(BD)(1.as[Elem])
// //     var y_hat = Array.fill(BD)(1.as[Elem])
// //     for (k <- 0 until EP) {
// //       for (b <- 0 until nNB) {
// //         // 1. Compute y_hat
// //         y_hat = Array.tabulate(BD){i =>
// //           val xRow = sX(i + b*BD)
// //           xRow.zip(updated){_*_}.reduce(_+_)
// //         }
// //         // 2. Compute y_err
// //         val thissY = Array.tabulate(BD){i =>
// //           sY(i + b*BD)
// //         }
// //         var y_err = thissY.zip(y_hat){_-_}
// //         // val a = y_err.zip(y_err){_*_}
// //         // val cost = a.reduce{_+_}
// //         // println("cost = " + cost + " @ " + k)
// //         // 3. Compute gradient
// //         val gradient = Array.tabulate(WD){i =>
// //           var xCol = Array.fill(BD)(1.as[Elem])
// //           var flatX = sX.flatten
// //           for (j <- 0 until BD) {
// //             xCol(j)=flatX(i + j*WD + b*BD*WD)
// //           }
// //           xCol.zip(y_err){_*_}.reduce{_+_}
// //         }
// //         updated = Array.tabulate(WD){i => 
// //           val movement = gradient(i)*AL/BD
// //           updated(i) + movement
// //         }
// //       }
// //       // // Cost tracker
// //       // val a = updated.zip(realW){_-_}
// //       // val b = a.zip(a){_*_}
// //       // val cost = b.reduce{_+_}
// //       // println("cost = " + cost + " @ " + k)


// //     }
// //     // TODO: Figure out why cost creeps back up after getting minimized
// //     val gold = updated
// //     val result = getMem(offchipResult)
// //     val diff = gold.zip(result){_-_}

// //     // // Show all things
// //     // printData(sY, "Y vals: ")
// //     // println("X vals: ")
// //     // sX.map{row =>
// //     //   printData(row, "  ")
// //     //   }

// //     // printData(realW, "original model: ", 0)
// //     // printData(sW, "initial model: ", 0)
// //     // printData(gold, "Software: ", 0)
// //     // printData(result, "Hardware: ", 0)
// //     // printData(diff, "Diff: ", 1)

// //     // WriteData(realW, "OMn")
// //     // WriteData(sW, "Wn")
// //     // WriteData(sX, "Xn")

// //     // // Compare
// //     // val maxDPcheck = 9
// //     // println("Comparing Software - Hardware")
// //     // // For each element in updated model
// //     // for (i <- 0 until diff.length) {
// //     //   // Keep going until we find failure point
// //     //   var th = 0
// //     //   while (th < maxDPcheck+1) {
// //     //     if (diff(i) > 1/Math.pow(10,th) | diff(i) < -1/Math.pow(10,th)) {
// //     //       println(i + ": Diverge @ " + th + "dp: " + diff(i))
// //     //       th = 9999 // Break while loop
// //     //     }
// //     //     else if (th == maxDPcheck) {
// //     //       println(i + ": OK! (" + maxDPcheck + " DP)")
// //     //       th = 9999 // Break while loop
// //     //     }
// //     //     else {
// //     //       th = th + 1
// //     //     }
// //     //   }
// //     // }

// //     // WriteData(cost, "cost")
// //   }

// //   // def printData(data: Rep[ForgeArray[Elem]], string: String, forceShow: Int) = {
// //   //   if (data.length < 15 || forceShow == 1) {
// //   //     println(string + ": " + data.mkString(", "))
// //   //   }
// //   //   else {
// //   //     println(string + ": <suppressed>")
// //   //   }

// //   // }


// //   // def WriteData(data:Rep[ForgeArray[Elem]], tag: String) = {
// //   //   val dir = System.getProperty("user.dir") + "/"
// //   //   val f = new File(dir + "data" + tag )
// //   //   println("Writing test data to " + dir + "data" + tag)
// //   //   if (f.exists) {f.delete()}
// //   //   val pw = new PrintWriter(f)
// //   //   for (k <- 0 until data.length) {
// //   //     if (k == data.length) {
// //   //       pw.write(data(k).toString)
// //   //     }
// //   //     else {
// //   //       pw.write(data(k) + "\n")
// //   //     }
// //   //   }
// //   //   pw.close
// //   // }



// // }

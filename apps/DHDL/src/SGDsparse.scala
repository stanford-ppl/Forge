// import dhdl.compiler._
// import dhdl.library._
// import dhdl.shared._
// import scala.util.Random
// import java.io._
// import scala.io.Source

// object SGDsparseCompiler extends DHDLApplicationCompiler with SGDsparse
// object SGDsparseInterpreter extends DHDLApplicationInterpreter with SGDsparse
// trait SGDsparse extends DHDLApplication {
//   // type Elem = FixPt[Signed,B16,B16]
//   type Elem = Flt

//   override def stageArgNames = List("weightsDim", "batchDim", "numBatch", "alpha", "epochs")
//   lazy val weightsDim = stageArgOrElse[Int](0, 20)
//   lazy val batchDim= stageArgOrElse[Int](1, 20)
//   lazy val numBatch = stageArgOrElse[Int](2, 2)
//   lazy val alpha = stageArgOrElse[Double](3, 0.01)
//   lazy val epochs = stageArgOrElse[Int](4, 2)
//   lazy val sparsity = stageArgOrElse[Double](5, 0.01)
//   lazy val beta = stageArgOrElse[Int](6, 2)

//   lazy val W = ArgIn[SInt]("W") //modelDim
//   lazy val B = ArgIn[SInt]("B") //batchDim
//   lazy val N = ArgIn[SInt]("N") //numBatches
//   lazy val A = ArgIn[Elem]("A") //alpha
//   lazy val E = ArgIn[Elem]("E") //epochs
//   lazy val S = ArgIn[Elem]("S") //sparsity
//   lazy val BE = ArgIn[Elem]("BE") //beta
//   lazy val NNZ = ArgIn[Elem]("NNZ")

//   def SGDsparse(offchipWeights: Rep[OffChipMem[Elem]], offchipXVals: Rep[OffChipMem[Elem]], 
//           offchipXRows: Rep[OffChipMem[Elem]], offchipXCols: Rep[OffChipMem[Elem]], 
//           offchipXIds: Rep[OffChipMem[Elem]], offchipY: Rep[OffChipMem[Elem]],
//           offchipResult: Rep[OffChipMem[Elem]]) {
//     /*
//     Using CISR:
//       X = [- - A - - B
//            C - - - D E
//            - - - F - -
//            - G H - - -
//            - - - I - -
//            - J - K - -] ...
//       numSlots = 4
//       XVals = [A, C, F, G| B, D, F, H| -, E, I, J| - - - K ...]
//       XCols = [2, 0, 3, 1| 5, 4, 3, 2| -, 5, 3, 1| - - - 3 ...]
//       XLens = [2, 3, 1, 2, 1, 2...]

//     Using CSB:
//       X = [- - A |- - B
//            C - - |- D E
//            - - - |F - -
//            ------------
//            - G H |- - -
//            - - - |I - -
//            - J - |K - -] ...
//       beta = 3
//       XVals = [A, C, B, D, E, F, G, H, J, I, K, ...]
//       XRows = [0, 1, 0, 1, 1, 2, 0, 0, 2, 1, 2, ...]
//       XCols = [2, 0, 2, 1, 2, 0, 1, 2, 1, 0, 0, ...]
//       XIds =  [0, 2, 6, 9]

//     */

//   //   val tileWeights = BRAM[Elem](weightsDim)
//   //   val tileXVals = BRAM[Elem](batchNNZ)
//   //   val tileXRows = BRAM[Elem](batchNNZ)
//   //   val tileXCols = BRAM[Elem](batchNNZ)
//   //   val tileXIds = BRAM[Elem](bathDim*weightsDim/(beta*beta))
//   //   val tileY = BRAM[Elem](batchDim)
//   //   val tileUpdated = BRAM[Elem](weightsDim)
//   //   val tileYErr = BRAM[Elem](batchDim)


//   //   Sequential(1 by 1) {kk => 
//   //     Parallel {
//   //       tileWeights := offchipweights(0::weightsDim)
//   //       tileY := offchipY(0::batchDim)
//   //       tileXIds := offchipXIds(0::batchDim)
//   //     }
//   //     Sequential(epochs by 1){epoch => 
//   //       Sequential((batchNNZ*numBatch) by batchNNZ){ i =>
//   //         // Load data
//   //         Parallel {
//   //           tileXVals := offchipXVals(i::i+batchNNZ)
//   //           tileXRows := offchipXRows(i::i+batchNNZ)
//   //           tileXCols := offchipXCols(i::i+batchNNZ)
//   //         }
//   //         // Get error
//   //         // Update
//   //         Sequential(1 by 1){ k =>
//   //           tileUpdated(0) = 1
//   //         }
//   //       }
//   //     }
//   //     out(0::weightsDim) := tileUpdated
//   //   }
//   // }

//   // TODO: Why does cost creep back up after it converges?
//   //       Make global variables to set args in main
//   //       Fix DHDL implementation

//   def main() = {
//     val WD = weightsDim
//     val BD = batchDim
//     val NB = numBatch
//     val AL = alpha
//     val EP = epochs
//     val SP = sparsity
//     val BE = beta
//     val myNNZ = 10 //PH

//     // Set test data bounds
//     val maxWnoise = 0.25
//     val maxW = 3
//     val maxX = 1
//     val maxYnoise = 0.1

//     // Set offchip mems
// 		val offchipWeights = OffChipMem[Elem]("ocWeights", WD)
// 		val offchipX = OffChipMem[Elem]("ocX", BD*NB, WD)
// 		val offchipY = OffChipMem[Elem]("ocY", BD*NB)
//     val offchipResult = OffChipMem[Elem]("ocResult", WD)

//     // Create test data
//     val realW = Array.fill(WD){random[Elem](maxW)}
//     val noiseW = Array.fill(WD){random[Elem](maxWnoise*2)}
//     val sW = realW.zip(noiseW){_+_-maxWnoise}
//     var sX = Array.fill(BD*NB){ Array.fill(WD){random[Elem](maxX)} }
//     for (i <- 0 until BD) {
//       for (j <- 0 until NB) {
//         if (random(1) > sparsity) {
//           sX(i,j) = 0
//         }
//       }
//     }
//     val realY = sX.map{row => row.zip(realW){_*_}.reduce(_+_)}
//     val noiseY = Array.fill(BD*NB){random[Elem](maxYnoise*2)}
//     val sY = realY.zip(noiseY){_+_-maxYnoise}

//     setArg(W, WD)
//     setArg(B, BD)
//     setArg(N, NB)
//     setArg(A, AL)
//     setArg(E, EP)
//     setArg(BE, BE)
//     setArg(NNZ, myNNZ) //PH

//     setMem(offchipWeights, sW)
//     setMem(offchipXVals, sX.flatten) //PH
//     setMem(offchipXRows, sX.flatten) //PH
//     setMem(offchipXCols, sX.flatten) //PH
//     setMem(offchipXIds, sX.flatten) //PH
//     setMem(offchipY, sY)
//     Accel{ SGDsparse(offchipWeights, offchipXVals, 
//                      offchipXRows, offchipXCols, offchipXIds,
//                      offchipY, offchipResult) }

//     // SGDsparse in Scala
//     var updated = sW.map{case any => any}
//     var y_err = sY.map{Createse any => any}
//     var y_hat = sY.map{case any => any}
//     for (k <- 0 until epochs) {
//       for (b <- 0 until NB) {
//         // 1. Compute y_hat
//         y_hat = Array.tabulate(BD){i =>
//           val xRow = sX(i + b*BD)
//           xRow.zip(updated){_*_}.reduce(_+_)
//         }
//         // 2. Compute y_err
//         val thissY = Array.tabulate(BD){i =>
//           sY(i + b*BD)
//         }
//         var y_err = thissY.zip(y_hat){_-_}
//         // val a = y_err.zip(y_err){_*_}
//         // val cost = a.reduce{_+_}
//         // println("cost = " + cost + " @ " + k)
//         // 3. Compute gradient
//         val gradient = Array.tabulate(WD){i =>
//           var xCol = thissY.map{case any => any}
//           var flatX = sX.flatten
//           for (j <- 0 until BD) {
//             xCol(j)=flatX(i + j*WD + b*BD*WD)
//           }
//           xCol.zip(y_err){_*_}.reduce{_+_}
//         }
//         updated = Array.tabulate(WD){i => 
//           val movement = gradient(i)*alpha/BD
//           updated(i) + movement
//         }
//       }
//       // // Cost tracker
//       // val a = updated.zip(realW){_-_}
//       // val b = a.zip(a){_*_}
//       // val cost = b.reduce{_+_}
//       // println("cost = " + cost + " @ " + k)

//     }
//     // TODO: Figure out why cost creeps back up after getting minimized
//     val gold = updated
//     val result = getMem(offchipResult)
//     val diff = gold.zip(result){_-_}

//     // // Show all things
//     // printData(sY, "Y vals: ")
//     // println("X vals: ")
//     // sX.map{row =>
//     //   printData(row, "  ")
//     //   }
//     printData(realW, "original model: ", 0)
//     printData(sW, "initial model: ", 0)
//     printData(gold, "Software: ", 0)
//     printData(result, "Hardware: ", 0)
//     printData(diff, "Diff: ", 1)

//     // // Compare
//     // val maxDPcheck = 9
//     // println("Comparing Software - Hardware")
//     // // For each element in updated model
//     // for (i <- 0 until diff.length) {
//     //   // Keep going until we find failure point
//     //   var th = 0
//     //   while (th < maxDPcheck+1) {
//     //     if (diff(i) > 1/Math.pow(10,th) | diff(i) < -1/Math.pow(10,th)) {
//     //       println(i + ": Diverge @ " + th + "dp: " + diff(i))
//     //       th = 9999 // Break while loop
//     //     }
//     //     else if (th == maxDPcheck) {
//     //       println(i + ": OK! (" + maxDPcheck + " DP)")
//     //       th = 9999 // Break while loop
//     //     }
//     //     else {
//     //       th = th + 1
//     //     }
//     //   }
//     // }

//     // WriteData(cost, "cost")
// 	}

//   def printData(data: Rep[ForgeArray[Elem]], string: String, forceShow: Int) = {
//     if (data.length < 15 || forceShow == 1) {
//       println(string + ": " + data.mkString(", "))
//     }
//     else {
//       println(string + ": <suppressed>")
//     }

//   }

//   // def WriteData(data:Rep[Elem], tag:String) = {
//   //   val dir = System.getProperty("user.dir") + "/"
//   //   val f = new File(dir + "data" + tag )
//   //   println("Writing test data to " + dir + "data" + tag)
//   //   if (f.exists) {f.delete()}
//   //   val pw = new PrintWriter(f)
//   //   for (k <- 0 to data.length-1) {
//   //     if (k == data.length-1) {
//   //       pw.write(data(k).toString)
//   //     }
//   //     else {
//   //       pw.write(data(k) + "\n")
//   //     }
//   //   }
//   //   pw.close
//   // }



// }

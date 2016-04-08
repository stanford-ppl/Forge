package ppl.dsl.forge
package dsls
package dhdl

trait DHDLGlobalAnalysis {
  this: DHDLDSL =>

  /**
   * In DHDL, a "global" is any value which is solely a function of input arguments
   * and constants. These are computed prior to starting the main computation, and
   * therefore appear constant to the majority of the program.
   **/

  def importGlobalAnalysis() = {
    val GlobalAnalyzer = analyzer("Global")
    val Prim = lookupGrp("DHDLPrim")
    val Tpes = lookupGrp("Tpes")
    val Ctrl = lookupGrp("BasicCtrl")
    val Lifts = lookupGrp("ConstLifts")
    val Reg = lookupTpe("Reg")

    def generateRule(args: List[String]) = "isGlobal(lhs) = " + args.map(arg => "isGlobal("+arg+")").mkString(" && ")

    val GlobalAnalysisRules = withAnalyzer(GlobalAnalyzer)
    GlobalAnalysisRules {
      analyze(Lifts, "constBit") using rule ${ isGlobal(lhs) = true }
      analyze(Lifts, "constFixPt") using rule ${ isGlobal(lhs) = true }
      analyze(Lifts, "constFltPt") using rule ${ isGlobal(lhs) = true }
      analyze(Reg, "reg_read") using rule ${ isGlobal(lhs) = regType($0) == ArgumentIn }

      // Holy hackery, batman!
      for (op <- Prim.nodes ++ Tpes.nodes ++ Ctrl.nodes) {
        analyze(op) using rule { generateRule(op.argNames) }
      }
    }
  }

}

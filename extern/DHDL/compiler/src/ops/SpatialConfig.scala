package dhdl.compiler.ops

object SpatialConfig {
  import ppl.delite.framework.Config._

  lazy val enableDSE = getProperty("spatial.dse", "false") != "false"
  lazy val genCGRA = getProperty("spatial.cgra", "false") != "false"
  lazy val debugging = getProperty("spatial.debugging", "false") != "false"
  lazy val verbose = getProperty("spatial.verbose", "false") != "false" || debugging
  lazy val pirdebug = getProperty("spatial.pirdebug", "false") != "false"
  lazy val loudModels = getProperty("spatial.loudmodels", "false") != "false"
}

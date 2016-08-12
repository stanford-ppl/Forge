package dhdl.compiler.ops

object SpatialConfig {
  import ppl.delite.framework.Config._

  var enableDSE: Boolean = getProperty("spatial.dse", "false") != "false"
  var genCGRA: Boolean = getProperty("spatial.cgra", "false") != "false"
  var debugging: Boolean = getProperty("spatial.debugging", "false") != "false"
  var verbose: Boolean = getProperty("spatial.verbose", "false") != "false" || debugging
  var pirdebug: Boolean = getProperty("spatial.pirdebug", "false") != "false"
  var loudModels: Boolean = getProperty("spatial.loudmodels", "false") != "false"
}

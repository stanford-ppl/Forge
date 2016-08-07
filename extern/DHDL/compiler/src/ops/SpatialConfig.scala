package dhdl.compiler.ops

object SpatialConfig {
  import ppl.delite.framework.Config._

  val enableDSE = getProperty("spatial.dse", "false") != "false"
  val genCGRA = getProperty("spatial.cgra", "true") != "false"
}

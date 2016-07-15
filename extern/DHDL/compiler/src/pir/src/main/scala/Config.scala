package dhdl

object Config {
  private def getProperty(prop: String, default: String) = {
    val p1 = System.getProperty(prop)
    val p2 = System.getProperty(prop.substring(1))
    if (p1 != null && p2 != null) {
      assert(p1 == p2, "ERROR: conflicting properties")
      p1
    }
    else if (p1 != null) p1 else if (p2 != null) p2 else default
  }

  // Properties go here
  var test = getProperty("dhdl.test", "false") == "true"
  var newMemAPI = getProperty("dhdl.newMemAPI", "false") == "true"
  //var eval = getProperty("dhdl.eval", "false") == "true"
  var genScala = getProperty("dhdl.scala", "true") == "true"
  var genDot = getProperty("dhdl.dot", "true") == "true"
  var genMaxJ = getProperty("dhdl.maxj", "true") == "true"
  var quick = getProperty("dhdl.quick", "false") == "true"

  var dse = false

  if (quick) {
    genMaxJ = false
    genDot = false
    genScala = false
    test = false
  }
}

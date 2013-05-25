package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait BasicMathOps {
  this: OptiLADSL => 
 
  def importBasicMathOps() {
    val Math = grp("BasicMath")
      
    // aliases for Math._  
    direct (Math) ("ceil", Nil, MDouble :: MInt) implements composite ${ Math.ceil($0).toInt }    
    direct (Math) ("abs", Nil, MDouble :: MDouble) implements composite ${ Math.abs($0) }    
    direct (Math) ("exp", Nil, MDouble :: MDouble) implements composite ${ Math.exp($0) }    
    direct (Math) ("log", Nil, MDouble :: MDouble) implements composite ${ Math.log($0) }    
  }
}
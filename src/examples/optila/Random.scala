package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

trait RandomOps {
  this: OptiLADSL => 
 
  def importRandomOps() {
    val Rand = grp("Rand")
    val A = tpePar("A")
    
    direct (Rand) ("random", A, Nil :: A) implements composite ${
      val mA = manifest[A]
      mA match {
        case Manifest.Double => optila_rand_double.AsInstanceOf[A]
        case Manifest.Float => optila_rand_float.AsInstanceOf[A]
        case Manifest.Int => optila_rand_int.AsInstanceOf[A]
        case Manifest.Boolean => optila_rand_boolean.AsInstanceOf[A]     
        case _ => sys.error("no random implementation available for type " + mA.toString) 
      }
    }
    
    compiler (Rand) ("optila_rand_double", Nil, Nil :: MDouble, effect = simple) implements codegen($cala, ${
      Global.randRef.nextDouble()
    })
    compiler (Rand) ("optila_rand_float", Nil, Nil :: MFloat, effect = simple) implements codegen($cala, ${
      Global.randRef.nextFloat()
    })
    compiler (Rand) ("optila_rand_int", Nil, Nil :: MInt, effect = simple) implements codegen($cala, ${
      Global.randRef.nextInt()
    })
    compiler (Rand) ("optila_rand_boolean", Nil, Nil :: MBoolean, effect = simple) implements codegen($cala, ${
      Global.randRef.nextBoolean()
    })
  }    
}

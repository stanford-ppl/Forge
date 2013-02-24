import optiwrangle.compiler._
import optiwrangle.library._
import optiwrangle.shared._

object HelloSimpleCompiler extends OptiWrangleApplicationCompiler with HelloSimple 
object HelloSimpleInterpreter extends OptiWrangleApplicationInterpreter with HelloSimple 

trait HelloSimple extends OptiWrangleApplication { 
  def main() = {
    println("hello world")    

    val tal = Table(1)
    val test = Array("Why Hello to you too")
    tal(0) = test.Clone
    val tal2 = tal.cut("o")
    println("The result is: " + tal2(0).apply(0))
  
    //val inFile = "/afs/cs.stanford.edu/u/gibbons4/data/hello.csv"
    val inFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.in.csv"
    val tff = Table(inFile, ",")
    val tfc = tff.cut("\"")
    val tfdrop = tfc.drop(0)
    val tfdelete = tfdrop.delete(1, "-126.", 0)
    val tfdr = tfdelete.delete(3)
    val tfp = tfdr.promote(0) 
 
    val outFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.out.csv"
    tfp.write(outFile, ",")  

    tfp.test()
  } 
}

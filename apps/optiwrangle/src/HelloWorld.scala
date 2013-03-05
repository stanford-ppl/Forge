import optiwrangle.compiler._
import optiwrangle.library._
import optiwrangle.shared._

object HelloSimpleCompiler extends OptiWrangleApplicationCompiler with HelloSimple 
object HelloSimpleInterpreter extends OptiWrangleApplicationInterpreter with HelloSimple 

trait HelloSimple extends OptiWrangleApplication { 
  def main() = {
    println("hello world")    
/*
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
    val tfe = tfp.extract(2, "121") 

    val outFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.out.csv"
    tfe.write(outFile, ",")  
*/

    val inFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.in.csv"
    var dw = DataWrangler(inFile, ",")
/*
    dw = dw.cut("on", "\"")
    dw = dw.drop(0)
    dw = dw.split("on", "12")
    dw = dw.delete()
*/
    dw=dw.transpose()
    dw=dw.transpose()
    dw=dw.cut("on", "0")
    //dw=dw.cut("on", "1", 1)
    dw=dw.split("on", "1")
    dw=dw.promote(0)
    dw=dw.drop(0)

    //dw=dw.cut("on", "2", false) // should this be a boolean? // dw.cutAll() ?
    //dw=dw.cut("on", "0", true) // repeatedly / max
    //dw=dw.cut("between", (3, 5), 4)
    //dw=dw.cut("between", ("3", "5"), 4)
    //dw=dw.cut("before", 3, 4)
    //dw=dw.cut("before", "3", 4)
    //dw=dw.cut("after", 5, 4)
    //dw=dw.cut("after", "5 hello", 4)
    //dw.cut(on=",")
    //dw.cutOnLeftRepeatedly

    //dw.cut(Map("on", ","))

    val outFile = "/afs/cs.stanford.edu/u/gibbons4/data/flickr.out.csv"
    dw.write(outFile, ",")
  } 
}

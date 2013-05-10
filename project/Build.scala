import sbt._
import Keys._
import sbt.std.TaskStreams

import java.io.File
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}

object ForgeBuild extends Build with ForgePreprocessor {
  val virtScala = Option(System.getenv("SCALA_VIRTUALIZED_VERSION")).getOrElse("2.10.0")
  val virtualization_lms_core = "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT"
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test"
 
  // -DshowSuppressedErrors=false
    System.setProperty("showSuppressedErrors", "false")
    
  // hook in Forge preprocessor
  val forgeSettings = Defaults.defaultSettings ++ Seq(
    organization := "stanford-ppl",
    sources in Compile <<= (sourceManaged in Compile, sources in Compile, streams) map { (dir,files,s) => files.map(preprocess(dir,_,s)) }
  )

  lazy val forge = Project("Forge", file("."), settings = forgeSettings)
}

trait ForgePreprocessor {
  var preprocessorEnabled = true
  var preprocessorDebug = false
  
  // investigate: should the preprocessor be implemented using parser combinators or some other strategy?
  
  def preprocess(srcManaged: File, src: File, stream: TaskStreams[_]) = {
    def err(s: String) = 
      error("[forge preprocessor]: in " + src.getName() + ", " + s)
      //stream.log.error("[forge preprocessor]: in " + src.getName() + ", " + s)
    def warn(s: String) = stream.log.warn("[forge preprocessor]: in " + src.getName() + ", " + s)
    
    // -- helpers
    
    abstract class CommentScope
    object NoComment extends CommentScope
    object CommentLine extends CommentScope
    object CommentBlock extends CommentScope {
      var nest = 0
    }
    
    // updates the comment scope based on the current characters
    def scanComment(scope: CommentScope, start: Int, input: Array[Byte]): (Int, CommentScope) = {
      if (start > input.length-1) (start, scope)
      else (input(start),input(start+1)) match {
        case ('/','/') if scope == NoComment => (start+2, CommentLine)
        case ('\n', _) if scope == CommentLine => (start+1, NoComment)
        case ('/','*') if scope != CommentLine => 
          CommentBlock.nest += 1
          (start+2, CommentBlock)
        case ('*','/') if scope == CommentBlock =>   
          CommentBlock.nest -= 1
          if (CommentBlock.nest == 0) (start+2, NoComment)
          else (start+2, CommentBlock)
        case _ => (start, scope)
      }
    }
    
    def endOfWord(c: Byte): Boolean = c match {
      case ' ' | ',' | '.' | '+' | ')' | '}' | '(' | '{' | '\n' => true  // fixme: platform-specific line sep
      case _ => false
    }
    
    def blockIndentation(start: Int, input: Array[Byte]): Int = {
      var i = start
      
      // scan to first new line
      while (i < input.length && input(i) != '\n') i += 1
      
      // scan to next non-blank
      var j = i+1
      while (j < input.length && input(j) == ' ') j += 1
      
      j-i-1
    }
    
    def scanTo$(start: Int, input: Array[Byte]): Int = {
      var i = start
      var scope: CommentScope = NoComment
      while (i < input.length-1) {
        val (nextI, nextScope) = scanComment(scope, i, input)
        i = nextI; scope = nextScope;
        if (scope == NoComment && input(i) == '$' && input(i+1) == '{') return i        
        i += 1
      }
      i+1 // not found
    }  

    /**
     * These enumerate the possible lexically enclosing op statements to a formatted block,
     * which are needed as arguments in the rewritten quoted args. We only attempt to do
     * this when the preprocessor encounters a block symbol, $b{..}
     */
    abstract class OpEncoding
    case class OpFromIs(name: String) extends OpEncoding {
      override def toString = "lookup(\""+name+"\")"
    }    
    case class OpFromImpl(binding: String) extends OpEncoding {
      override def toString = binding
    }
    case class OpFromOp(grp: String, name: String) extends OpEncoding {
      override def toString = "lookup(\""+grp+"\",\""+name+"\")"
    }    
    
    def scanBackToOp(start: Int, input: Array[Byte]): OpEncoding = {
      val words = Array("is","op","impl") // enclosing op definers
      var i = start
      var foundWord = ""
      val maxWordLength = words.map(_.length).max
      while (i > maxWordLength && foundWord == "") {
        for (w <- words) if (i > w.length) {
          if (input(i-w.length-1) == ' ' && endOfWord(input(i)) && input.slice(i-w.length,i).corresponds(w.getBytes){ _ == _ }) // could loop instead of slicing if we need to be more efficient
            foundWord = w          
        }
        i -= 1
      }
      
      foundWord match {
        case "is" => 
          // scan backwards to extract the name
          while (i > 1 && input(i) != '"') i -= 1
          val endNameIndex = i          
          i -= 1
          while (i > 1 && input(i) != '"') i -= 1
          val startNameIndex = i          
          
          if (endNameIndex == 0 || (startNameIndex == 0 && input(i) != '"')) err("could not find preceding op name before 'is' declaration")
          OpFromIs(new String(input.slice(startNameIndex+1,endNameIndex)))
        
        case "op" =>
          // scan forwards to extract the grp and op name
          while (i < start && input(i) != '(') i += 1
          val startGrpIndex = i
          while (i < start && input(i) != ')') i += 1
          val endGrpIndex = i
          while (i < start && input(i) != '"') i += 1
          val startNameIndex = i
          i += 1
          while (i < start && input(i) != '"') i += 1
          val endNameIndex = i
          
          if (endNameIndex == start) err("could not find grp name or op name following 'op' declaration")
          OpFromOp(new String(input.slice(startGrpIndex+1,endGrpIndex)), new String(input.slice(startNameIndex+1,endNameIndex)))
        
        case "impl" =>  
          // scan forwards to extract the op name binding
          while (i < start && input(i) != '(') i += 1
          val startOpIndex = i
          while (i < start && input(i) != ')') i += 1
          val endOpIndex = i
          
          if (endOpIndex == start) err("could not find op binding following 'impl' declaration")
          OpFromImpl(new String(input.slice(startOpIndex+1,endOpIndex)))
        
        case _ => err("could not find lexically enclosing op declaration for formatted block")
      }      
    }     
    
    def isBlockArg(arg: String) = { arg.startsWith("b[") && arg.endsWith("]") }
    
    def writeFormattedBlock(start: Int, input: Array[Byte], output: ArrayBuffer[Byte]): Int = {
      var i = start
      var bracketScope = 1
      var endBlock = false      
      val args = new HashSet[String]()
      val argMap = new HashMap[String,String]()
      val startCommentIndices = new ArrayBuffer[Int]()
      val endCommentIndices = new ArrayBuffer[Int]()
      
      // scan the block forwards, collecting identifiers
      var scope: CommentScope = NoComment
      while (i < input.length-1 && !endBlock) {
        val (nextI, nextScope) = scanComment(scope, i, input)
        if (scope == NoComment && nextScope != NoComment) {
          startCommentIndices += i
        }
        else if (scope != NoComment && nextScope == NoComment) {
          endCommentIndices += nextI
        }
        i = nextI; scope = nextScope;      
        
        if (scope == NoComment) {
          if (input(i) == '{')
            bracketScope += 1          
          else if (input(i) == '}') {
            bracketScope -= 1
          }        
          if (bracketScope == 0) {
            endBlock = true
          }        
        
          if (input(i) == '$') { // found identifier
            var j = i
            while (j < input.length && !endOfWord(input(j))) j += 1
            args += new String(input.slice(i+1,j))
            i = j-1
          }
        }
        
        i += 1  
      }
      
      // look for block functions that require us to have the op identifier
      // we don't do this in general because it's not necessary and doesn't play well with overloading
      val enclosingOp = 
        if (args.exists(isBlockArg)) {        
          // scan backwards until the lexically enclosing op identifier, which can be one of:
          //   <name> is ...
          //   op (grp) (name, ....)
          //   impl (binding) (...)      
          Some(scanBackToOp(i, input))
        }
        else None            
      
      // write the formatted block    
      val indent = " "*blockIndentation(start, input)
      val indentInterior = "  " + indent 
      val nl = System.getProperty("line.separator")
      output ++= ("{" + nl).getBytes        
      for (a <- args) {
        // need to add fix up block args and a prefix for positional args
        try {
          if (isBlockArg(a)) {
            val a2 = a.slice(2,a.length-1)
            val i = a2.toInt
            val z: String = "arg"+a2 // wtf?
            argMap += (a -> z)
            output ++= (indentInterior + "val arg" + a2 + " = quotedArg("+a2+", "+enclosingOp.get+")" + nl).getBytes                    
          }
          else {
            val i = a.toInt
            val z: String = "arg"+a // wtf?
            argMap += (a -> z)
            output ++= (indentInterior + "val arg" + a + " = quotedArg("+a+")" + nl).getBytes                    
          }
        }
        catch {
          case _:NumberFormatException => 
            if (isBlockArg(a)) {
              val a2 = a.slice(2,a.length-1)
              argMap += (a -> a2)
              output ++= (indentInterior + "val " + a2 + " = quotedArg(\""+a2+"\", "+enclosingOp.get+")" + nl).getBytes
            }
            else {
              output ++= (indentInterior + "val " + a + " = quotedArg(\""+a+"\")" + nl).getBytes
            }
        }        
      }
      output ++= (indentInterior + "s\"\"\"").getBytes
      
      // cut out comments
      val strBlockBuf = new StringBuilder()
      var j = start
      for (k <- 0 until startCommentIndices.length) {
        strBlockBuf ++= new String(input.slice(j, startCommentIndices(k)))
        if (endCommentIndices.length > k) j = endCommentIndices(k)
      }
      strBlockBuf ++= new String(input.slice(j, i-1))
      var strBlock = strBlockBuf.toString
      // remap args inside the input block      
      for (a <- args) {
        if (argMap.contains(a)) {
          strBlock = strBlock.replace("$"+a, "$"+argMap(a))
        }
      }
      // swallow the indentation, since we'll re-indent inside Forge anyways
      strBlock = strBlock.replace(indent, "").trim
      
      output ++= strBlock.getBytes      
      output ++= ("\"\"\"" + nl).getBytes      
      output ++= (indent + "}").getBytes
        
      // return block end index
      i
    }
    
    def expand(input: Array[Byte]): Option[Array[Byte]] = {
      val output = new ArrayBuffer[Byte]()

      var start = 0
      var i = scanTo$(start, input)
      val foundBlock = i < input.length
      
      while (i < input.length) {      
        output ++= input.slice(start,i)
        val blockEnd = writeFormattedBlock(i+2, input, output)
        start = blockEnd
        i = scanTo$(start, input)
      }

      if (foundBlock) {
        // write remaining
        output ++= input.slice(start, i)
        Some(output.toArray) 
      } 
      else None
    }
    
    // --
        
    // preprocess entry
        
    if (preprocessorEnabled) {
      val dest = srcManaged / "preprocess" / src.getName()               
      if (!preprocessorDebug && dest.exists() && dest.lastModified() > src.lastModified()) {
        dest
      }   
      else {
        val input = IO.readBytes(src)        
        val output = expand(input)
        if (output.isDefined) {
          IO.write(dest, output.get)
          dest
        }
        else {
          src
        }    
      }
    }
    else {
      src
    }
  }
  
  
}

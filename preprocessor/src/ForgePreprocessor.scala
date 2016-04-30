package forge
package preprocessor

import sbt._
import sbt.std.TaskStreams
import java.io.File
import scala.collection.mutable.{ArrayBuffer,HashSet,HashMap}

trait ForgePreprocessor {
  var preprocessorEnabled = true
  var preprocessorDebug = false

  // investigate: should the preprocessor be implemented using parser combinators or some other strategy?

  def preprocess(srcManaged: File, src: File, stream: TaskStreams[_]) = {
    def err(s: String) = error("[forge preprocessor]: in " + src.getName() + ", " + s)
      //stream.log.error("[forge preprocessor]: in " + src.getName() + ", " + s)
    def warn(s: String) = stream.log.warn("[forge preprocessor]: in " + src.getName() + ", " + s)

    // -- helpers

    class Pattern(pattern: String) {
      assert(pattern.length > 0, "Cannot create pattern of length 0")
      private val bytes = pattern.getBytes.zipWithIndex

      def unapply(x: (Int, Array[Byte])) = {
        val (start,input) = x
        bytes.forall{case (b,i) => (start+i) < input.length && input(start+i) == b}
      }
      def length = pattern.length
    }
    object Pattern {
      def apply(x: String) = new Pattern(x)
    }

    trait Feature
    object NoFeature extends Feature
    object CodeBlock extends Pattern("${") with Feature
    object DSLAnnotation  extends Pattern("@dsl") with Feature

    trait CommentScope
    object NoComment extends CommentScope
    object CommentBlock extends Pattern("/*") with CommentScope { var nest = 0 }
    object DocumentBlock extends Pattern("/**") with CommentScope with Feature { var in = false }
    object SummaryBlock extends Pattern("/***") with CommentScope with Feature { var in = false }
    object CommentLine extends Pattern("//") with CommentScope
    object EndBlock extends Pattern("*/")
    object NewLine extends Pattern("\n")


    def endOfWord(c: Byte): Boolean = c match {
      case ' ' | ',' | '.' | ':' | '+' | '*' | '-' | '/' | ')' | '}' | '(' | '{' | '\n' => true  // fixme: platform-specific line sep
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

    // updates the comment scope based on the current characters
    def scanComment(scope: CommentScope, start: Int, input: Array[Byte], allowDoc: Boolean): (Int, CommentScope) = {
      if (start > input.length-1) (start, scope)
      else ((start,input)) match {
        case CommentLine()   if scope == NoComment    => (start+2, CommentLine)
        case NewLine()       if scope == CommentLine  => (start+1, NoComment)
        case SummaryBlock()  if allowDoc => CommentBlock.nest += 1; SummaryBlock.in = true;  (start, SummaryBlock)
        case DocumentBlock() if allowDoc => CommentBlock.nest += 1; DocumentBlock.in = true; (start, DocumentBlock)
        case CommentBlock()  if scope != CommentLine  => CommentBlock.nest += 1; (start+2, CommentBlock)
        case EndBlock()      if scope == CommentBlock || scope == DocumentBlock || scope == SummaryBlock =>
          CommentBlock.nest -= 1
          if (CommentBlock.nest == 0) { SummaryBlock.in = false; DocumentBlock.in = false }
          val newScope = if (CommentBlock.nest == 0) NoComment
                         else if (SummaryBlock.in)   SummaryBlock
                         else if (DocumentBlock.in)  DocumentBlock
                         else                        CommentBlock
          (start+2, newScope)
        case _ => (start, scope)
      }
    }

    def scanToFeature(start: Int, input: Array[Byte], isDSLScope: Boolean): (Feature, Int) = {
      var i = start
      var scope: CommentScope = NoComment
      var feature: Feature = NoFeature
      while (i < input.length && feature == NoFeature) {
        val (nextI, nextScope) = scanComment(scope, i, input, isDSLScope && scope == NoComment)
        i = nextI; scope = nextScope;

        if (scope == NoComment) ((i,input)) match {
          case CodeBlock() => feature = CodeBlock
          case DSLAnnotation() => feature = DSLAnnotation
          case _ => i += 1
        }
        else if (scope == DocumentBlock) feature = DocumentBlock
        else if (scope == SummaryBlock) feature = SummaryBlock
        else i += 1
      }
      (feature, i)
    }

    /**
     * These enumerate the possible lexically enclosing op statements to a formatted block,
     * which are needed as arguments in the rewritten quoted args. We only attempt to do
     * this when the preprocessor encounters a block symbol, $b{..}
     */
    abstract class OpEncoding
    case class OpFromTpeScope(name: String) extends OpEncoding {
      override def toString = "lookupOp(\""+name+"\")"
    }
    case class OpFromImpl(binding: String) extends OpEncoding {
      override def toString = binding
    }
    case class OpFromOp(grp: String, name: String) extends OpEncoding {
      override def toString = "lookupOp("+grp+",\""+name+"\")"
    }

    val opStarters = {
      /*var list = List[String]()
      for (back <- List("", "internal", "compiler", "library")) {
        for (mtyp <- List("infix", "static", "direct")) {
          list :+= back + (if (back == "") "" else ".") + mtyp
      }}*/
      List("internal", "compiler", "library", "infix", "static", "direct") // ++ list
    }

    def scanBackToOp(start: Int, input: Array[Byte]): OpEncoding = {
      val words = opStarters ++ List("impl", "lower") // enclosing op definers
      //println("words: " + words.mkString(", "))
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
        case word if opStarters.contains(word) =>
          var inTpeScope = true
          var startGrpIndex = -1
          var endGrpIndex = -1

          // scan forwards to extract the grp name, only if we are not in a tpe scope
          while (i < start && input(i) != '(') i += 1
          if (input(i+1) != '"') {
            inTpeScope = false
            startGrpIndex = i
            while (i < start && input(i) != ')') i += 1
            endGrpIndex = i
          }

          // scan forwards to extract the op name
          while (i < start && input(i) != '"') i += 1
          val startNameIndex = i
          i += 1
          while (i < start && input(i) != '"') i += 1
          val endNameIndex = i
          if (endNameIndex == start) err("could not find op name following op declaration")

          if (!inTpeScope) {
            if (startGrpIndex == -1 || endGrpIndex == -1) err("could not find grp name following op declaration")
            OpFromOp(new String(input.slice(startGrpIndex+1,endGrpIndex)), new String(input.slice(startNameIndex+1,endNameIndex)))
          }
          else {
            OpFromTpeScope(new String(input.slice(startNameIndex+1,endNameIndex)))
          }

        case "impl" =>
          // scan forwards to extract the op name binding
          while (i < start && input(i) != '(') i += 1
          val startOpIndex = i
          while (i < start && input(i) != ')') i += 1
          val endOpIndex = i

          if (endOpIndex == start) err("could not find op binding following 'impl' declaration")
          OpFromImpl(new String(input.slice(startOpIndex+1,endOpIndex)))

        // TODO: Only works in transformer scope right now
        /*case "lower" =>
          while (i < start && input(i) != '(') i += 1
          val startOpIndex = i
          while (i < start && input(i) != ')' && input(i) != ',') i += 1
          val endOpIndex = i

          if (input(i) == ',') {
            // scan forwards to extract the op name
            while (i < start && input(i) != '"') i += 1
            val startNameIndex = i
            i += 1
            while (i < start && input(i) != '"') i += 1
            val endNameIndex = i
            OpFromOp(new String(input.slice(startOpIndex+1,endOpIndex)), new String(input.slice(startNameIndex+1,endNameIndex)))
          }
          else
            OpFromImpl(new String(input.slice(startOpIndex+1,endOpIndex)))*/

        case _ => err("could not find lexically enclosing op declaration for formatted block")
      }
    }

    def isBlockArg(arg: String) = arg.startsWith("b[")
    def isBoundArg(arg: String) = arg.startsWith("a[")

    def endOfBoundArgName(arg: String) = arg.indexOf(']')
    def parseBoundArguments(start: Int, input: Array[Byte], args: ArrayBuffer[String]): Int = {
      var j = start
      while (j < input.length && input(j) != ']') j += 1
      val arg = new String(input.slice(start,j+1))
      if (!args.contains(arg)) {
        args += arg
      }
      j
    }

    def endOfBlockArgName(arg: String) = if (arg.contains('(')) arg.indexOf('(') - 1 else arg.indexOf(']')
    def getCapturedArgs(arg: String, argMap: HashMap[String,String]) = {
      if (arg.contains('(')) {
        val s = arg.slice(arg.indexOf('(')+1,arg.lastIndexOf(')'))
        if (argMap.contains(s.drop(1))) Array(mapNestedArgs(s,argMap))
        else s.split(",").map(mapNestedArgs(_,argMap))
      }
      else Array[String]()
    }
    def mapNestedArgs(arg: String, argMap: HashMap[String,String]): String = {
      assert(arg.length > 0)
      "s\"\"\"" + arg(0) + argMap.getOrElse(arg.drop(1),arg.drop(1)) + "\"\"\"" // still has $ at this point
    }

    def parseBlockArguments(start: Int, input: Array[Byte], args: ArrayBuffer[String]): Int = {
      var j = start
      while (j < input.length && !endOfWord(input(j))) j += 1

      var arg = new String(input.slice(start,j))
      if (isBlockArg(arg) && input(j) == '(') {
        // add captured parameters as well
        var c = j
        var parenScope = 1
        while (c < input.length && parenScope != 0) {
          if (input(c) == '$') c = parseBlockArguments(c+1, input, args)
          else c += 1

          if (input(c) == '(') parenScope += 1
          else if (input(c) == ')') parenScope -= 1
        }
        arg = arg + new String(input.slice(j, c+1))
        j = c+1
      }
      if (!args.contains(arg)) { // slow, but order matters here
        args += arg
      }
      j
    }

    def endOfTypeArgName(arg: String) = arg.indexOf(']')
    def parseTypeArgument(start: Int, input: Array[Byte], tpeArgs: ArrayBuffer[String]): Int = {
      var j = start+2 // skip ahead of opening bracket
      while (j < input.length && input(j) != ']') {
        if (input(j) == '[')
          err("higher-kinded type arguments are not currently allowed in formatted blocks")
        j += 1
      }

      // a sketch, similar to parseBlockArguments, for allowing higher-kinded type args:
      // var bracketScope = 1
      // while (j < input.length && bracketScope != 0)) {
      //   // check for higher-kinded type arguments
      //   if (j+2 < input.length && input(j) == '$' && input(j+1) == 't' && input(j+2) == '[')
      //     j = parseTypeArgument(j+1, input, args)
      //   else
      //     j += 1
      //
      //   if (input(j) == '[') bracketScope += 1
      //   else if (input(j) == ']') bracketScope -= 1
      // }

      j += 1
      val tpeArg = new String(input.slice(start, j))
      if (!tpeArgs.contains(tpeArg)) {
        tpeArgs += tpeArg
      }
      j
    }

    def writeFormattedBlock(start: Int, input: Array[Byte], output: ArrayBuffer[Byte]): Int = {
      var i = start
      var bracketScope = 1
      var endBlock = false
      val args = new ArrayBuffer[String]()
      val argMap = new HashMap[String,String]()
      val tpeArgs = new ArrayBuffer[String]()
      val tpeArgMap = new HashMap[String,String]()
      val startCommentIndices = new ArrayBuffer[Int]()
      val endCommentIndices = new ArrayBuffer[Int]()

      // scan the block forwards, collecting identifiers
      var scope: CommentScope = NoComment
      while (i < input.length-1 && !endBlock) {
        val (nextI, nextScope) = scanComment(scope, i, input, false)
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

          if (input(i) == '$' && input(i-1) != '\\') { // found identifier
            val j =
              if (i+2 < input.length && input(i+1) == 't' && input(i+2) == '[')
                parseTypeArgument(i+1, input, tpeArgs)
              else if (i+2 < input.length && input(i+1) == 'a' && input(i+2) == '[')
                parseBoundArguments(i+1,input,args)
              else
                parseBlockArguments(i+1, input, args)
            i = j-1
          }
        }

        i += 1
      }

      // look for block functions that require us to have the op identifier
      // we don't do this in general because it's not necessary and doesn't play well with overloading
      val enclosingOp =
        if (args.exists(isBlockArg) || !tpeArgs.isEmpty) {
          // scan backwards until the lexically enclosing op identifier, which can be one of:
          //   static | direct | infix | internal (grp) (name, ....)
          //   static | direct | infix | internal (name) (....)
          //   impl (binding) (...)
          Some(scanBackToOp(i, input))
        }
        else None

      // write the formatted block
      val indent = " "*blockIndentation(start, input)
      val indentInterior = "  " + indent
      val nl = System.getProperty("line.separator")
      output ++= ("{" + nl).getBytes

      // need to add fix up block args and a prefix for positional args
      // we do this in 2 passes to handle nested args
      var argId = 1 // use a unique id, since blocks with different arguments need a different id
      for (a <- args) {
        try {
          if (isBlockArg(a)) {
            val a2 = a.slice(2,endOfBlockArgName(a))
            val i = a2.toInt
            val z: String = "arg"+argId // wtf? scala compiler error unless we break up z like this
            argMap += (a -> z)
          }
          else if (isBoundArg(a)) {
            val a2 = a.slice(2,endOfBoundArgName(a)).split(',')
            val i1 = a2(0).toInt
            val i2 = a2(1).toInt
            val z: String = "f_" + "__arg" + i1 + "_" + "__arg" + i2
            argMap += (a -> z)
          }
          else {
            val i = a.toInt
            val z: String = "arg"+argId
            argMap += (a -> z)
          }
          argId += 1
        }
        catch {
          case _:NumberFormatException =>
            if (isBlockArg(a)) {
              val a2 = a.slice(2,endOfBlockArgName(a))
              argMap += (a -> a2)
            }
            else if (isBoundArg(a)) {
              println("Parsing bound arg: " + a)

              val a2 = a.slice(2,endOfBoundArgName(a)).split(',')
              val i1 = a2(0)
              val i2 = a2(1).toInt
              val z: String = "f_" + i1 + "_" + "__arg" + i2
              argMap += (a -> z)
            }
            else {
              argMap += (a -> a)
            }
        }
      }

      for (a <- args) {
        def quoteArgName(t: String) = if (argMap(a).startsWith("arg")) t else "\""+t+"\""
        if (isBlockArg(a)) {
          val a2 = a.slice(2,endOfBlockArgName(a))
          output ++= (indentInterior + "val " + argMap(a) + " = quotedBlock("+quoteArgName(a2)+", "+enclosingOp.get+", List("+getCapturedArgs(a,argMap).mkString(",")+"))" + nl).getBytes
        }
        else if (!isBoundArg(a)) {
          output ++= (indentInterior + "val " + argMap(a) + " = quotedArg("+quoteArgName(a)+")" + nl).getBytes
        }
      }

      // process type args in a similar way, but because we don't allow higher-kinded args now, only one pass is required
      for (t <- tpeArgs) {
        val t2 = t.slice(2,endOfTypeArgName(t))
        val z: String = "tpe"+t2
        tpeArgMap += (t -> z)
        try {
          val i = t2.toInt
          output ++= (indentInterior + "val " + z + " = quotedTpe("+t2+", "+enclosingOp.get+")" + nl).getBytes
        }
        catch {
          case _:NumberFormatException =>
            output ++= (indentInterior + "val " + z + " = quotedTpe(\""+t2+"\", "+enclosingOp.get+")" + nl).getBytes
        }
      }

      output ++= (indentInterior + "s\"\"\"").getBytes

      // swallow comments
      val strBlockBuf = new StringBuilder()
      var j = start
      for (k <- 0 until startCommentIndices.length) {
        strBlockBuf ++= new String(input.slice(j, startCommentIndices(k)))
        if (endCommentIndices.length > k) j = endCommentIndices(k)
        if (input.slice(startCommentIndices(k),endCommentIndices(k)).contains('\n')) strBlockBuf += '\n'
      }
      strBlockBuf ++= new String(input.slice(j, i-1))
      var strBlock = strBlockBuf.toString

      // remap args inside the input block, outside-in
      for (a <- args.reverse) {
        if (argMap.contains(a)) {
          if (isBoundArg(a))
            strBlock = strBlock.replace("$"+a, argMap(a)) // don't include the $ for bound args
          else
            strBlock = strBlock.replace("$"+a, "$"+argMap(a))
        }
      }
      // remap tpe args
      for (t <- tpeArgs) {
        if (tpeArgMap.contains(t)) {
          strBlock = strBlock.replace("$"+t, "$"+tpeArgMap(t))
        }
      }
      // replace escaped $
      strBlock = strBlock.replace("\\$", "$")

      // swallow the indentation, since we'll re-indent inside Forge anyways
      strBlock = strBlock.replace(indent, "").trim

      output ++= strBlock.getBytes
      output ++= ("\"\"\"" + nl).getBytes
      output ++= (indent + "}").getBytes

      // return block end index
      i
    }

    def writeDocumentBlock(start: Int, input: Array[Byte], output: ArrayBuffer[Byte], startScope: CommentScope, expandComments: Boolean): Int = {
      var i = start
      var scope = startScope
      // Scan to end of comment block
      while (i < input.length && scope != NoComment) {
        val (newIndex, newScope) = scanComment(scope, i, input, false)
        if (newScope != scope) { i = newIndex; scope = newScope }
        else { i += 1 }
      }
      val end = i
      var trueEnd = end-2
      while (input(trueEnd) == '*' && trueEnd > start) { trueEnd -= 1 }  // Drop ending *s

      if (trueEnd <= start || !expandComments) {
        // e.g. /******/ markers
        output ++= input.slice(start, end)
      }
      else {
        if (startScope == DocumentBlock) output ++= ("doc(s\"\"\"").getBytes
        else if (startScope == SummaryBlock) output ++= ("summary(s\"\"\"").getBytes
        i = start
        var lineStart = start
        while (i < trueEnd) {
          lineStart = i
          // Drop the spaces and first * at the beginning of each line, if one exists
          while (i < trueEnd && input(i) != '\n') { i += 1 }
          var j = lineStart
          while (j < i && input(j) == ' ' || input(j) == '\t') { j += 1 }
          if (input(j) == '*') { j += 1 }
          //if (j > i) { j = lineStart }

          if (j < i) output ++= input.slice(j, i+1) // Add the line to the output
          i += 1
        }
        output ++= ("\"\"\")").getBytes
      }
      // return end index
      end
    }

    def expand(input: Array[Byte]): Option[Array[Byte]] = {
      val output = new ArrayBuffer[Byte]()

      var start = 0
      var feature: Feature = NoFeature
      var i = 0
      var isDSLFile = false    // Is this a DSL file?
      var isDSLScope = false   // Is this a DSL scope? (requiring preprocessor expansion)

      while (i < input.length) {
        val (nextFeature, nextIndex) = scanToFeature(start, input, isDSLScope)
        feature = nextFeature; i = nextIndex
        output ++= input.slice(start,i)

        start = feature match {
          case DocumentBlock => writeDocumentBlock(i+3, input, output, DocumentBlock, isDSLScope)
          case SummaryBlock  => writeDocumentBlock(i+4, input, output, SummaryBlock, isDSLScope)
          case CodeBlock =>
            isDSLFile = true
            isDSLScope = true
            writeFormattedBlock(i+2, input, output)

          case DSLAnnotation =>
            //println("Found feature at index ")
            isDSLFile = true
            isDSLScope = true
            i + DSLAnnotation.length

          case _ => i // Shouldn't happen in general
        }
      }

      if (isDSLFile) Some(output.toArray) else None
    }

    // --

    // preprocess entry

    if (preprocessorEnabled) {
      //System.out.println("Preprocessing file " + src.getName())

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

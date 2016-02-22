package ppl.dsl.forge
package dsls
package optima

import core.{ForgeApplication,ForgeApplicationRunner}

trait Stringables { this: OptiMADSL =>

  object TStringable extends TypeClassSignature {
    def name = "Stringable"
    def prefix = "_str"
    def wrapper = Some("strtype")
  }

  def importStringFormatting() {
    val T = tpePar("T")

    // override default string formatting (numericPrecision is a global defined in extern)
    // we use "" + $a instead of $a.toString to avoid an NPE when explicitly calling toString inside the REPL
    val fmt_str = direct (lookupGrp("FString")) ("format_numeric", T, T :: MString)

    // TODO: Add global precision
    //val f = "(\"% .\"+Global.numericPrecision+\"g\")" // can't escape quotes inside string interpolation scope
    impl (fmt_str) (codegen($cala, ${
      /*def numericStr[A](x: A) = {
        val s = \$f.format(x)
        val padPrefix = (Global.numericPrecision+6) - s.length
        if (padPrefix > 0) " "*padPrefix + s else s
      }*/
      /*if ($0.isInstanceOf[Double] || $0.isInstanceOf[Float]) numericStr($0) else*/ ("" + $0)
    }))

    impl (fmt_str) (codegen(cpp, "convert_to_string<" +  unquotes("remapWithRef("+opArgPrefix+"0.tp)") + " >(" + quotedArg(0) + ")"))

    internal (lookupGrp("FString")) ("padspace", Nil, MString :: MString) implements composite ${
      "  " + $0
      // if ($0.startsWith("-")) "  " + $0 else "   " + $0
    }
  }

  def importStringables() {
    importStringFormatting()

    val T = tpePar("T")

    val Stringable = tpeClass("Stringable", TStringable, T)

    // Stringable type class interface
    infix (Stringable) ("mkStr", T, T :: MString)

    // primitive implementations
    val DoubleStringable = tpeClassInst("StringableDouble", Nil, Stringable(MDouble))
    infix (DoubleStringable) ("mkStr", Nil, MDouble :: MString) implements composite ${ format_numeric($0) }

    val FloatStringable = tpeClassInst("StringableFloat", Nil, Stringable(MFloat))
    infix (FloatStringable) ("mkStr", Nil, MFloat :: MString) implements composite ${ format_numeric($0) }

    val IntStringable = tpeClassInst("StringableInt", Nil, Stringable(MInt))
    infix (IntStringable) ("mkStr", Nil, MInt :: MString) implements composite ${ format_numeric($0) }

    val LongStringable = tpeClassInst("StringableLong", Nil, Stringable(MLong))
    infix (LongStringable) ("mkStr", Nil, MLong :: MString) implements composite ${ format_numeric($0) }

    val BoolStringable = tpeClassInst("StringableBool", Nil, Stringable(MBoolean))
    infix (BoolStringable) ("mkStr", Nil, MBoolean :: MString) implements composite ${ format_numeric($0) }

    val StrStringable = tpeClassInst("StringableStr", Nil, Stringable(MString))
    infix (StrStringable) ("mkStr", Nil, MString :: MString) implements composite ${ $0 }

  }
}
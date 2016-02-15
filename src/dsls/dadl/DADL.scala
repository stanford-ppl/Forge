package ppl.dsl.forge
package dsls
package dadl

import core.{ForgeApplication,ForgeApplicationRunner}

object DADLDSLRunner extends ForgeApplicationRunner with DADLDSL

trait DADLDSL extends ForgeApplication
  with ArchOps
  with NodeOps {
//  with ArchOps {
//  with Modules {

  def dslName = "DADL"

//  override def addREPLOverride = false
  def testing() = {
    val DirectOps = grp("Direct")
    val myprintlnOp = direct (DirectOps) (
      name = "myprintln",
      tpePars = List(),
      MAny :: MUnit,
      effect = simple)

    impl (myprintlnOp) { codegen ($cala, ${println($0)}) }
    impl (myprintlnOp) { codegen (cpp, ${ std::cout << $0 << std::endl }) }

    /**
     * Example composite op
     */
		lift(DirectOps) (MInt)
		lift(DirectOps) (MBoolean)
		importStrings()
    importMisc()
    val IntOps = withTpe (MInt)
		IntOps {
			infix ("+") (MInt :: MInt) implements codegen ($cala, ${ $self + $1 })
		  infix ("<") (MInt :: MBoolean) implements codegen ($cala, ${ $self < $1 })
		}

    val T = tpePar("T")
    val printLT = direct (DirectOps) (
      name = "printLT",
      tpePars = List(T),
      MethodSignature(List(("arr", MArray(MInt)), ("maxnum", MInt)), MUnit),
      effect = simple)

    impl (printLT) { composite ${
      var i = 0
      while(i < $arr.length) {
        if ($arr(i) < $maxnum) println(i)
        i = i + 1
      }
    }}
  }

  def otherStuff() = {
//		importDADLArchOps()

//    importTuples()
//    importModules()
//    importDADLDirectOps()

//    val T = tpePar("T")
//    val rep = tpe("Rep", T, stage=compile)
//    primitiveTypes ::= rep
//    tpeAlias("Wire", rep)

//		val TypeOps = grp("TypeOps")
//		lift(TypeOps) (MArray)
/*    direct (TypeOps) ("println", List(), List(MAny) :: MUnit, effect = simple) implements codegen($cala, ${ println($0) }) */
  }

  def specification() = {
    importCore()
    importNodes()
    ()
	}
}

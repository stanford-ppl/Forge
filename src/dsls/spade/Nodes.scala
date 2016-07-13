package ppl.dsl.forge
package dsls
package spade
trait NodeOps {
  this: SpadeDSL =>

//  object HwNodeClass extends TypeClassSignature {
//    def name = "HwNode"
//    def prefix = "_hw"
//    def wrapper = None
//  }

  def importNodes() = {
    importALU()
  }

  def importALU() {
    // Import MyLink {
    val linkTpe = tpe("MyLink")
    val T1 = tpePar("T1")
    val T2 = tpePar("T2")

    val link_new = internal (linkTpe) ("link_new", List(T1, T2), (T1, T2) :: linkTpe, effect = simple)

    val link_create = internal (linkTpe) ("link_create", List(T1, T2), (T1, T2) :: linkTpe, effect = simple) implements composite ${
      val link = link_new($0, $1)
      link
    }

    // --- Scala Backend
    impl (link_new) (codegen($cala, ${ new MyLink { } }))

    // --- Dot Backend
    impl (link_new) (codegen(dot, ${
      $0 -> $1
		}))
    // Import MyLink }

    // Create a "HwNode" type class {

//    val HwNode = tpeClass("HwNode", HwNodeClass, (T1, T2))
//    infix (HwNode) ("->", (T1, T2), (T1, T2) :: linkTpe)
    // Create a "HwNode" type class }


    // Import ALU {
    val aluTpe = tpe("ALU")
    val T = tpePar("T")

    // --- Nodes
    val alu_new   = internal (aluTpe) ("alu_new", List(), Nil :: aluTpe, effect = simple)

    // --- Internals
    internal (aluTpe) ("alu_create", List(), Nil :: aluTpe, effect = simple) implements composite ${
      val alu = alu_new
      alu
    }

    // --- API
    static (aluTpe) ("apply", List(), Nil :: aluTpe) implements composite ${ alu_create }
    infix (aluTpe) ("->", T, (aluTpe, T) :: linkTpe) implements composite ${ link_create($0, $1)}
    infix (aluTpe) ("<->", T, (aluTpe, T) :: linkTpe) implements composite ${
      link_create($0, $1)
      link_create($1, $0)
    }

    // --- Scala Backend
    impl (alu_new) (codegen($cala, ${ new ALU { } }))

    // --- Dot Backend
    impl (alu_new) (codegen(dot, ${
      $sym [label= "\$sym" shape="square" style="filled" fillcolor=$regFillColor ]
		}))
    // Import ALU }

    // Import Switch {
    val switchTpe = tpe("Switch")

    // --- Nodes
    val switch_new   = internal (switchTpe) ("switch_new", List(), Nil :: switchTpe, effect = simple)

    // --- Internals
    internal (switchTpe) ("switch_create", List(), Nil :: switchTpe, effect = simple) implements composite ${
      val switch = switch_new
      switch
    }

    // --- API
    static (switchTpe) ("apply", List(), Nil :: switchTpe) implements composite ${ switch_create }
    infix (switchTpe) ("->", T, (switchTpe, T) :: linkTpe) implements composite ${ link_create($0, $1)}
    infix (switchTpe) ("<->", T, (switchTpe, T) :: linkTpe) implements composite ${
      link_create($0, $1)
      link_create($1, $0)
    }

    // --- Scala Backend
    impl (switch_new) (codegen($cala, ${ new Switch { } }))

    // --- Dot Backend
    impl (switch_new) (codegen(dot, ${
      $sym [label= "\$sym" shape="square" style="filled" fillcolor=$regFillColor ]
		}))
    // Import Switch }

  }
}

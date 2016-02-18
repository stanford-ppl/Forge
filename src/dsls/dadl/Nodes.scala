package ppl.dsl.forge
package dsls
package dadl
trait NodeOps {
	this: DADLDSL =>
	def importNodes() = {

    // Create a group for all nodes
    val NodeOps = grp("Node")

    // Define an 'ALU' type
    val aluTpe = tpe("ALU")
    data(aluTpe, ("_bits", MInt))

    // Define a 'Link' type
    val linkTpe = tpe("Link")
    data(linkTpe, ("_src", aluTpe), ("_dst", aluTpe))

    // Instantiating ALU
    val aluApply = static (aluTpe) (
      name = "apply",
      List(),
      MInt :: aluTpe,
      effect = simple)

    impl (aluApply) {  codegen ($cala, ${
      new ALU($0) { }
    })}

    impl (aluApply) {  codegen (dot, ${
      $sym [shape="color" style="filled" fillcolor="blue" color="white"]
    })}


    val linkApply = static (linkTpe) (
      name = "apply",
      List(),
      List(aluTpe, aluTpe) :: linkTpe,
      effect = simple)

    impl (linkApply) { codegen ($cala, ${
      new Link($0, $1) { }
    })}

    impl (linkApply) {  codegen (dot, ${
      $0 -> $1
    })}

    val aluOps = withTpe(aluTpe)
    aluOps {
//      infix("->") (aluTpe :: linkTpe, effect = simple) implements codegen ($cala, ${new Link($self, $1) { }} )
      val infixLinkOp = infix("->") (aluTpe :: linkTpe, effect = simple)
      impl (infixLinkOp) { codegen ($cala, ${
        new Link($self, $1) { }
      })}

      impl (infixLinkOp) { codegen (dot, ${
        $self -> $1
      })}
    }
	}
}

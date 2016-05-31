package ppl.dsl.forge
package dsls
package spade
trait NodeOps {
  this: SpadeDSL =>
  def importNodes() = {

    // Create a group for all nodes
    val NodeOps = grp("Node")

    // Define an 'ALU' type
    val aluTpe = tpe("ALU")
    data(aluTpe, ("_bits", MInt))

    // Define a 'LinkNode' type
    val linkTpe = tpe("LinkNode")
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
      new LinkNode($0, $1) { }
    })}

    impl (linkApply) {  codegen (dot, ${
      $0 -> $1
    })}

    val aluOps = withTpe(aluTpe)
    aluOps {
//      infix("->") (aluTpe :: linkTpe, effect = simple) implements codegen ($cala, ${new LinkNode($self, $1) { }} )
      val infixLinkNodeOp = infix("->") (aluTpe :: linkTpe, effect = simple)
      impl (infixLinkNodeOp) { codegen ($cala, ${
        new LinkNode($self, $1) { }
      })}

      impl (infixLinkNodeOp) { codegen (dot, ${
        $self -> $1
      })}
    }
  }
}

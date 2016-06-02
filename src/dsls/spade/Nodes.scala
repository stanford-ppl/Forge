package ppl.dsl.forge
package dsls
package spade
trait NodeOps {
  this: SpadeDSL =>
  def importNodes() = {

    // Create a group for all nodes
    val NodeOps = grp("Node")

    // Define a 'ALU' type
    val aluTpe = tpe("ALU")
//    data(aluTpe, ("_bits", MInt))

    // Define a 'ALU' type
    val switchTpe = tpe("Switch")

    // Define all combinations of the 'LinkNode' type
    val linkTpe_alu2alu = tpe("Link_alu2alu")
    data(linkTpe_alu2alu, ("_src", aluTpe), ("_dst", aluTpe))

    val linkTpe_alu2switch = tpe("Link_alu2switch")
    data(linkTpe_alu2switch, ("_src", aluTpe), ("_dst", switchTpe))

    val linkTpe_switch2alu = tpe("Link_switch2alu")
    data(linkTpe_switch2alu, ("_src", switchTpe), ("_dst", aluTpe))

    val linkTpe_switch2switch = tpe("Link_switch2switch")
    data(linkTpe_switch2switch, ("_src", switchTpe), ("_dst", switchTpe))


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
      $sym [shape="square" style="filled" fillcolor="blue" color="white"]
    })}


    // Instantiating Switch
    val switchApply = static (switchTpe) (
      name = "apply",
      List(),
      MUnit :: switchTpe,
      effect = simple)

    impl (switchApply) {  codegen ($cala, ${
      new Switch() { }
    })}

    impl (switchApply) {  codegen (dot, ${
      $sym [shape="circle" style="filled" fillcolor="yellow" color="white"]
    })}

    val linkApply_alu2alu = static (linkTpe_alu2alu) (
      name = "apply",
      List(),
      List(aluTpe, aluTpe) :: linkTpe_alu2alu,
      effect = simple)
    impl (linkApply_alu2alu) { codegen ($cala, ${
      new LinkNode($0, $1) { }
    })}
    impl (linkApply_alu2alu) {  codegen (dot, ${
      $0 -> $1
    })}

    val linkApply_alu2switch = static (linkTpe_alu2switch) (
      name = "apply",
      List(),
      List(aluTpe, switchTpe) :: linkTpe_alu2switch,
      effect = simple)
    impl (linkApply_alu2switch) { codegen ($cala, ${
      new LinkNode($0, $1) { }
    })}
    impl (linkApply_alu2switch) {  codegen (dot, ${
      $0 -> $1
    })}

    val linkApply_switch2alu = static (linkTpe_switch2alu) (
      name = "apply",
      List(),
      List(switchTpe, aluTpe) :: linkTpe_switch2alu,
      effect = simple)
    impl (linkApply_switch2alu) { codegen ($cala, ${
      new LinkNode($0, $1) { }
    })}
    impl (linkApply_switch2alu) {  codegen (dot, ${
      $0 -> $1
    })}

    val linkApply_switch2switch = static (linkTpe_switch2switch) (
      name = "apply",
      List(),
      List(switchTpe, switchTpe) :: linkTpe_switch2switch,
      effect = simple)
    impl (linkApply_switch2switch) { codegen ($cala, ${
      new LinkNode($0, $1) { }
    })}
    impl (linkApply_switch2switch) {  codegen (dot, ${
      $0 -> $1
    })}


//    val aluOps = withTpe(aluTpe)
//    aluOps {
////      infix("->") (aluTpe :: linkTpe, effect = simple) implements codegen ($cala, ${new LinkNode($self, $1) { }} )
//      val infixLinkNodeOp = infix("->") (aluTpe :: linkTpe, effect = simple)
//      impl (infixLinkNodeOp) { codegen ($cala, ${
//        new LinkNode($self, $1) { }
//      })}
//
//      impl (infixLinkNodeOp) { codegen (dot, ${
//        $self -> $1
//      })}
//    }
  }
}

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
    static (aluTpe) (
      name = "apply",
      List(),
      MInt :: aluTpe,
      effect = simple) implements codegen (
        $cala, ${ new ALU($0) { }})


    static (linkTpe) (
      name = "apply",
      List(),
      List(aluTpe, aluTpe) :: linkTpe,
      effect = simple) implements codegen (
        $cala, ${ new Link($0, $1) { }})


    val aluOps = withTpe(aluTpe)
    aluOps {
//      infix("->") (aluTpe :: linkTpe, effect = simple) implements codegen ($cala, ${new Link($self, $1) { }} )
      infix("->") (aluTpe :: linkTpe, effect = simple) implements codegen ($cala, ${new Link($self, $1) { } })
    }
	}
}

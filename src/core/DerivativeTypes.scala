package ppl.dsl.forge
package core

/**
 * These are types that need to be mixed in to the IR.
 *
 * The downside is that if a DSL needs to use these types, they must explicitly import them.
 */

trait DerivativeTypes {
  this: Forge =>

  abstract class ForgeCollectionType {
    val tpeArg: Rep[DSLType]
    val alloc: Rep[DSLOp]
    val size: Rep[DSLOp]
    val apply: Rep[DSLOp]
    val update: Rep[DSLOp]
  }
}

trait DerivativeTypesExp {
  this: ForgeExp =>

  abstract class DSLOps {
    val grp: Rep[DSLGroup]
    lazy val name = grp.name + "Ops"
    var ops = List[Rep[DSLOp]]()
    def targets: List[CodeGenerator] = generators
  }
}

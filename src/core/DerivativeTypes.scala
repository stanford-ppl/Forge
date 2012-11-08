package ppl.dsl.forge
package core

/**
 * These are types that need to be mixed in to the IR.
 * 
 * The downside is that if a DSL needs to use these types, they must explicitly import them.
 */

trait DerivativeTypes {
  this: Forge =>
  
  abstract class Ops {
    def name: String
    def exp = name + "Exp"
    def opt = exp
    def targets: List[CodeGenerator] = generators
    def lift: Option[String] = None
  }
  
  abstract class LMSOps extends Ops
  abstract class DSLOps extends Ops {
    var ops = List[Rep[DSLOp]]()
  }     
}
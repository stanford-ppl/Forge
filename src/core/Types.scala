package ppl.dsl.forge

/*
 * Types used for declaring new objects inside a Forge specification
 */
 abstract class TypeClass {
   def name: String
   def prefix: String
 }
abstract class DSLGroup 
abstract class DSLType extends DSLGroup
abstract class TypePar extends DSLType
abstract class DSLData
abstract class DSLStruct
abstract class DSLOp
abstract class LiftStm
abstract class RewriteRule
abstract class CodeGenRule

/**
 * Types representing built-in constructs
 */
abstract class StageTag
abstract class MethodType
abstract class EffectType
abstract class OpType
abstract class DeliteType
abstract class CodeGenerator {
  def name: String
}
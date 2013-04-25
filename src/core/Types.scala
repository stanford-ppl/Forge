package ppl.dsl.forge

/*
 * Types used for declaring new objects inside a Forge specification
 */
abstract class TypeClass {
  def name: String
  def prefix: String
  def wrapper: Option[String]
}
abstract class DSLGroup 
abstract class DSLType extends DSLGroup
abstract class TypePar extends DSLType
abstract class TypeAlias extends DSLType
abstract class DSLArg
abstract class DSLData
abstract class DSLStruct
abstract class DSLOp
abstract class LiftStm
//abstract class RewriteRule
abstract class Rule // other names to consider, DSLOp, DSLRule
abstract class DeliteRule extends Rule
abstract class CodeGenRule extends Rule

/**
 * Types representing built-in constructs
 */
abstract class StageTag
abstract class MethodType
abstract class EffectType
abstract class AliasHint
abstract class Frequency
//abstract class OpType
abstract class CodeGenerator {
  def name: String
}
abstract class DeliteParallelStrategy extends DSLType

package ppl.dsl.forge

/*
 * Types used for declaring new objects inside a Forge specification
 */
abstract class TypeClassSignature {
  def name: String
  def prefix: String
  def wrapper: Option[String]
}
abstract class DSLGroup
abstract class DSLType extends DSLGroup
abstract class DSLTypeClass extends DSLType
abstract class DSLTypeClassInst extends DSLType
abstract class TypePar extends DSLType
abstract class TypeAlias extends DSLType
abstract class DSLArg
abstract class DSLData
abstract class DSLStruct
abstract class DSLOp
abstract class DSLIdentifier
abstract class LiftStm
abstract class RewriteRule
abstract class CodeGenRule

/**
 * Types representing built-in constructs
 */
abstract class StageTag
abstract class MethodType
abstract class MethodSignatureType
abstract class EffectType
abstract class AliasHint
abstract class Frequency
abstract class OpType
abstract class CodeGenerator {
  def name: String
}
abstract class DeliteParallelStrategy extends DSLType

package ppl.dsl.meta

/*
 * Types used for declaring new objects inside a MetaDSL specification
 */
 abstract class TypeClass {
   def name: String
   def prefix: String
 }
abstract class DSLType 
abstract class TypeArg extends DSLType
abstract class DSLData
abstract class DSLStruct
abstract class DSLOp
abstract class RewriteRule
abstract class CodeGenRule

/**
 * Types representing built-in constructs
 */
abstract class MethodType
abstract class EffectType
abstract class OpType
abstract class DeliteType
abstract class CodeGenerator {
  def name: String
}
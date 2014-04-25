package ppl.dsl.forge
package core

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import scala.tools.nsc.io._
import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericFatCodegen, GenericCodegen}

import templates.shared.ForgeCodeGenShared
import templates.library.ForgeCodeGenInterpreter
import templates.compiler.ForgeCodeGenDelite
import templates.ident.ForgeCodeGenIdent

trait ForgeApplicationRunner extends ForgeApplication with ForgeExp {
  val dsl = dslName.filterNot(_ == ' ').capitalize
  val build = "build" // TODO: make configurable

  final def main(args: Array[String]) {
    info("DSL Being Staged:[" + this.getClass.getName + "] (" + dsl + ")")

    // -- stage forge primitives
    extern(grp("ForgeArray"))
    extern(grp("ForgeArrayBuffer"))
    extern(grp("ForgeHashMap"))
    extern(grp("Var"), withLift = true)
    extern(grp("Lambda"))
    extern(grp("Record"))
    extern(grp("InputOutput"))
    extern(grp("Profiling"))
    extern(grp("Reppable"))
    extern(grp("Tests"))
    extern(grp("Asserts"), targets = List($cala))

    // in order to use arrays and arraybuffers as parallel collections, we must be able to access their parallel collection methods.
    // here we construct a transient group (not saved in the Ops IR) to hold this mapping. unfortunately, we now have a dependence between the
    // extern code and the specification. it is not exposed to DSL authors, but this mapping must be updated if the methods in the extern code change.

    def transient(name: String, method: MethodType, tpePars: List[Rep[TypePar]], args: List[Rep[DSLType]], retTpe: Rep[DSLType], effect: EffectType): Rep[DSLOp] = {
      Op(grp("transient"), name, method, tpePars, listToArgs(args), Nil, arg("__pos",MSourceContext), retTpe, effect, nohint)
    }

    val T = tpePar("T")

    val aAlloc = transient("array_raw_alloc", compilerMethod, T, (MArray(T),MInt), MArray(T), mutable)
    val aLength = transient("array_length", directMethod, T, MArray(T), MInt, pure)
    val aApply = transient("array_apply", directMethod, T, (MArray(T),MInt), T, pure)
    val aUpdate = transient("array_update", compilerMethod, T, (MArray(T),MInt,T), MUnit, write(0))
    parallelize (MArray) as ParallelCollection(T, aAlloc, aLength, aApply, aUpdate)

    val abAlloc = transient("array_buffer_raw_alloc", compilerMethod, T, (MArrayBuffer(T),MInt), MArrayBuffer(T), mutable)
    val abLength = transient("array_buffer_length", compilerMethod, T, MArrayBuffer(T), MInt, pure)
    val abApply = transient("array_buffer_apply", compilerMethod, T, (MArrayBuffer(T),MInt), T, pure)
    val abUpdate = transient("array_buffer_update", compilerMethod, T, (MArrayBuffer(T),MInt,T), MUnit, write(0))
    val abSetLength = transient("array_buffer_set_length", compilerMethod, T, (MArrayBuffer(T),MInt), MUnit, write(0))
    val abAppendable = transient("array_buffer_appendable", compilerMethod, T, MArrayBuffer(T), MBoolean, pure)
    val abAppend = transient("array_buffer_dcappend", compilerMethod, T, (MArrayBuffer(T),MInt,T), MUnit, write(0))
    val abCopy = transient("array_buffer_copy", compilerMethod, T, (MArrayBuffer(T),MInt,MArrayBuffer(T),MInt,MInt), MUnit, write(2))
    parallelize (MArrayBuffer) as ParallelCollectionBuffer(T, abAlloc, abLength, abApply, abUpdate, abSetLength, abAppendable, abAppend, abCopy)

    // -- stage the specification to build the Forge IR
    // this has the side effect of populating all of the internal Forge collections
    val y = specification()

    // -- fast compile mode
    if (Config.fastCompile) {
      flattenIR()
    }

    // -- run code generators

    Directory(Path(build)).deleteRecursively()

    // shared
    val sharedCodegen = new ForgeCodeGenShared {
      val IR: ForgeApplicationRunner.this.type = ForgeApplicationRunner.this
    }

    // library (interpreter)
    val libraryCodegen = new ForgeCodeGenInterpreter {
      val IR: ForgeApplicationRunner.this.type = ForgeApplicationRunner.this
    }

    // delite
    val deliteCodegen = new ForgeCodeGenDelite {
      val IR: ForgeApplicationRunner.this.type = ForgeApplicationRunner.this
    }

    // identity
    val identCodegen = new ForgeCodeGenIdent {
      val IR: ForgeApplicationRunner.this.type = ForgeApplicationRunner.this
    }

    var codeGenerators: List[ForgeCodeGenBackend{val IR: ForgeApplicationRunner.this.type; val buildDir: String}] = List(sharedCodegen)
    if (Config.genLib) codeGenerators :+= libraryCodegen
    if (Config.genDelite) codeGenerators :+= deliteCodegen
    if (Config.genIdent) codeGenerators :+= identCodegen

    for (c <- codeGenerators) {
      c.emitDSLImplementation()
    }

    info("DSL generation complete. Please run publish and compile the generated files against Delite to check for errors.")
  }
}

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
    extern(grp("Var"), withLift = true)
    extern(grp("Record"))
    extern(grp("InputOutput"))
    extern(grp("Profiling"))
    extern(grp("Reppable"))
    extern(grp("Tests"))

    // -- stage the specification to build the Forge IR
    // this has the side effect of populating all of the internal Forge collections
    val y = specification()

    // -- run sanity checkers
    // TODO: implement these!
    //  1) all ops that are declared as delite ops have delite collection input/outputs
    // check(y)

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

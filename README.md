Forge
=====

A prototype meta DSL that generates Delite DSL implementations from a specification-like program.

Structure
=========

A Forge DSL consists of a Forge specification (see src/examples) and optional applications (apps)
and external code (extern). Forge generates DSL implementations from the specification, which are
combined with the apps and external components to produce a complete DSL with examples. The publish
script (bin/publish) automates this process for apps and external components located in apps/{dslName}
and extern/{dslName}, by convention.

Alternatively, a user can simply copy the results of the Forge generation to another location to 
combine it with existing components.

Installation
============

Forge currently works with and requires Lightweight Modular Staging 
(https://github.com/TiarkRompf/virtualization-lms-core) branch `delite-develop` and 
Delite (https://github.com/stanford-ppl/Delite) branch `develop`.

1. Download and build both of these repositories on the corresponding branch. Make sure that
you run `sbt publish-local` on each of them (in Delite, only the `runtime` and `framework` projects
need to be published). Set the `DELITE_HOME` environment variable to the root of the Delite repository
and the `FORGE_HOME` environment variable to the root of the Forge repository.


2. Compile Forge using `sbt compile` from the root of the Forge repository. The SimpleVectorDSL
example can then be run using:

        bin/update ppl.dsl.forge.examples.SimpleVectorDSLRunner SimpleVector
        cd published/simplevector/

3. Run the sample application (apps/simplevector/src/HelloWorld.scala)

    Interpreter (library mode):

        bin/delitec HelloSimpleInterpreter

    Compiler (Delite mode):

        bin/delitec HelloSimpleCompiler
        bin/delite HelloSimpleCompiler


Using an existing Forge DSL
===========================

To use a Forge DSL that comes packaged with the Forge repository (e.g. OptiML, OptiQL), you
can generate the DSL and then cd into the `published` folder (or move it to another location)
and use it as normal, or you can put your applications in the `<FORGE_HOME>/apps/<DSL>/src/` folder,
which is automatically copied to the generated DSL directory during publishing.

To generate a DSL and move it to an external location (e.g. OptiML):

        bin/update ppl.dsl.forge.dsls.optiml.OptiMLDSLRunner OptiML
        mv published/OptiML <destination>
     
The destination will now contain the generated DSL, and you can add applications directly
in the `apps` folder, and compile and run them using
        
        sbt compile
        bin/delitec <fully.qualified.appObject>
        bin/delite <fully.qualified.appObject>

You can also enter interactive mode using

        sbt "; console"


To run a DSL application located in `<FORGE_HOME>/apps/<DSL>/src/`, (e.g. Logistic Regression):

* Interpreter: 

        bin/update -r "LogRegInterpreter <args>" ppl.dsl.forge.dsls.optiml.OptiMLDSLRunner OptiML

* Compiler:

        bin/update -d -r "LogRegCompiler <args>" ppl.dsl.forge.dsls.optiml.OptiMLDSLRunner OptiML

* Interactive:
        
        bin/update -i ppl.dsl.forge.dsls.optiml.OptiMLDSLRunner OptiML

If the DSL has already been generated, you can supply the `-s 3` parameter to `update` to skip
immediately to the publishing step.

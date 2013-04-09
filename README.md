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
(https://github.com/TiarkRompf/virtualization-lms-core) branch `wip-delite-develop` and 
Delite (https://github.com/stanford-ppl/Delite) branch `wip-develop`.

1. Download and build both of these repositories on the corresponding branch. Make sure that
you run `sbt publish-local` on each of them and also set the `DELITE_HOME` environment variable
to the root of the Delite repository.


2. Compile Forge using `sbt compile` from the root of the Forge repository. The SimpleVectorDSL
example can then be run using:

`bin/forge ppl.dsl.forge.examples.SimpleVectorDSLRunner`
`bin/publish simplevector`
`cd published/simplevector/`
`sbt compile`

3. Run the sample application (apps/simplevector/src/HelloWorld.scala)

Interpreter (library mode):

`bin/delitec HelloSimpleInterpreter`


Compiler (Delite mode):

`bin/delitec HelloSimpleCompiler`
`bin/delite HelloSimpleCompiler`



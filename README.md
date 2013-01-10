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

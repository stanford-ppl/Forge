# Delite Hardware Definition Language (DHDL)

This quickstart guide will help you clone, build and use DHDL.

## Cloning and installation
~~~~
  git clone https://raghup17@bitbucket.org/raghup17/dhdl.git
  export DHDL_HOME=`pwd`/dhdl
  cd dhdl
  make
~~~~

## Directory structure
~~~~
  dhdl
  ├── apps........................ DHDL applications. New applications must be added to this folder
  │   ├── src
  │   │   ├─ benchmarks........... Application benchmarks
  │   │   └─ characterization..... Microbenchmarks to characterize nodes on a given target
  ├── bin......................... DHDL helper script to build and run applications
  ├── docs
  │   ├── api..................... Scaladoc documentation generated from comments in code
  │   └── manual.................. [WIP] Tex sources for user guide
  ├── lib_managed
  ├── project..................... DHDL project and build definitions
  ├── out......................... Directory in which all generated files are created after compilation.
  ├── src
  │   ├── analysis................ All analyses passes defined over DHDL IR
  │   ├── codegen
  │   │   ├── dot................. Dot code generation package
  │   │   ├── scala............... Scala code generation package
  │   │   └── maxj................ MaxJ code generation package
  │   ├── graph................... DHDL node definition and global graph data
  │   │   └─ traversal............ DHDL IR graph traversal interface
  │   ├── transform............... All transformation passes defined over DHDL IR
  │   └── main.................... Application entry and definition of traits to be mixed with in with every DHDL application
  └── test
~~~~

## Quick start
All the commands described below are to be executed from DHDL's home directory:
~~~~
  cd $DHDL_HOME
~~~~
* To build DHDL and all applications:
~~~~
  make
~~~~

* To build and run a particular application (e.g. `DotProduct`):
~~~~
  bin/dhdl DotProduct 96 1 4 true # <tile size> <outer parallelism> <inner parallelism> <metapipeline? true/false>
                                  # Files generated in $DHDL_HOME/out folder
~~~~

* The dataflow graph of a DHDL program is output as a 'dot' file. To view the graph (requires 'dot'
  to be installed on your machine):
~~~
  cd out/dot
  make
  # Open 'outfile.pdf' in your favorite PDF viewer
~~~

* To build documentation:
~~~~
  make doc
~~~~

### Contact ###

* Raghu Prabhakar (raghup17@stanford.edu)

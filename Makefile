.PHONY: apps

all: build ctags

build:
	cd ${HYPER_HOME} && sbt "; project forge; compile" && cd ${FORGE_HOME}

ctags:
	cd ${HYPER_HOME} && sbt "; project forge; gen-ctags" && cd ${FORGE_HOME}

dhdl: build
	cd ${HYPER_HOME} && ${FORGE_HOME}/bin/update ppl.dsl.forge.dsls.dhdl.DHDLDSLRunner DHDL && cd ${FORGE_HOME}
	ln -s ${HYPER_HOME}/published/DHDL DHDL.symlink

apps:
	cp ${FORGE_HOME}/apps/DHDL/src/* ${HYPER_HOME}/published/DHDL/apps/src/
	cd ${HYPER_HOME}/published/DHDL && sbt compile

dhdl_clean:
	rm -f DHDL.symlink
	rm -rf ${HYPER_HOME}/published/DHDL

spade: build
	cd ${HYPER_HOME} && ${FORGE_HOME}/bin/update ppl.dsl.forge.dsls.spade.SpadeDSLRunner Spade && cd ${FORGE_HOME}
	ln -s ${HYPER_HOME}/published/Spade Spade.symlink

spade_clean:
	rm -f Spade.symlink
	rm -rf ${HYPER_HOME}/published/Spade

optiml: build
	cd ${HYPER_HOME} && ${FORGE_HOME}/bin/update ppl.dsl.forge.dsls.optiml.OptiMLDSLRunner OptiML && cd ${FORGE_HOME}
	ln -s ${HYPER_HOME}/published/OptiML OptiML.symlink

optiml_clean:
	rm -f OptiML.symlink
	rm -rf ${HYPER_HOME}/published/OptiML


clean:
	cd ${HYPER_HOME} && sbt "; project forge; clean" && cd ${FORGE_HOME}

distclean: clean dhdl_clean spade_clean

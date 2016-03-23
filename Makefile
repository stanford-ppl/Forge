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

dadl: build
	cd ${HYPER_HOME} && ${FORGE_HOME}/bin/update ppl.dsl.forge.dsls.dadl.DADLDSLRunner DADL && cd ${FORGE_HOME}
	ln -s ${HYPER_HOME}/published/DADL DADL.symlink

dadl_clean:
	rm -f DADL.symlink
	rm -rf ${HYPER_HOME}/published/DADL


clean:
	cd ${HYPER_HOME} && sbt "; project forge; clean" && cd ${FORGE_HOME}

distclean: clean dhdl_clean dadl_clean

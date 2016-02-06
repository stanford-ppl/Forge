
all: build ctags

build:
	cd ${HYPER_HOME} && sbt "; project forge; compile" && cd ${FORGE_HOME}

ctags:
	cd ${HYPER_HOME} && sbt "; project forge; gen-ctags" && cd ${FORGE_HOME}

dhdl: build
	cd ${HYPER_HOME} && ${FORGE_HOME}/bin/update ppl.dsl.forge.dsls.dhdl.DHDLDSLRunner DHDL && cd ${FORGE_HOME}
	ln -s ${HYPER_HOME}/published/DHDL DHDL

dhdl_clean:
	rm -f DHDL
	rm -rf ${HYPER_HOME}/published/DHDL

clean:
	cd ${HYPER_HOME} && sbt "; project forge; clean" && cd ${FORGE_HOME}

distclean: clean dhdl_clean

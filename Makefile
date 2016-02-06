
all: build ctags

build:
	cd .. && sbt "; project forge; compile" && cd forge

ctags:
	cd .. && sbt "; project forge; gen-ctags" && cd forge

clean:
	cd .. && sbt "; project forge; clean" && cd forge

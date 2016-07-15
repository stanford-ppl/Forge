SRC_EXT=dot
OBJ_EXT=pdf
SOURCES=$(wildcard *.dot)
OBJECTS=$(SOURCES:.dot=.pdf)

all: $(SOURCES) $(OBJECTS)

%.pdf : %.dot
	dot -Tpdf $< > $@

clean:
	\rm -f $(OBJECTS)

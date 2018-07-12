.PHONY: build doc examples install uninstall test clean

build:
	dune build @install
	rm -f lib && ln -sf _build/install/default/lib lib

doc:
	dune build @doc
	rm -f doc && ln -sf _build/default/_doc/_html doc

examples:
	dune build @examples
	mkdir -p bin \
	    && cd bin \
	    && find ../_build/default/examples/ -name '*.exe' -exec ln -sf '{}' . ';'

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean
	rm -f lib doc
	rm -rf bin

.PHONY: build doc examples install uninstall test clean

build:
	jbuilder build @install
	rm -f lib && ln -sf _build/install/default/lib lib

doc:
	jbuilder build @doc
	rm -f doc && ln -sf _build/default/_doc/_html doc

examples:
	jbuilder build @examples
	mkdir -p bin \
	    && cd bin \
	    && find ../_build/default/examples/ -name '*.exe' -exec ln -sf '{}' . ';'

install:
	jbuilder install

uninstall:
	jbuilder uninstall

test:
	jbuilder runtest

clean:
	jbuilder clean
	rm -f lib doc
	rm -rf bin

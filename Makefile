.PHONY: build doc install uninstall test clean

build:
	jbuilder build @install
	rm -f lib && ln -sf _build/install/default/lib lib

doc:
	jbuilder build @doc
	rm -f doc && ln -sf _build/default/_doc/_html

install:
	jbuilder install

uninstall:
	jbuilder uninstall

test:
	jbuilder runtest

clean:
	jbuilder clean
	rm -f bin lib doc

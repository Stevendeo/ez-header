.PHONY: all build clean

all: build

_build:
	mkdir _build

_build/src/ez_header.ml: src/ez_header.ml | _build
	cp $< $@

build: _build/src/ez_header.ml
	ocamlc -o ez-header $<

clean:
	rm -rf _build

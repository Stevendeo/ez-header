.PHONY: all build clean

all: build

_build:
	mkdir _build
	cp -r src _build/src

build: _build
	ocamlc -o ez-header _build/src/ez_header.ml

clean:
	rm -rf _build

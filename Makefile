all: build

build:
	dune build

test: build
	dune exec src/afind.exe

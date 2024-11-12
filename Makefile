all: build

build:
	@dune build

test:
	$(MAKE) -C test test

.PHONY: test

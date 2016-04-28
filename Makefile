.PHONY: all

all: reliquary stdlib

reliquary: src/*.hs
	cabal build
	cp dist/build/reliquary/reliquary .

stdlib: data/stdlib.asm
	nasm -felf64 data/stdlib.asm -o data/stdlib.o

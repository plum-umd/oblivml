EXE_NAME := oblivml

OCAMLBUILD_FLAGS := -use-ocamlfind
OCAMLBUILD       := ocamlbuild $(OCAMLBUILD_FLAGS)

all: native byte

native:
	$(OCAMLBUILD) '$(EXE_NAME).native'

byte:
	$(OCAMLBUILD) '$(EXE_NAME).byte'

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean native byte

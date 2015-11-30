# Config for OCamlMakefile
RESULT := lolc
SOURCES := ast.ml stringify.ml main.ml
PRE_TARGETS :=
LIBS := str

# Set debugging flag to enable exception backtraces for OCAMLRUNPARAM=b
OCAMLFLAGS := -g

OCAMLYACC := menhir
YFLAGS := --infer --explain

.PHONY: all

all: native-code

include OCamlMakefile

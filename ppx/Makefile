ROOTDIR = ..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

directories := $(addprefix $(ROOTDIR)/,utils bytecomp parsing stdlib \
                                       compilerlibs typing)

include_directories := $(addprefix -I , $(directories))

flags := -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-40-41-42-44-45-48 -warn-error A

ocamlc := $(ROOTDIR)/runtime/ocamlrun $(ROOTDIR)/ocamlc $(flags)

libraries := ocamlbytecomp.cma  ocamlcommon.cma

.PHONY: build
build: printexc_ppx

printexc_ppx: printexc_ppx.ml
	$(ocamlc) -o $@ $(libraries) printexc_ppx.ml

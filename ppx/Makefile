ROOTROOT = ../..
ROOTDIR = ..

include $(ROOTDIR)/Makefile.config
include $(ROOTDIR)/Makefile.common

ocamlc := $(ROOTROOT)/runtime/ocamlrun $(ROOTROOT)/ocamlc

.PHONY: build
build:	
	cd printexc_ppx; $(ocamlc) -c printexc_ppx.ml
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sébastien Hinderer, projet Gallium, INRIA Paris             *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

include ../config/Makefile

CC := $(BYTECC)

CFLAGS = -I ../byterun -I ../stdlib
run := run_$(UNIX_OR_WIN32)

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from athis)
sources := \
  $(run).c \
  run_stubs.c \
  testlib.mli testlib.ml \
  run_command.mli run_command.ml \
  filetype.mli filetype.ml \
  filecompare.mli filecompare.ml \
  backends.mli backends.ml \
  variables.mli variables.ml \
  environments.mli environments.ml \
  builtin_variables.mli builtin_variables.ml \
  builtin_modifiers.mli builtin_modifiers.ml \
  actions.mli actions.ml \
  builtin_actions.mli builtin_actions.ml \
  tests.mli tests.ml \
  builtin_tests.mli builtin_tests.ml \
  tsl_ast.mli tsl_ast.ml \
  tsl_parser.mly \
  tsl_lexer.mll \
  tsl_semantics.mli tsl_semantics.ml \
  options.mli options.ml \
  main.ml

# List of .ml files used for ocamldep and to get the list of modules

ml_files := \
  $(filter %.ml,$(subst .mll,.ml,$(subst .mly,.ml,$(sources))))

cmo_files := $(ml_files:.ml=.cmo)

cmx_files := $(ml_files:.ml=.cmx)

# List of .mli files for ocamldep
mli_files := \
  $(filter %.mli,$(subst .mly,.ml,$(sources)))

cmi_files := $(mli_files:.mli=.cmi)

c_files := $(filter %.c, $(sources))

o_files := $(c_files:.c=.$(O))

lexers := $(filter %.mll,$(sources))

parsers := $(filter %.mly,$(sources))

generated := \
  $(lexers:.mll=.ml) \
  $(parsers:.mly=.mli) $(parsers:.mly=.ml)

bcmodules := $(o_files) $(cmo_files)

ncmodules := $(o_files) $(cmx_files)

directories = ../utils ../parsing ../stdlib ../compilerlibs

include_directories = $(addprefix -I , $(directories))

flags = -g -nostdlib $(include_directories) \
  -strict-sequence -safe-string -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error A

ocamlc := ../byterun/ocamlrun ../ocamlc $(flags)

ocamlopt := ../byterun/ocamlrun ../ocamlopt $(flags)

ocamldep := ../byterun/ocamlrun ../tools/ocamldep -slash

ocamllex := ../byterun/ocamlrun ../lex/ocamllex

ocamlyacc := ../yacc/ocamlyacc

ocamltest.byte$(EXE): $(bcmodules)
	$(ocamlc) -custom ocamlcommon.cma -o $@ $^

%.cmo: %.ml
	$(ocamlc) -c $<

ocamltest.opt$(EXE): $(ncmodules)
	$(ocamlopt) ocamlcommon.cmxa -o $@ $^

%.cmx: %.ml
	$(ocamlopt) -c $<

%.cmi: %.mli
	$(ocamlc) -c $<

%.ml %.mli: %.mly
	$(ocamlyacc) $<

%.ml: %.mll
	$(ocamllex) -q $<

%.$(O): %.c
	$(CC) $(CFLAGS) $(BYTECCCOMPOPTS) -c $<

.PHONY: clean
clean:
	rm -rf ocamltest.byte$(EXE) ocamltest.opt$(EXE)
	rm -rf $(o_files)
	rm -rf $(cmi_files)
	rm -rf $(cmo_files)
	rm -rf $(cmx_files)
	rm -rf $(generated)

ifneq "$(TOOLCHAIN)" "msvc"
.PHONY: depend
depend: $(generated)
	$(CC) -MM $(CFLAGS) $(BYTECCCOMPOPTS) $(c_files) \
	  | sed -e 's/\.o/.$$(O)/' > .depend
	$(ocamldep) $(mli_files) $(ml_files) >> .depend
endif

include .depend

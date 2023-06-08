#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sebastien Hinderer, projet Gallium, INRIA Paris             *
#*                                                                        *
#*   Copyright 2016 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The Makefile for ocamltest

ROOTDIR = ..

include $(ROOTDIR)/Makefile.common
include $(ROOTDIR)/Makefile.best_binaries

# List of source files from which ocamltest is compiled
# (all the different sorts of files are derived from this)

# ocamltest has two components: its core and the OCaml "plugin"
# which is actually built into the tool but clearly separated from its core

core := \
  run_$(UNIX_OR_WIN32).c run_stubs.c \
  $(call OCAML_CONFIGURED_MODULE, ocamltest_config) \
  $(call OCAML_MODULE, ocamltest_unix) \
  $(call OCAML_MODULE, ocamltest_stdlib) \
  $(call OCAML_MODULE, run_command) \
  $(call OCAML_MODULE, filecompare) \
  $(call OCAML_MODULE, variables) \
  $(call OCAML_MODULE, environments) \
  $(call OCAML_MODULE, result) \
  $(call OCAML_MODULE, actions) \
  $(call OCAML_MODULE, tests) \
  $(call OCAML_MODULE, strace) \
  $(call OCAML_MODULE, tsl_ast) \
  $(call OCAMLYACC_PARSER, tsl_parser) \
  $(call OCAMLLEX_LEXER, tsl_lexer) \
  $(call OCAML_MODULE, modifier_parser) \
  $(call OCAML_MODULE, tsl_semantics) \
  $(call OCAML_MODULE, builtin_variables) \
  $(call OCAML_MODULE, actions_helpers) \
  $(call OCAML_MODULE, builtin_actions) \
  $(call OCAML_MODULE, translate)

ocaml_plugin := \
  $(call OCAML_MODULE, ocaml_backends) \
  $(call OCAML_MODULE, ocaml_filetypes) \
  $(call OCAML_MODULE, ocaml_variables) \
  $(call OCAML_MODULE, ocaml_modifiers) \
  $(call OCAML_MODULE, ocaml_directories) \
  $(call OCAML_MODULE, ocaml_files) \
  $(call OCAML_MODULE, ocaml_flags) \
  $(call OCAML_MODULE, ocaml_commands) \
  $(call OCAML_MODULE, ocaml_tools) \
  $(call OCAML_MODULE, ocaml_compilers) \
  $(call OCAML_MODULE, ocaml_toplevels) \
  $(call OCAML_MODULE, ocaml_actions) \
  $(call OCAML_MODULE, ocaml_tests)

sources := $(core) $(ocaml_plugin) \
  $(call OCAML_MODULE, options) \
  $(call OCAML_MODULE, main)

# List of .ml files used for ocamldep and to get the list of modules

ml_files := \
  $(filter %.ml, \
    $(subst .ml.in,.ml,$(subst .mll,.ml,$(subst .mly,.ml,$(sources)))) \
  )

cmo_files := $(ml_files:.ml=.cmo)

cmx_files := $(ml_files:.ml=.cmx)

# List of .mli files for ocamldep
mli_files := \
  $(filter %.mli,$(subst .mly,.mli,$(sources)))

cmi_files := $(mli_files:.mli=.cmi)

c_files := $(filter %.c, $(sources))

o_files := $(c_files:.c=.$(O))

lexers := $(filter %.mll,$(sources))

parsers := $(filter %.mly,$(sources))

dependencies_generated_prereqs := \
  $(lexers:.mll=.ml) \
  $(parsers:.mly=.mli) $(parsers:.mly=.ml)

generated := $(dependencies_generated_prereqs) $(parsers:.mly=.output)

bytecode_modules := $(o_files) $(cmo_files)

native_modules := $(o_files) $(cmx_files)

directories := $(addprefix $(ROOTDIR)/,utils bytecomp parsing \
                                       compilerlibs file_formats)

include_directories := $(addprefix -I , $(directories))

flags := $(STDLIBFLAGS) -g $(include_directories) \
  -strict-sequence -strict-formats \
  -w +a-4-9-41-42-44-45-48 -warn-error +A

ocamlc = $(BEST_OCAMLC) $(flags)

ocamlopt = $(BEST_OCAMLOPT) $(flags)

.SECONDARY: $(lexers:.mll=.ml) $(parsers:.mly=.mli) $(parsers:.mly=.ml)

.PHONY: all allopt opt.opt # allopt and opt.opt are synonyms
all: ocamltest$(EXE)
allopt: ocamltest.opt$(EXE)
opt.opt: allopt

compdeps_names=ocamlcommon ocamlbytecomp
compdeps_paths=$(addprefix $(ROOTDIR)/compilerlibs/,$(compdeps_names))
deps_paths = \
  $(compdeps_paths) $(addprefix $(ocamltest_unix_path)/,$(ocamltest_unix_name))
deps_byte=$(addsuffix .cma,$(deps_paths))
deps_opt=$(addsuffix .cmxa,$(deps_paths))

$(eval $(call PROGRAM_SYNONYM,ocamltest))

ocamltest_unix.%: flags+=$(ocamltest_unix_include) -opaque

ocamltest$(EXE): $(deps_byte) $(bytecode_modules)
	$(V_LINKC)$(ocamlc) $(ocamltest_unix_include)-custom -o $@ $^

%.cmo: %.ml $(deps_byte)
	$(V_OCAMLC)$(ocamlc) -c $<

$(eval $(call PROGRAM_SYNONYM,ocamltest.opt))

ocamltest.opt$(EXE): $(deps_opt) $(native_modules)
	$(V_LINKOPT)$(ocamlopt) $(ocamltest_unix_include)-o $@ $^

%.cmx: %.ml $(deps_opt)
	$(V_OCAMLOPT)$(ocamlopt) -c $<

%.cmi: %.mli $(deps_byte)
	$(V_OCAMLC)$(ocamlc) -c $<

# Manual

.PHONY: doc

doc: ocamltest.html

ocamltest.html: ocamltest.org
	pandoc -s --toc -N -f org -t html -o $@ $<

.PHONY: clean
clean:
	rm -rf ocamltest ocamltest.exe ocamltest.opt ocamltest.opt.exe
	rm -rf *.o *.obj *.cm*
	rm -rf $(generated)
	rm -f ocamltest.html
	rm -rf $(DEPDIR)

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(c_files:.c=.$(D)))
endif

.PHONY: distclean
distclean: clean
	rm -f ocamltest_config.ml ocamltest_unix.ml

$(DEPDIR)/%.$(D): %.c | $(DEPDIR)
	$(V_CCDEPS)$(DEP_CC) $(OC_CPPFLAGS) $(CPPFLAGS) $< -MT '$*.$(O)' -MF $@

.PHONY: depend
depend: $(dependencies_generated_prereqs)
	$(V_OCAMLDEP)$(OCAMLDEP_CMD) $(mli_files) $(ml_files) > .depend

-include .depend

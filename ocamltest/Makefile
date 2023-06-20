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

ocamltest_CORE = \
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

ocamltest_ocaml_PLUGIN = \
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

ocamltest_SOURCES = $(ocamltest_CORE) $(ocamltest_ocaml_PLUGIN) \
  $(call OCAML_MODULE, options) \
  $(call OCAML_MODULE, main)

# List of .ml files used for ocamldep and to get the list of modules

ocamltest_ML_FILES = \
  $(filter %.ml, \
    $(subst .ml.in,.ml,\
      $(subst .mll,.ml,$(subst .mly,.ml,$(ocamltest_SOURCES)))))

ocamltest_CMO_FILES = $(ocamltest_ML_FILES:.ml=.cmo)

ocamltest_CMX_FILES = $(ocamltest_ML_FILES:.ml=.cmx)

# List of .mli files for ocamldep
ocamltest_MLI_FILES = \
  $(filter %.mli,$(subst .mly,.mli,$(ocamltest_SOURCES)))

ocamltest_C_FILES = $(filter %.c, $(ocamltest_SOURCES))
ocamltest_STUBS = $(subst .c, , $(ocamltest_C_FILES))

ocamltest_LEXERS = $(filter %.mll,$(ocamltest_SOURCES))

ocamltest_PARSERS = $(filter %.mly,$(ocamltest_SOURCES))

ocamltest_DEPS_PREREQS = \
  $(ocamltest_LEXERS:.mll=.ml) \
  $(ocamltest_PARSERS:.mly=.mli) $(ocamltest_PARSERS:.mly=.ml)

ocmaltest_GENERATED = \
  $(ocamltest_DEPS_PREREQS) $(ocamltest_PARSERS:.mly=.output)

directories = $(addprefix $(ROOTDIR)/,utils bytecomp parsing \
                                       compilerlibs file_formats)

include_directories = $(addprefix -I , $(directories))

flags = $(STDLIBFLAGS) -g $(include_directories) \
  -strict-sequence -strict-formats \
  -principal -absname -bin-annot \
  -w +a-4-9-40-41-42-44-45-48 -warn-error +A

ocamlc = $(BEST_OCAMLC) $(flags)

ocamlopt = $(BEST_OCAMLOPT) $(flags)

.SECONDARY: $(ocamltest_LEXERS:.mll=.ml) \
  $(ocamltest_PARSERS:.mly=.mli) $(ocamltest_PARSERS:.mly=.ml)

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

ocamltest$(EXE): \
  $(deps_byte) \
  $(ocamltest_STUBS:=.b.$(O)) $(ocamltest_MODULES:=.cmo)
	$(V_LINKC)$(ocamlc) $(ocamltest_unix_include)-custom -o $@ $^

%.cmo: %.ml $(deps_byte)
	$(V_OCAMLC)$(ocamlc) -c $<

$(eval $(call PROGRAM_SYNONYM,ocamltest.opt))

ocamltest.opt$(EXE): \
  $(deps_opt) \
  $(ocamltest_STUBS:=.n.$(O)) $(ocamltest_MODULES:=.cmx)
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
	rm -rf $(ocmaltest_GENERATED)
	rm -f ocamltest.html
	rm -rf $(DEPDIR)

ifeq "$(COMPUTE_DEPS)" "true"
include $(addprefix $(DEPDIR)/, $(ocamltest_C_FILES:.c=.$(D)))
endif

.PHONY: distclean
distclean: clean
	rm -f ocamltest_config.ml ocamltest_unix.ml

$(DEPDIR)/%.$(D): %.c | $(DEPDIR)
	$(V_CCDEPS)$(DEP_CC) $(OC_CPPFLAGS) $(CPPFLAGS) $< -MT '$*.$(O)' -MF $@

.PHONY: depend
depend: $(ocamltest_DEPS_PREREQS)
	$(V_OCAMLDEP)$(OCAMLDEP_CMD) $(ocamltest_MLI_FILES) $(ocamltest_ML_FILES) > .depend

-include .depend

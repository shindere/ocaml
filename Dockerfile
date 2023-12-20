# Copied from:
# https://github.com/ocaml/ocaml/pull/12843#discussion_r1431595758

FROM ocaml/opam:alpine-3.19-opam
RUN git clone https://github.com/ocaml/ocaml.git && git -C ocaml checkout 3908dd368eaf391f671db33804a452ad1868fb3a
WORKDIR ocaml
RUN ./configure && make -j coldstart

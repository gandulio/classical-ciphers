FROM ocaml/opam:alpine
RUN sudo apk add m4
RUN opam repository add home https://opam.ocaml.org
RUN opam upgrade -y && opam depext -i containers jbuilder core_kernel reason
COPY Main.re Main.re
COPY jbuild jbuild
COPY compile.sh compile.sh
COPY run.sh run.sh
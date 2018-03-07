FROM ocaml/opam:alpine
RUN sudo apk add m4
RUN opam repository add home https://opam.ocaml.org
RUN opam upgrade -y
RUN opam pin add jbuilder https://github.com/dra27/jbuilder.git#limit-fds
RUN opam depext -i containers core_kernel reason minicli
COPY Main.re Main.re
COPY jbuild jbuild
COPY compile.sh compile.sh
COPY run.sh run.sh
copy small.txt small.txt
copy big.txt big.txt
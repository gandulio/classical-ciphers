FROM ocaml/opam:alpine
RUN sudo apk add m4 && \
    opam repository add home https://opam.ocaml.org && \
    opam upgrade -y && \
    opam pin add jbuilder https://github.com/dra27/jbuilder.git#limit-fds && \
    opam depext -i containers core_kernel reason minicli
COPY Main.re Main.re
COPY jbuild jbuild
COPY compile.sh compile.sh
COPY run.sh run.sh
COPY small.txt small.txt
COPY big.txt big.txt
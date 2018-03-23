FROM ocaml/opam:alpine
RUN sudo apk add m4 && \
    opam repository add home https://opam.ocaml.org && \
    opam upgrade -y && \
    opam pin add jbuilder https://github.com/dra27/jbuilder.git#limit-fds && \
    opam depext -i containers core_kernel reason minicli
COPY Main.re Main.re
COPY Common.re Common.re
COPY Caesar.re Caesar.re
COPY Playfair.re Playfair.re
COPY Rail_Fence.re Rail_Fence.re
COPY Row_Transpose.re Row_Transpose.re
COPY Vigenere.re Vigenere.re
COPY jbuild jbuild
COPY compile.sh compile.sh
COPY run.sh run.sh
COPY gradeit.sh gradeit.sh
COPY show.sh show.sh
COPY small.txt small.txt
COPY big.txt big.txt
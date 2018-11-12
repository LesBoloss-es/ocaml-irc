ARG TAG=latest
ARG IMAGE=ocaml/opam2:$TAG
ARG SWITCH=

FROM $IMAGE
MAINTAINER Nicolas Jeannerod

RUN [ -z "$SWITCH" ] || opam switch create "$SWITCH"

WORKDIR /home/opam/workdir

COPY *.opam .
RUN opam depext -i $(opam show . -f depends: | cut -d '"' -f 2)

COPY . .
RUN sudo chown -R opam .
RUN eval $(opam env) && make
RUN eval $(opam env) && make test
RUN eval $(opam env) && make examples
RUN eval $(opam env) && make doc

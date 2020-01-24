FROM ubuntu as builder
ENV TZ=Antarctica/Palmer
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update
RUN apt-get install -y ghc ghc-prof ghc-doc
RUN apt-get install -y cabal-install
RUN cabal update
ARG CACHEBUST=1 
COPY . .
WORKDIR app
RUN cabal build
CMD cabal run
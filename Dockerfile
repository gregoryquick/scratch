FROM ubuntu as builder
ENV TZ=Antarctica/Palmer
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update
RUN apt-get install -y build-essential libgtk2.0-dev
RUN apt-get install -y pkg-config
RUN apt-get install libcairo2-dev
RUN apt-get install -y ghc ghc-prof ghc-doc
RUN apt-get install -y cabal-install
RUN cabal v2-update
RUN mkdir /build 
WORKDIR /build 
RUN cabal v1-install alex
RUN cabal v1-install happy
RUN cabal v1-install gtk2hs-buildtools-0.13.5.1
RUN cabal v1-install cairo
RUN cabal v1-install netwire
RUN cabal v1-install vector-space 
COPY src src
COPY LICENSE .
COPY Setup.hs .
COPY app.cabal .
RUN cabal v1-build
CMD cabal v1-run
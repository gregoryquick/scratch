FROM ubuntu as builder
ENV TZ=Antarctica/Palmer
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN apt-get update
RUN apt-get install -y build-essential libgtk2.0-dev
RUN apt-get install -y pkg-config
RUN apt-get install libcairo2-dev
RUN apt-get install -y ghc ghc-prof ghc-doc
RUN apt-get install -y cabal-install
RUN cabal update
RUN cabal install --global alex
RUN cabal install --global happy
RUN cabal install --global gtk2hs-buildtools-0.13.5.1
RUN cabal install --global cairo
RUN cabal install --global netwire
ARG CACHEBUST=1 
COPY . .
WORKDIR app
RUN cabal build
CMD cabal run
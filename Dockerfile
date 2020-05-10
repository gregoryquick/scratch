<<<<<<< HEAD
FROM debian as build
RUN apt-get update
RUN apt-get install -y pkg-config
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir build
RUN chmod -R ugo+rw /build/
WORKDIR build
COPY stack.yaml /build/stack.yaml 
COPY app.cabal /build/app.cabal
RUN apt-get install -y libcairo2-dev
RUN stack install cairo
RUN stack install netwire
RUN stack install mtl

COPY src ./src
COPY LICENSE .
COPY Setup.hs .
RUN stack build --dependencies-only
RUN stack build
#RUN mv "$(stack path --local-install-root)/bin" /build/bin
CMD stack run

#FROM ubuntu as app
#RUN mkdir /app
#WORKDIR /app
#COPY --from=build /build/bin .
#EXPOSE 8080
#CMD ["/app/app", "8080"]



=======
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
RUN cabal install --global vector-space
ARG CACHEBUST=1 
COPY . .
WORKDIR app
RUN cabal build
CMD cabal run
>>>>>>> parent of a942b3b... Changed to stack for building

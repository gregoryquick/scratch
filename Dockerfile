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




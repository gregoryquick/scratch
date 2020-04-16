FROM fpco/stack-build as dependencies
RUN mkdir /build
WORKDIR /build
#Get dependencies
RUN apt-get -y update && apt-get -y upgrade
RUN apt-get install -y build-essential libgtk2.0-dev
RUN apt-get install -y pkg-config
RUN apt-get install -y libcairo2-dev
# cabal stuffs
COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config
#Get my cabal and stack info
COPY stack.yaml /build/stack.yaml
COPY app.cabal /build/app.cabal
#Build depenencies
RUN stack build --system-ghc --dependencies-only

FROM fpco/stack-build as build
COPY --from=dependencies /root/.stack /root/.stack
COPY . /build/
WORKDIR /build
RUN stack build --system-ghc
#RUN mv "$(stack path --local-install-root --system-ghc)/bin" /build/bin

CMD stack run
#FROM ubuntu as app
#RUN mkdir /app
#WORKDIR /app

#COPY --from=build /build/bin .
#EXPOSE 8080
#CMD ["/app/app", "8080"]

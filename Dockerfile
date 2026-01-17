FROM alpine:3.18.4 AS build

LABEL "org.opencontainers.image.title"="Dojang"
LABEL "org.opencontainers.image.licenses"="GPL-3.0-or-later"

RUN apk add --no-cache build-base=0.5-r3 \
        cabal=3.10.1.0-r1 \
        ghc=9.4.4-r1 \
        zlib-dev=1.2.13-r1 \
        zlib-static=1.2.13-r1
RUN cabal v2-update && cabal v2-install --constraint="text -simdutf" --constraint="hpack <0.37" hpack

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/dojang/package.yaml

WORKDIR /src/dojang

RUN /root/.local/bin/hpack
# hadolint ignore=DL3059
RUN cabal v2-configure --constraint="dojang +static"
# hadolint ignore=DL3059
RUN cabal v2-build --only-dependencies

COPY . /src/dojang

RUN /root/.local/bin/hpack

ARG DOJANG_DEV_BUILD

RUN if [ "$DOJANG_DEV_BUILD" != "" ]; then \
        echo "packages: ./dojang.cabal" >> cabal.project && \
        echo "package dojang" >> cabal.project && \
        echo "  ghc-options: -DDOJANG_DEV_BUILD=$DOJANG_DEV_BUILD" \
        >> cabal.project; \
        fi && cabal v2-install

FROM alpine:3.18.4

COPY --from=build /root/.local/bin/dojang /usr/local/bin/

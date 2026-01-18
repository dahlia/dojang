FROM alpine:3.23 AS build

LABEL "org.opencontainers.image.title"="Dojang"
LABEL "org.opencontainers.image.licenses"="GPL-3.0-or-later"

# Install build dependencies
RUN apk add --no-cache \
        bash=5.3.3-r1 \
        build-base=0.5-r3 \
        curl=8.17.0-r1 \
        gmp-dev=6.3.0-r4 \
        gmp-static=6.3.0-r4 \
        libffi-dev=3.5.2-r0 \
        ncurses-dev=6.5_p20251123-r0 \
        zlib-dev=1.3.1-r2 \
        zlib-static=1.3.1-r2

# Install ghcup, GHC 9.10.3, and cabal
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1
ENV BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1
ENV GHCUP_INSTALL_BASE_PREFIX=/root
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ENV PATH="/root/.ghcup/bin:${PATH}"
RUN ghcup install ghc 9.10.3 --set && \
    ghcup install cabal recommended --set

# Install hpack
RUN cabal v2-update && \
    cabal v2-install --constraint="text -simdutf" --constraint="hpack <0.37" hpack

# Add just the package.yaml file to capture dependencies
COPY package.yaml /src/dojang/package.yaml

WORKDIR /src/dojang

RUN /root/.local/bin/hpack
# hadolint ignore=DL3059
RUN cabal v2-configure --constraint="dojang +static" \
        --allow-newer=toml-parser:base \
        --allow-newer=fortytwo:text \
        --allow-newer=fortytwo:ansi-terminal
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

FROM alpine:3.23

COPY --from=build /root/.local/bin/dojang /usr/local/bin/

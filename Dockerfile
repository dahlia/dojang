# syntax=docker/dockerfile:1

FROM alpine:3.23 AS build

ARG TARGETARCH
ARG GHCUP_VERSION=0.1.40.0
ARG GHCUP_METADATA_COMMIT=4ad95215c0f869d5da04ff05e70b60027e20547f
ARG GHCUP_METADATA_SHA256=a4db470bb1fa67e71df84acd24b023779a9d6c4633d3e6ca90976ee1529e7922
ARG GHC_VERSION=9.10.3
ARG STACK_VERSION=3.11.1
ARG GHCUP_X86_64_SHA256=70ca52b73ee796f5c43b4259f7fcedc2a0d60d85a6a9ed40a82ea8553fca34a0
ARG GHCUP_AARCH64_SHA256=86df64134ab8ca6d4e8b0980e94fc36a447ff09ea823885034a3dd5617e840f3
ARG GHC_X86_64_SHA256=0253c087da23aabfb6521e741cd3a35d3fcde201d74e34cd293b2bbde722432d
ARG GHC_AARCH64_SHA256=4866d3b241c59860f2f6eaa2a5e96632113e57100b911ff8aa936665cc0045fe
ARG STACK_X86_64_SHA256=1fda71e657cd8d355625cc66b61b352699279dfee2664c014a392163bd19a952
ARG STACK_AARCH64_SHA256=1617ae9976a5cd38ad4daec583b026b589eb45d5482afb045cd4ca8c8d0de6d0

RUN apk add --no-cache \
        bash=5.3.3-r1 \
        build-base=0.5-r3 \
        ca-certificates=20260611-r0 \
        coreutils=9.8-r1 \
        curl=8.20.0-r0 \
        gmp-dev=6.3.0-r4 \
        gmp-static=6.3.0-r4 \
        libffi-dev=3.5.2-r0 \
        ncurses-dev=6.5_p20251123-r0 \
        perl=5.42.2-r0 \
        tar=1.35-r4 \
        xz=5.8.3-r0 \
        zlib-dev=1.3.2-r0 \
        zlib-static=1.3.2-r0

# Install the official GHCup binary only after verifying its architecture-
# specific release checksum.
RUN set -eux; \
    case "$TARGETARCH" in \
      amd64) \
        tool_arch="x86_64"; \
        checksum="$GHCUP_X86_64_SHA256" \
        ;; \
      arm64) \
        tool_arch="aarch64"; \
        checksum="$GHCUP_AARCH64_SHA256" \
        ;; \
      *) \
        echo "Unsupported build architecture: $TARGETARCH" >&2; \
        exit 1 \
        ;; \
    esac; \
    url="https://github.com/haskell/ghcup-hs/releases/download/v$GHCUP_VERSION/$tool_arch-linux-ghcup-$GHCUP_VERSION"; \
    curl --proto '=https' --tlsv1.2 -fsSL "$url" -o /tmp/ghcup; \
    echo "$checksum  /tmp/ghcup" | sha256sum -c -; \
    install -m 0755 /tmp/ghcup /usr/local/bin/ghcup; \
    rm /tmp/ghcup

ENV PATH="/root/.ghcup/bin:${PATH}"

# Cache a checksum-pinned GHCup metadata snapshot, then install from the exact
# official Alpine bindist URL without consulting a mutable metadata channel.
RUN set -eux; \
    metadata_url="https://raw.githubusercontent.com/haskell/ghcup-metadata/$GHCUP_METADATA_COMMIT/ghcup-0.0.9.yaml"; \
    mkdir -p /root/.ghcup/cache; \
    curl --proto '=https' --tlsv1.2 -fsSL "$metadata_url" \
      -o /root/.ghcup/cache/ghcup-0.0.9.yaml; \
    echo "$GHCUP_METADATA_SHA256  /root/.ghcup/cache/ghcup-0.0.9.yaml" \
      | sha256sum -c -; \
    case "$TARGETARCH" in \
      amd64) \
        ghc_arch="x86_64"; \
        checksum="$GHC_X86_64_SHA256" \
        ;; \
      arm64) \
        ghc_arch="aarch64"; \
        checksum="$GHC_AARCH64_SHA256" \
        ;; \
      *) \
        echo "Unsupported build architecture: $TARGETARCH" >&2; \
        exit 1 \
        ;; \
    esac; \
    url="https://downloads.haskell.org/~ghc/$GHC_VERSION/ghc-$GHC_VERSION-$ghc_arch-alpine3_18-linux.tar.xz"; \
    curl --proto '=https' --tlsv1.2 -fsSL "$url" -o /tmp/ghc.tar.xz; \
    echo "$checksum  /tmp/ghc.tar.xz" | sha256sum -c -; \
    ghcup -o -n install ghc -u file:///tmp/ghc.tar.xz "$GHC_VERSION"; \
    rm /tmp/ghc.tar.xz; \
    ghcup -o set ghc "$GHC_VERSION"; \
    ghc --numeric-version | grep -Fx "$GHC_VERSION"

# Stack's release archive is also architecture-specific and checksum-pinned.
RUN set -eux; \
    case "$TARGETARCH" in \
      amd64) \
        stack_arch="x86_64"; \
        checksum="$STACK_X86_64_SHA256" \
        ;; \
      arm64) \
        stack_arch="aarch64"; \
        checksum="$STACK_AARCH64_SHA256" \
        ;; \
      *) \
        echo "Unsupported build architecture: $TARGETARCH" >&2; \
        exit 1 \
        ;; \
    esac; \
    archive="stack-$STACK_VERSION-linux-$stack_arch.tar.gz"; \
    url="https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/$archive"; \
    curl --proto '=https' --tlsv1.2 -fsSL "$url" -o "/tmp/$archive"; \
    echo "$checksum  /tmp/$archive" | sha256sum -c -; \
    tar -xzf "/tmp/$archive" -C /tmp; \
    install -m 0755 \
      "/tmp/stack-$STACK_VERSION-linux-$stack_arch/stack" \
      /usr/local/bin/stack; \
    rm -rf "/tmp/$archive" "/tmp/stack-$STACK_VERSION-linux-$stack_arch"; \
    stack --numeric-version | grep -Fx "$STACK_VERSION"

WORKDIR /src/dojang

# Resolve locked dependencies before copying frequently changing sources.
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build \
      --only-dependencies \
      --system-ghc \
      --no-install-ghc

RUN set -eux; \
    adduser -D builder; \
    ln -s /bin/false /usr/bin/false; \
    chmod 0755 /root; \
    mkdir -p /home/builder/.stack /out; \
    cp -a /root/.stack/. /home/builder/.stack/; \
    chown -R builder:builder \
      /home/builder \
      /out \
      /src/dojang

COPY . .

ENV HOME="/home/builder"

# Run the complete test suite in the same Alpine toolchain used for releases.
RUN --mount=type=tmpfs,target=/tmp \
    set -eux; \
    chmod 1777 /tmp; \
    su builder -c \
      'TMPDIR="/tmp" stack test --system-ghc --no-install-ghc'

USER builder

ARG DOJANG_DEV_BUILD

RUN set -eux; \
    case "${DOJANG_DEV_BUILD:-}" in \
      "") \
        ghc_options="-O2" \
        ;; \
      *[!0-9]*) \
        echo "DOJANG_DEV_BUILD must be an integer." >&2; \
        exit 1 \
        ;; \
      *) \
        ghc_options="-O2 -DDOJANG_DEV_BUILD=$DOJANG_DEV_BUILD" \
        ;; \
    esac; \
    stack build \
      --flag dojang:static \
      --system-ghc \
      --no-install-ghc \
      --ghc-options="$ghc_options" \
      --copy-bins \
      --local-bin-path=/out; \
    ! readelf -l /out/dojang | grep -q INTERP

FROM alpine:3.23

LABEL "org.opencontainers.image.title"="Dojang"
LABEL "org.opencontainers.image.licenses"="GPL-3.0-or-later"
LABEL "org.opencontainers.image.source"="https://github.com/dahlia/dojang"

COPY --from=build /out/dojang /usr/local/bin/

ENTRYPOINT ["dojang"]

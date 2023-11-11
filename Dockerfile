FROM debian:bookworm-slim as builder

# Postgres-websockets repo metadata
ARG GIT_REPO=https://github.com/diogob/postgres-websockets.git
ARG GIT_TAG=multiple-docker-architectures

# Install System Dependencies
RUN apt-get update \
    && apt-get install -y \
        libpq-dev \
        wget \
        git \
        build-essential \
        libffi-dev \
        libgmp-dev \
        zlib1g-dev \
        curl \
    && rm -rf /var/lib/apt/lists/*

# Clone the Repository
WORKDIR /app
RUN git clone \
    $GIT_REPO \
    --branch $GIT_TAG \
    --depth 1

# Build the Project from source using the resolver it specifies
# https://github.com/diogob/postgres-websockets/tree/master#building-from-source
WORKDIR /app/postgres-websockets
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# install gpg keys
RUN \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C \
    && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01 \
    && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 88B57FCF7DB53B4DB3BFA4B1588764FBE22D19C4 \
    && gpg --batch --keyserver keyserver.ubuntu.com --recv-keys EAF2A9A722C0C96F2B431CA511AAD8CEDEE0CAEF

# install ghcup
RUN \
    curl https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup > /usr/bin/ghcup \
    && chmod +x /usr/bin/ghcup \
    && ghcup config set gpg-setting GPGStrict

ARG GHC=9.6.3
ARG CABAL=latest

# install GHC and cabal
RUN \
    ghcup -v install ghc --isolate /usr/local --force ${GHC} && \
    ghcup -v install cabal --isolate /usr/local/bin --force ${CABAL}

RUN cabal update
RUN cabal build
RUN cp `find dist-newstyle -executable -type f -name postgres-websockets` ./

# Lightweight Final Image
FROM debian:bookworm-slim

# Install Runtime Dependencies
RUN apt-get update \
    && apt-get install -y \
        libpq-dev \
        libgmp-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy the Binary from the Builder
COPY --from=builder /app/postgres-websockets/postgres-websockets /usr/local/bin/postgres-websockets

# Set the Entry Point
ENTRYPOINT ["postgres-websockets"]

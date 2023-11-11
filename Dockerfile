FROM haskell:9.6.3-slim as builder

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
RUN cabal build
RUN cabal install

# Lightweight Final Image
FROM debian:bullseye-slim

# Install Runtime Dependencies
RUN apt-get update \
    && apt-get install -y \
        libpq-dev \
        libgmp-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy the Binary from the Builder
COPY --from=builder /root/.local/bin/postgres-websockets /usr/local/bin/postgres-websockets

# Set the Entry Point
ENTRYPOINT ["postgres-websockets"]

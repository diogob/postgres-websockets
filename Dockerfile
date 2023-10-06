# Use Alpine Linux as base image
FROM alpine:edge

# Install libpq and gmp dependencies (dynamic libraries required by the project)
RUN apk update && apk add libpq gmp libffi

# Copy the prebuilt binary from stack-work into the container
# (substitute your project name for 'example')
COPY .stack-work/docker/_home/.local/bin/postgres-websockets /usr/local/bin/postgres-websockets
COPY docker.conf /etc/postgres-websockets.conf
COPY ./client-example /home/postgres-websockets/client-example

RUN adduser -D postgres-websockets
USER postgres-websockets

ENV PGWS_DB_URI= \
  PGWS_JWT_SECRET=

# Run the binary on container start
# (substitute your project name for 'example')
CMD postgres-websockets

EXPOSE 3000
STOPSIGNAL SIGINT

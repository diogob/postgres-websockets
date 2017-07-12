# Use Alpine Linux as base image
FROM alpine:3.6

# Install libpq and gmp dependencies (dynamic libraries required by the project)
RUN apk update && apk add libpq gmp libffi

# Copy the prebuilt binary from stack-work into the container
# (substitute your project name for 'example')
COPY .stack-work/docker/_home/.local/bin/postgrest-ws /usr/local/bin/postgrest-ws
COPY docker.conf /etc/postgrest-ws.conf

RUN adduser -D postgrest-ws
USER postgrest-ws

ENV PGWS_DB_URI= \
    PGWS_JWT_SECRET=

# Run the binary on container start
# (substitute your project name for 'example')
CMD ["postgrest-ws", "/etc/postgrest-ws.conf"]

EXPOSE 3000

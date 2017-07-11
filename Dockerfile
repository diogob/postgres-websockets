# Use Alpine Linux as base image
FROM alpine:3.6

# Install libpq and gmp dependencies (dynamic libraries required by the project)
RUN apk update && apk add libpq gmp libffi

# Copy the prebuilt binary from stack-work into the container
# (substitute your project name for 'example')
COPY .stack-work/docker/_home/.local/bin/postgrest-ws /usr/local/bin/postgrest-ws

RUN adduser -D postgrest-ws
USER postgrest-ws

ENV PGRSTWS_DB_URI= \
    PGRST_DB_POOL=100 \
    PGRST_SERVER_HOST=*4 \
    PGRST_SERVER_PORT=3000 \
    PGRST_SERVER_PROXY_URL= \
    PGRST_JWT_SECRET= \
    PGRST_SECRET_IS_BASE64=false \
    PGRST_MAX_ROWS= \
    PGRST_PRE_REQUEST=

# Run the binary on container start
# (substitute your project name for 'example')
CMD ["postgrest-ws"]

EXPOSE 3000

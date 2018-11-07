FROM alpine:3.8

RUN echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk -U add shadow@testing

RUN apk add --update ca-certificates openssl && update-ca-certificates

RUN apk add ghc alpine-sdk linux-headers postgresql-dev zlib-dev

RUN wget -qO- https://get.haskellstack.org/ | sh
RUN stack config set system-ghc --global true

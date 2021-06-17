docker build -f Dockerfile.build -t diogob/alpine-ghc .
stack --compiler ghc-8.8.3 --system-ghc --docker --docker-image=diogob/alpine-ghc install
docker build -t diogob/postgres-websockets .

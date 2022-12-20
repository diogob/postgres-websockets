docker build -f Dockerfile.build -t diogob/alpine-ghc .
stack --system-ghc --docker --docker-image=diogob/alpine-ghc install
docker build -t diogob/postgres-websockets .

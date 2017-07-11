docker build -f Dockerfile.build -t diogob/alpine-ghc .
stack --docker --docker-image=diogob/alpine-ghc install
docker build -t diogob/postgrest-ws .

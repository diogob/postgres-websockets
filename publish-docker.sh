VERSION=$(awk '/^version: / { print $2 };' < postgres-websockets.cabal)
docker build . --tag diogob/postgres-websockets:latest
docker tag diogob/postgres-websockets:latest diogob/postgres-websockets:$VERSION
docker push diogob/postgres-websockets
docker push diogob/postgres-websockets:$VERSION

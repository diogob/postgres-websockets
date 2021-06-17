VERSION=$(awk '/^version: / { print $2 };' < postgres-websockets.cabal)
docker tag diogob/postgres-websockets:latest diogob/postgres-websockets:$VERSION
docker push diogob/postgres-websockets
docker push diogob/postgres-websockets:$VERSION
docker tag diogob/postgres-websockets docker.pkg.github.com/diogob/postgres-websockets/postgres-websockets:$VERSION
docker push docker.pkg.github.com/diogob/postgres-websockets/postgres-websockets
docker tag diogob/postgres-websockets docker.pkg.github.com/diogob/postgres-websockets/postgres-websockets:latest
docker push docker.pkg.github.com/diogob/postgres-websockets/postgres-websockets
docker push docker.pkg.github.com/diogob/postgres-websockets/postgres-websockets:$VERSION

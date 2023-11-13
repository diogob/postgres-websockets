VERSION=$(awk '/^version: / { print $2 };' < postgres-websockets.cabal)
docker buildx build --platform amd64 .
docker buildx build --platform arm64 .
docker buildx build -t diogob/postgres-websockets:latest --platform arm64,amd64 --push .
docker buildx build -t diogob/postgres-websockets:$VERSION --platform arm64,amd64 --push .

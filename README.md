# postgres-websockets

![CI](https://github.com/diogob/postgres-websockets/actions/workflows/ci.yml/badge.svg)

postgres-websockets is a [middleware](https://hackage.haskell.org/package/wai) that adds websockets capabilites on top of [PostgreSQL](https://www.postgresql.org)'s asynchronous notifications using LISTEN and NOTIFY commands.

postgres-websockets allows you to:

 * Send messages a websocket triggering a [NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) command in a PostgreSQL database.
 * Receive messages sent to any database channel though a websocket.
 * Authorize the use of channels using a [JWT](https://jwt.io) issued by another service.
 * Authorize read-only, write-only, or read and write websockets.

## Running the server

### Quickstart using docker-compose
The `docker-compose.yml` present in the repository will start a PostgreSQL database alongside a postgres-websockets and a pg-recorder.
To try it out, you will need [Docker](https://www.docker.com/) installed and [git](https://git-scm.com) to clone this repository.

```bash
git clone https://github.com/diogob/postgres-websockets.git
cd postgres-websockets
docker-compose up
```

### Pre-compiled binaries

You can download binaries from the [releases page](https://github.com/diogob/postgres-websockets/releases). Currently, only Linux binaries complied against Ubuntu on amd64 are provided.

### Building from source
To build the project, I recommend the use of [GHCup](https://www.haskell.org/ghcup/) to install GHC and cabal.
You also need to have [git](https://git-scm.com) installed to download the source code.
Having installed ghc and cabal the following commands should install `postgres-websockets` into your `~/.cabal/bin` directory:

```bash
git clone https://github.com/diogob/postgres-websockets.git
cd postgres-websockets
cabal install
```

If you have any problems processing any Postgres related library on a Mac, try installing [Postgres.app](http://postgresapp.com/).

After the build, you should be able to run the server using `~/.cabal/bin/postgres-websockets`.

To run the example below you will need a PostgreSQL server running on port 5432 of your localhost.

```bash
source sample-env && ~/.cabal/bin/postgres-websockets
```
After running the above command, open your browser on http://localhost:3000 to see an example of usage.

### Runtime configuration

The sample config file provided in the [sample-env](https://github.com/diogob/postgres-websockets/tree/master/sample-env) file has a jwt secret just for testing and is used in the [sample client]((https://github.com/diogob/postgres-websockets/tree/master/client-example)).

## Opening connections

To open a websocket connection to the server, there are two possible request formats:

1. Requesting a channel and giving a token

When you request access to a channel called `chat` the address of the websockets will look like:
```
ws://chat/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.QKGnMJe41OFZcjz_qQSplmWAmVd_hmVjijKUNoJYpis
```

The token on the URL above has no `channels` claim, therefore is can be used to open connections to any channel, be careful when issuing those.

When the token contains a `channels` claim, the value of that claim should be a list of allowed channels.
Any requested channel not set in that claim will result in an error opening the connection. 

2. Giving only the token

When you inform only the token on the websocket address, the `channels` claim must be present.
In this case, all channels present in the claim will be available simultaneously in the same connection.
This is useful for clients that need to monitor or broadcast a set of channels, being more convenient than managing multiple websockets.
The address will look like:
```
ws://eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicnciLCJjaGFubmVsIjoiY2hhdCJ9.fEm6P7GHeJWZG8OtZhv3H0JdqPljE5dainvsoupM9pA
```

## Receiving messages from the browser

Every message received from the browser will be in JSON format as:
```javascript
{
  "event": "WebsocketMessage",
  "channel": "destination_channel",
  "payload": "message content",
  "claims": { "message_delivered_at": 0.0, "a_custom_claim_from_the_jwt": "your_custom_value" }
}
```

Where `claims` contain any custom claims added to the JWT with the added `message_delivered_at` which marks the timestamp in UNIX format of when the message was processed by postgres-websockets just before being sent to the database.
Also, `channel` contains the channel used to send the message, this should be used to send any messages back to that particular client.
Finally, `payload` contain a string with the message contents.

A easy way to process messages received asynchronously is to use [pg-recorder](https://github.com/diogob/pg-recorder) with some custom stored procedures.
For more options on notification processing, check the [PostgREST documentation on the topic](https://postgrest.com/en/v4.3/intro.html#external-notification).

## Sending messages to the browser

To send a message to a particular channel on the browser, one should notify the postgres-websockets listener channel and pass a JSON object containing the channel and payload such as:
```sql
SELECT pg_notify(
  'postgres-websockets-listener',
  json_build_object('event', 'WebsocketMessage', 'channel', 'chat', 'payload', 'test')::text
);
```

Where `postgres-websockets-listener` is the database channel used by your instance of postgres-websockets and `chat` is the channel where the browser is connected (the same issued in the JWT used to connect).

## Monitoring Connections

To monitor connection opening one should set the variable `PGWS_META_CHANNEL` which will enable the meta-data messages generation in the server on the channel name specified.
When using the configuration in the [sample-env](./sample-env) the channel `server-info` will receive see messages like the one below each time a connection is established (only after the JWT is validated).

```javascript
{"event":"ConnectionOpen","channel":"server-info","payload":"server-info","claims":{"mode":"rw","message_delivered_at":1.602719440727465893e9}}
```

You can also read these messages having an additional database listener on the `PGWS_LISTEN_CHANNEL`.

## Using a secure socket

To use a secure socket (`wss://`) you can set the configuration variables `PGWS_CERTIFICATE_FILE` and `PGWS_KEY_FILE`. Once these two variables point to a valid X.509 certificate, the server will enable TLS connections. Below a quick example of how to generate a self-signed certificate using [OpenSSL](https://www.openssl.org/) command line tool:

```
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```

## Recovering from listener database connection failures

The database connection used to wait for notification where the `LISTEN` command is issued can cause problems when it fails. To prevent this problem from completely disrupting our websockets server, there are two ways to configure postgres-websockets:

* Self healing connection - postgres-websockets comes with a connection supervisor baked in. You just need to set the configuration `PGWS_CHECK_LISTENER_INTERVAL` to a number of milliseconds that will be the maximum amount of time losing messages. There is a cost for this since at each interval an additional SELECT query will be issued to ensure the listener connection is still active. If the connection is not found, the connection thread will be killed and respawned. This method has the advantage of keeping all channels and websocket connections alive while the database connection is severed (although messages will be lost).
* Using external supervision - you can also unset `PGWS_CHECK_LISTENER_INTERVAL` and postgres-websockets will try to shut down the server when the database connection is lost. Some external process can then restart the server. All websocket connections will be lost.

## Acknowledgements

The project was largely inspired and originally designed for use with [PostgREST](https://github.com/begriffs/postgrest).

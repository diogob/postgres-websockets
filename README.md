# postgres-websockets [![CircleCI](https://circleci.com/gh/diogob/postgres-websockets.svg?style=svg)](https://circleci.com/gh/diogob/postgres-websockets)

postgres-websockets is a [middleware](https://hackage.haskell.org/package/wai) that adds websockets capabilites on top of [PostgreSQL](https://www.postgresql.org)'s asynchronous notifications using LISTEN and NOTIFY commands.
The project was largely inspired and originaly designed for use with [PostgREST](https://github.com/begriffs/postgrest).

postgres-websockets allows you to:
 * Open websockets with multiple channels and exchange messages with the database asynchronously.
 * Send messages to that websocket so they become a [NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) command in a PostgreSQL database.
 * Receive messages sent to any database channel though a websocket.
 * Authorize the use of channels using a [JWT](https://jwt.io) issued by another service or by another PostgREST endpoint.
 * Authorize read-only, write-only, or read and write websockets.

## Running the server

To build the project I recommend the use of [Stack](http://docs.haskellstack.org/en/stable/README/).
You also need to have [git](https://git-scm.com) installed to download the source code.
Having installed stack the following commands should install `postgres-websockets` into your `~/.local/bin` directory:

```bash
git clone https://github.com/diogob/postgres-websockets.git
cd postgres-websockets
stack setup
stack build
```

If you have any problems processing any Postgres related library on a Mac, try installing [Postgres.app](http://postgresapp.com/).

After the build you should be able to run the server using `~/.local/bin/postgres-websockets` (you can add `~/.local/bin` to your PATH variable):

To run the example bellow you will need a PostgreSQL server running on port 5432 of your localhost. You can also change the database connection string editting the `sample.conf` file.
```bash
~/.local/bin/postgres-websockets sample.conf
Listening on port 3000
```

After running the above command, open your browser on http://localhost:3000 to see an example of usage.

The sample config file provided in the [sample.conf](https://github.com/diogob/postgres-websockets/tree/master/sample.conf) file comes with a jwt secret just for testing and is used in the sample client.
You will find the complete sources for the example under the folder [client-example](https://github.com/diogob/postgres-websockets/tree/master/client-example).

## Opening connections

To open a websocket connection to the server there are two possible request formats.

1. Requesting a channel and giving a token

When you request access to a channel called `chat` the address of the websockets will look like:
```
ws://chat/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.QKGnMJe41OFZcjz_qQSplmWAmVd_hmVjijKUNoJYpis
```
When the token contains a "channel" claim, the value of that claim has precedence over the requested channel.


2. Giving only the token

When you inform only the token on the websocket address, the channel must be present in the claims of your token. The address will look like:
```
ws://eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicnciLCJjaGFubmVsIjoiY2hhdCJ9.fEm6P7GHeJWZG8OtZhv3H0JdqPljE5dainvsoupM9pA
```

To use a secure socket (`wss://`) you will need a proxy server like nginx to handle the TLS layer. Some services (e.g. Heroku) will handle this automatially.

## Monitoring messages

There is a way to receive a copy of every message sent from websocket clients to the server. This is useful in cases where one needs to audit the messages or persist then using an independent asynchronous process. To do so, one should enable a configuration called `audit-channel`. This option should be the name of a channel where all the messages sent from a websocket client will be replicated as an aditional NOTIFY command.

When running the example page, all messages received by the audit channel are visible in the last section of the page.

# PostgREST-WS [![CircleCI](https://circleci.com/gh/diogob/postgrest-ws.svg?style=svg)](https://circleci.com/gh/diogob/postgrest-ws)

PostgREST-WS is a [WAI](https://hackage.haskell.org/package/wai) middleware designed for use with [PostgREST](https://github.com/begriffs/postgrest)
that adds websockets capabilites to the HTTP layer on top of [PostgreSQL](https://www.postgresql.org).

PostgREST-WS allows you to:
 * Open websockets with multiple channels and relay messages to the database.
 * Send messages to that websocket so they become a [NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) command in a PostgreSQL database.
 * Receive messages sent to the database to that channel in that websocket.
 * Authorize the use of channels using a [JWT](https://jwt.io) issued by another service or by another PostgREST endpoint.

## Running the server

To build the project I recommend the use of [Stack](http://docs.haskellstack.org/en/stable/README/).
You also need to have [git](https://git-scm.com) installed to download the source code.
Having installed stack the following commands should install `postgrest-ws` into your `~/.local/bin` directory:

```bash
git clone https://github.com/diogob/postgrest-ws.git
cd postgrest-ws
stack setup
stack build
```

If you have any problems processing any Postgres related library on a Mac, try installing [Postgres.app](http://postgresapp.com/).

After the build you should be able to run the server using `~/.local/bin/postgrest-ws` (you can add `~/.local/bin` to your PATH variable):

To run the example bellow you will need a PostgreSQL server running on port 5432 of your localhost. You can also change the database connection string editting the `sample.conf` file.
```bash
~/.local/bin/postgrest-ws sample.conf
Listening on port 3000
```

After running the above command, open your browser on http://localhost:3000 to see an example of usage.

The sample config file provided in the [sample.conf](https://github.com/diogob/postgrest-ws/tree/master/sample.conf) file comes with a jwt secret just for testing and is used in the sample client.
You will find the complete sources for the example under the folder [client-example](https://github.com/diogob/postgrest-ws/tree/master/client-example).

## Monitoring messages

There is a way to receive a copy of every message sent from websocket clients to the server. This is useful in cases where one needs to audit the messages or persist then using an independent asynchronous process. To do so, one should enable a configuration called `audit-channel`. This option should be the name of a channel where all the messages sent from a websocket client will be replicated as an aditional NOTIFY command.

When running the example page, all messages received by the audit channel are visible in the last section of the page.

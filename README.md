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

## Receiving messages from the browser

Every message received from the browser will be in JSON format as:
```javascript
{
  "claims": { "message_delivered_at": 0.0, "a_custom_claim_from_the_jwt": "your_custom_value" },
  "channel": "destination_channel",
  "payload": "message content"
}
```

Where `claims` contain any custom claims added to the JWT with the added `message_delivered_at` which marks the timestamp in unix format of when the message was processed by postgres-websockets just before being sent to the database.
Also `channel` contains the channel requested in the JWT, and this should be used to send any messages back to that particular client.
Finally `payload` contain a string with the message contents.

A easy way to process messages received asynchronously is to use [pg-recorder](https://github.com/diogob/pg-recorder) with some custom stored procedures.
For more options on notification processing check the [PostgREST documentation on the topic](https://postgrest.com/en/v4.3/intro.html#external-notification).

## Sending messages to the browser

To send a message to a particular channel on the browser one should notify the postgres-websockets listener channel and pass a JSON object containing the channel and payload such as:
```sql
SELECT pg_notify(
  'postgres-websockets-listener',
  json_build_object('channel', 'chat', 'payload', 'test')::text
);
```

Where `postgres-websockets-listener` is the database channel used by your instance of postgres-websockets and `chat` is the channel where the browser is connected (the same issued in the JWT used to connect).

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/51b906e726214165a786bc5bcd544f37)](https://www.codacy.com/app/diogob/postgrest-ws?utm_source=github.com&utm_medium=referral&utm_content=diogob/postgrest-ws&utm_campaign=badger)
# PostgREST-WS [![CircleCI](https://circleci.com/gh/diogob/postgrest-ws.svg?style=svg)](https://circleci.com/gh/diogob/postgrest-ws)

PostgREST-WS is an extension of [PostgREST](https://github.com/begriffs/postgrest) 
that adds websockets capabilites to the HTTP layer on top of [PostgreSQL](https://www.postgresql.org).

To understand the concept of mapping a database schema directly to an HTTP API watch the video and take
a look at the docs at the [PostgREST website](http://postgrest.com).

This project extends these ideas assuming that websockets can be used as conduits for database notifications
in the form of PostgreSQL's [LISTEN](https://www.postgresql.org/docs/current/static/sql-listen.html)/[NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) commands.

The server provided will do basically two things (on top of already existing PostgREST functionality):

 * Allow opening websockets with multiple channels and relay messages to the database.
   * Every message sent to the websocket becomes a **NOTIFY** using the websocket channel (given by the JWT) and the content of the message as the payload.
   * Every message sent to the database to that channel will be delivered to that websocket with the payload as data.
 * Authorize the use of channels using a [JWT](https://jwt.io) issued by another service or by another PostgREST endpoint.

## Running the server

We still do not provide pre-compiled binaries, so you will have to build the server from source.
I recommend the use of [Stack](http://docs.haskellstack.org/en/stable/README/).
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

```bash
~/.local/bin/postgrest-ws
```

the above command will display the options that you can use. For more information on parameters and how to server you can take a look at the [PostgREST documentation](http://postgrest.com/install/server/#running-the-server) on the subject since `postgrest-ws` does not change anything.

## Opening notification channels

To open a notification channel you need to specify a JWT that authorizes you to do so.
The JWT should contain a claim **channel** for the channel name and a claim **mode** that tells what operations are allowed (**r** for read, **w** for write or **rw** for both).

Other claims will be sent as a json field called `userClaims` in the notify message.
We cannot set them as database variables as we would do in other PostgREST calls because the
client reading the notification will not be sharing the same transaction as the client that generates the NOTIFY.

The content of the message is added to the notification in a field called `payload`.

To open a websocket for the channel **chat** in mode rw (read and write) we could use the JavaScript code:
```javascript
var jwt = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjaGFubmVsIjoiY2hhdCIsIm1vZGUiOiJydyJ9\
._w1IGqgRfLM6epJwy6wBWOZOeSILEpJozR64qVuFgpU',
    host = window.location.hostname,
    uri = 'ws://' + host + ':3000/' + jwt,
    ws = new WebSocket(uri);
```

Now the `ws` variable contains our websocket and anything sent to it will land on our database as a **NOTIFY**.
To send anything we can use:

```javascript
ws.send('Hi!');
```        

In the example above the message received by the database client listening will be:
```json
{"userClaims":{"mode":"rw","channel":"chat"}, "payload": "Hi!"}
```

To receive messages from the database we use a callback on the `onmessage` event like this:

```javascript
ws.onmessage = function(event){
    console.log('Message received: ' + event.data.payload);
}
```

You will find a complete example under the folder [client-example](https://github.com/diogob/postgrest-ws/tree/master/client-example).

# PostgREST-WS [![CircleCI](https://circleci.com/gh/diogob/postgrest-ws.svg?style=svg)](https://circleci.com/gh/diogob/postgrest-ws)

PostgREST-WS is a [WAI](https://hackage.haskell.org/package/wai) middleware designed for use with [PostgREST](https://github.com/begriffs/postgrest)
that adds websockets capabilites to the HTTP layer on top of [PostgreSQL](https://www.postgresql.org).

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

```bash
~/.local/bin/postgrest-ws
```

the above command will display the options that you can use.
The sample config file provided in the `sample.conf` file comes with a weak jwt secret just for testing and is used in the sample client and in the example bellow.

## Opening notification channels

To open a notification channel you need to specify a JWT that authorizes you to do so.
The JWT should contain a claim **channel** for the channel name and a claim **mode** that tells what operations are allowed (**r** for read, **w** for write or **rw** for both).

Other claims will be sent as a json field called `claims` in the notify message.
We cannot set them as database variables as we would do in other PostgREST calls because the
client reading the notification will not be sharing the same transaction as the client that generates the NOTIFY.

The content of the message is added to the notification in a field called `payload`.

To open a websocket for the channel **chat** in mode rw (read and write) we could use the JavaScript code:
```javascript
var jwt = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicnciLCJjaGFubmVsI\
joiY2hhdCJ9.aMsIxB6f-dDuJJWt6hvebrs7CvBkvLClBmQjqaeYXb0',
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
{"claims":{"mode":"rw","channel":"chat"}, "payload": "Hi!"}
```

To receive messages from the database we use a callback on the `onmessage` event like this:

```javascript
ws.onmessage = function(event){
    console.log('Message received: ' + event.data.payload);
}
```

You will find a complete example under the folder [client-example](https://github.com/diogob/postgrest-ws/tree/master/client-example).
The example uses the [sample configuration file](https://github.com/diogob/postgrest-ws/tree/master/sample.conf).

## Use as Middleware

To understand the concept of mapping a database schema directly to an HTTP API watch the video and take
a look at the docs at the [PostgREST website](http://postgrest.com).

This project extends these ideas assuming that websockets can be used as conduits for database notifications
in the form of PostgreSQL's [LISTEN](https://www.postgresql.org/docs/current/static/sql-listen.html)/[NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html) commands.

The server provided in this repo is only an example to test the middleware functionality:

 * Allow opening websockets with multiple channels and relay messages to the database.
   * Every message sent to the websocket becomes a **NOTIFY** using the websocket channel (given by the JWT) and the content of the message as the payload.
   * Every message sent to the database to that channel will be delivered to that websocket with the payload as data.
 * Authorize the use of channels using a [JWT](https://jwt.io) issued by another service or by another PostgREST endpoint.

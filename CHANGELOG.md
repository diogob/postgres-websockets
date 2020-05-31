# CHANGELOG

## Next release

- Send close connection once the JWT token expires (if channel is open with a token using the `exp` claim).
- Change the format of the timestamp sent with each message to use an integer value in nanoseconds.
- Change the name of the field with the message delivery timestamp from `message_delivered_at` to `deliveredAt`.

## 0.6.1.0

- Add capability to unset `PGWS_ROOT_PATH` to disable static file serving.

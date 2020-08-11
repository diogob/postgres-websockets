# CHANGELOG

## 0.8.0.1

- Fix compilation error due to missing version upper bound for protolude.

## 0.8.0.0

- Added support for opening multiple channels at once and also issue tokens that allow multiple channels - thanks to @jamesmstone

## 0.7.0.0

- The server `postgres-websockets` will shutdown when the listener database connection is closed, this allows for external process supervisors to restart the service on database failures.
- Middleware interface now requires a parameter `IO UTCTime` prividing a function that will produce the system time.
- Send close connection once the JWT token expires (if channel is open with a token using the `exp` claim).

## 0.6.1.0

- Add capability to unset `PGWS_ROOT_PATH` to disable static file serving.

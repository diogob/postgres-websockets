# CHANGELOG

## 0.11.2.1

- Upgrade stackage to 21.14

## 0.11.2.0

- Upgrade stackage to LTS 19.0. Recompiled with GHC 9.0.2.
- Change code so we can use aeson's LTS 19.0 version (2.0.3.0).

## 0.11.1.0

- Add TLS native connection so we can have `wss://` connections without using any sort of proxy `PGWS_CERTIFICATE_FILE` and `PGWS_KEY_FILE` should be used to set the the TLS certificate and enable secure connections.

## 0.11.0.0

- Add `PGWS_CHECK_LISTENER_INTERVAL` to configure interval to check database listener connection and respawn listener in case the connection is not found.

## 0.10.0.1

- Upgrade stackage to LTS 16.11. Recompiled with GHC 8.8.3.
- Upgrade Alpine base docker image to Alpine 3.12

## 0.10.0.0

- Add `PGWS_META_CHANNEL` to configure optional metadata channel to send events from the server. Initially the oply event is `ConnectionOpen`.
- Add property `event` to message JSON. Two possible values so far: `ConnectionOpen` and `WebsocketMessage`.
- Breaking change: the property `channel` is not appended to claims anymore. If `channel` is in the original token claims it will still be present.

## 0.9.0.0

- Add @filename semantics to PGWS_DB_URI configiration variable to allow secret management to use a file instead of an environment variable.
- Add `PGWS_RETRIES` to limit the amount of times the server tries to open a database connection upon startup (defaults to 5). This breaks backward compatibility if you rely on the behaviour of the server to try infitite times.

## 0.8.0.1

- Fix compilation error due to missing version upper bound for protolude.

## 0.8.0.0

- Added support for opening multiple channels at once and also issue tokens that allow multiple channels - thanks to @jamesmstone

## 0.7.0.0

- The server `postgres-websockets` will shutdown when the listener database connection is closed, this allows for external process supervisors to restart the service on database failures.
- Middleware interface now requires a parameter `IO UTCTime` providing a function that will produce the system time.
- Send close connection once the JWT token expires (if channel is open with a token using the `exp` claim).

## 0.6.1.0

- Add capability to unset `PGWS_ROOT_PATH` to disable static file serving.

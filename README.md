# miniterl

A Telex client for Erlang.

## API

```
-type url() :: binary().
-type app_id() :: binary().
-type user_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type action() :: undefined | #{binary() => binary()} | [{binary(), binary()}].
-type message_id() :: binary().

-spec notify_app(url(), app_id(), title(), body()) -> {ok, message_id()} | {error, term()}.
-spec notify_app(url(), app_id(), title(), body(), action()) -> {ok, message_id()} | {error, term()}.
-spec notify_user(url(), user_id(), title(), body()) -> {ok, message_id()} | {error, term()}.
-spec notify_user(url(), user_id(), title(), body(), action()) -> {ok, message_id()} | {error, term()}.
-spec add_followup(url(), message_id(), body()) -> {ok, message_id()} | {error, term()}.
```

## Build

```
$ rebar3 compile
```

## Test

```
$ rebar3 eunit
```

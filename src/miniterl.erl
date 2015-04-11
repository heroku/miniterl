-module(miniterl).

%% API exports
-export([add_followup/3,
         notify_app/4,
         notify_app/5,
         notify_user/4,
         notify_user/5]).

-type url() :: binary().
-type app_id() :: binary().
-type user_id() :: binary().
-type title() :: binary().
-type body() :: binary().
-type action() :: undefined | #{binary() => binary()} | [{binary(), binary()}].
-type message_id() :: binary().

%%====================================================================
%% API functions
%%====================================================================

-spec notify_app(url(), app_id(), title(), body()) -> {ok, message_id()} | {error, term()}.
notify_app(URL, AppID, Title, Body) ->
    notify_app(URL, AppID, Title, Body, undefined).
-spec notify_app(url(), app_id(), title(), body(), action()) -> {ok, message_id()} | {error, term()}.
notify_app(URL, AppID, Title, Body, Action) ->
    post_message(URL, <<"app">>, AppID, Title, Body, Action).

-spec notify_user(url(), user_id(), title(), body()) -> {ok, message_id()} | {error, term()}.
notify_user(URL, UserID, Title, Body) ->
    notify_user(URL, UserID, Title, Body, undefined).
-spec notify_user(url(), user_id(), title(), body(), action()) -> {ok, message_id()} | {error, term()}.
notify_user(URL, UserID, Title, Body, Action) ->
    post_message(URL, <<"user">>, UserID, Title, Body, Action).

-spec add_followup(url(), message_id(), body()) -> {ok, message_id()} | {error, term()}.
add_followup(URL, MessageID, Body) ->
    ID = binary_to_list(MessageID),
    Path = string:join(["/producer/messages/", ID, "/followups"], ""),
    FullURL = hackney_url:make_url(URL, list_to_binary(Path), <<"">>),
    post(FullURL, [{<<"id">>, MessageID}, {<<"body">>, Body}]).

%%====================================================================
%% Internal functions
%%====================================================================

post_message(URL, Type, ID, Title, Body, Action) ->
    Message = make_message(Type, ID, Title, Body, Action),
    FullURL = hackney_url:make_url(URL, <<"/producer/messages">>, <<"">>),
    post(FullURL, Message).

post(URL, Body) ->
    Headers = [{<<"User-Agent">>, user_agent()},
               {<<"Content-Type">>, <<"application/json">>}],
    Payload = jsx:encode(Body),
    case hackney:request(post, URL, Headers, Payload, []) of
        {ok, 201, _, Client} ->
            {ok, ResponseBody} = hackney:body(Client),
            Result = jsx:decode(ResponseBody),
            {ok, proplists:get_value(<<"id">>, Result)};
        {error, Reason} ->
            {error, Reason}
    end.

make_message(Type, ID, Title, Body, undefined) ->
    [{<<"title">>, Title},
     {<<"body">>, Body},
     {<<"target">>, [{<<"type">>, Type}, {<<"id">>, ID}]}];
make_message(Type, ID, Title, Body, Action) ->
    Message = make_message(Type, ID, Title, Body, undefined),
    Message ++ [{<<"action">>, Action}].

user_agent() ->
    list_to_binary(string:join(["miniterl", version()], "/")).

version() ->
    case application:get_key(miniterl, vsn) of
        {ok, Version} -> Version;
        undefined -> "undefined"
    end.

%%====================================================================
%% Unit tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

make_message_test() ->
    Type = <<"app">>,
    ID = <<"id">>,
    Title = <<"title">>,
    Body = <<"body">>,
    Target = [{<<"type">>, Type}, {<<"id">>, ID}],
    ?assertEqual([{<<"title">>, Title},
                  {<<"body">>, Body},
                  {<<"target">>, Target}],
                 make_message(Type, ID, Title, Body, undefined)).

make_message_with_action_test() ->
    Type = <<"app">>,
    ID = <<"id">>,
    Title = <<"title">>,
    Body = <<"body">>,
    Target = [{<<"type">>, Type}, {<<"id">>, ID}],
    Action = [{<<"label">>, <<"label">>}, {<<"url">>, <<"url">>}],
    ?assertEqual([{<<"title">>, Title},
                  {<<"body">>, Body},
                  {<<"target">>, Target},
                  {<<"action">>, Action}],
                 make_message(Type, ID, Title, Body, Action)).

-endif.

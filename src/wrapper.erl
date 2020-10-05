-module(wrapper).

-export([
    get_request/1,
    post_request/2,
    delete_request/1,
    store_session/1,
    clear_session/1,
    get_session/1
]).

-include("webdriver.hrl").

get_request(Url) ->
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Url),
    Headers = [
        {"Content-Type", json:mime_type()},
        {"host", Host ++ ":" ++ integer_to_list(Port)},
        {"connection", "keep-alive"}],
    Request = {Url, Headers},
    request(get, Request, #{timeout => 10000}).

post_request(Url, Body) -> 
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Url),
    JSON = jsx:encode(Body),
    Len  = length(binary_to_list(JSON)),
    Headers = [
        {"Content-Length", integer_to_list(Len)},
        {"Content-Type", json:mime_type()},
        {"host", Host ++ ":" ++ integer_to_list(Port)},
        {"connection", "keep-alive"}],
    Request = {Url, Headers, "application/json; charset=utf-8", JSON},
    request(post, Request, #{timeout => 10000}).

delete_request(Url) -> 
    {ok, {_, _, Host, Port, _, _}} = http_uri:parse(Url),
    Headers = [
        {"Content-Type", json:mime_type()},
        {"host", Host ++ ":" ++ integer_to_list(Port)},
        {"connection", "keep-alive"}],
    Request = {Url, Headers, "application/json; charset=utf-8", ""},
    request(delete, Request, #{timeout => 10000}).

request(Method, Request, Options) -> 
    %io:format(user, "Method: ~p~nRequest. ~p~nOptions: ~p~n", [Method, Request, Options]),
    Result = httpc:request(Method, Request, [{timeout, timeout(Options)}, {autoredirect, true}], []),
    case Result of
        {ok, Response} -> 
            {_HttpResponse, _Headers, Message} = Response,
            parse_response(Message);
        {error, Reason} -> Reason
    end.

store_session(SessionData) ->
    case ets:info(?SESSION_ETS) of
        undefined -> ets:new(?SESSION_ETS, ?SESSION_ETS_OPTS);
        _ -> ok
    end,
    ets:insert(?SESSION_ETS, SessionData).

clear_session(Name) ->
    ets:delete(?SESSION_ETS, Name),
    case ets:info(?SESSION_ETS) of
        '$end_of_table' -> ets:delete(?SESSION_ETS);
        _ -> ok
    end.

get_session(Name) ->
    [{_, Driver, SessionId, _}] = ets:lookup(?SESSION_ETS, Name),
    {Driver, SessionId}.

timeout(#{timeout := TimeOut}) ->
    TimeOut.

parse_response(Json) ->
    Response = jsx:decode(list_to_binary(Json)),
    maps:get(<<"value">>, Response, {error, empty_response}).

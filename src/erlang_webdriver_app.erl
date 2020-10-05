%%%-------------------------------------------------------------------
%% @doc erlang_webdriver public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_webdriver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_webdriver_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

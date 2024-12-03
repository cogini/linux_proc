%%%-------------------------------------------------------------------
%% @doc linux_proc public API
%% @end
%%%-------------------------------------------------------------------

-module(linux_proc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    linux_proc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

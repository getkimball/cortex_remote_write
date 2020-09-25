-module(cortex_remote_write_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    cortex_remote_write_sup:start_link().

stop(_State) ->
    ok.

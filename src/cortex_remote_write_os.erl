-module(cortex_remote_write_os).
%% Module to wrap os commands to ease mocking
-include_lib("kernel/include/logger.hrl").


-export([getenv/1,
         system_time/1]).

getenv(E) ->
    os:getenv(E).

system_time(U) ->
    os:system_time(U).

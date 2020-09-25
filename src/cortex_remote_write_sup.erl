-module(cortex_remote_write_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = case application:get_env(cortex_remote_write, url) of
      undefined -> [];
      _URL -> [#{id    => cortex_remote_write_server,
                 start => {cortex_remote_write_server, start_link, []}}]
    end,

    {ok, {{one_for_one, 1, 5}, Procs}}.

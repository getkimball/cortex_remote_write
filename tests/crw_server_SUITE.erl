-module(crw_server_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("prometheus/include/prometheus_model.hrl").
-include_lib("../include/cortex_pb.hrl").


-define(MUT, cortex_remote_write_server).
-define(MECKED, [prometheus_registry,
                 prometheus_collector,
                 cortex_remote_write_http]).

all() -> [{group, test_server}].

groups() -> [{test_server, [
                aa_test_single_gauge
              ]}
            ].

init_per_testcase(_, Config) ->
    lists:foreach(fun meck:new/1, ?MECKED),
    application:set_env(cortex_remote_write, url, "URL"),
    application:set_env(cortex_remote_write, username, "USERNAME"),
    application:set_env(cortex_remote_write, password, "PASSWORD"),

    meck:expect(cortex_remote_write_http, send_write_request, ['_', '_', '_'], ok),
    Config.

end_per_testcase(_, Config) ->
    ok = stop(),
    lists:foreach(fun(Mod) ->
        ?assert(meck:validate(Mod)),
        meck:unload(Mod)
    end, ?MECKED),
    Config.

aa_test_single_gauge(Config) ->
    Value = 1,
    TimeStampMS = 100,
    Name = <<"foo">>,
    Help = <<"help">>,
    Type = 'GAUGE',
    Labels = [#'LabelPair'{name= <<"__name__">>, value=Name}],
    Metric = #'Metric'{gauge=#'Gauge'{value=Value},
                       timestamp_ms=TimeStampMS},
    Metrics = #'MetricFamily'{name=Name,
                              help=Help,
                              type=Type,
                              metric=[Metric]},

    set_metrics([Metrics]),

    ok = start(),
    run(),

    Sample = #'Sample'{value=Value, timestamp_ms=TimeStampMS},
    TimeSeries = #'TimeSeries'{labels=Labels, samples=[Sample]},
    Metadata = #'MetricMetadata'{type='GAUGE', metric_name=Name, help=Help},
    Write = #'WriteRequest'{timeseries=[TimeSeries], metadata=[Metadata]},

    assert_write_calls(1, '_', '_', Write),

    Config.


run() ->
    ?MUT:tick(),
    meck:wait(cortex_remote_write_http, send_write_request, ['_', '_', '_'], 1000).

assert_write_calls(Calls, URL, ReqOpts, WriteRequest) ->
    io:format("Calls ~p~n", [meck:history(cortex_remote_write_http)]),
    io:format("Looking for Call ~p~n", [{URL, ReqOpts, WriteRequest}]),
    ?assertEqual(Calls, meck:num_calls(cortex_remote_write_http, send_write_request, [URL, ReqOpts, WriteRequest])).


start() ->
    {ok, _Pid} = ?MUT:start_link(),
    ok.

stop() ->
    ?MUT:stop().

set_metrics(Metrics) ->
    meck:expect(prometheus_registry, collect, fun(default, RCB) ->
        RCB(default, default)
    end),

    meck:expect(prometheus_collector, collect_mf, fun(default, default, CCB) ->
        lists:foreach(CCB, Metrics)
    end).

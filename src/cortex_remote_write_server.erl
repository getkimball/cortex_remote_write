%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2020 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cortex_remote_write_server).
-include_lib("cortex_pb.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("prometheus/include/prometheus_model.hrl").

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         tick/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {default_labels=[],
                url="https://prometheus-us-central1.grafana.net/api/prom/push",
                username="",
                password=""}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, URL} = application:get_env(cortex_remote_write, url),
    {ok, Username} = application:get_env(cortex_remote_write, username),
    {ok, Password} = application:get_env(cortex_remote_write, password),
    Interval = application:get_env(cortex_remote_write, interval, 15000),
    DefaultLabelsPL = application:get_env(cortex_remote_write,
                                          default_labels,
                                          []),
    {ok, _TRef} = timer:apply_interval(Interval, ?MODULE, tick, []),

    DefaultLabels = [#'LabelPair'{name=N, value=V} || {N, V} <-DefaultLabelsPL],

    tick(),
    {ok, #state{default_labels=DefaultLabels,
                url=URL,
                username=Username,
                password=Password}}.

tick() ->
    gen_server:cast(?MODULE, tick).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(tick, State) ->
    iterate_metrics(fun handle_metric_family/4, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%

send_write_request(WriteRequest,
                   #state{url=URL,
                          username=Username,
                          password=Password}) ->
    Msg = cortex_pb:encode_msg(WriteRequest),
    {ok, ReqBody} = snappy:compress(Msg),

    ReqHeaders = [
      {<<"X-Prometheus-Remote-Write-Version">>, <<"0.1.0">>}
    ],
    ReqOpts = [
      {basic_auth, {Username, Password}}],

    {ok, Code, RespHeaders, ClientRef} = hackney:request(
      post,
      URL,
      ReqHeaders,
      ReqBody,
      ReqOpts),

    {ok, Body} = hackney:body(ClientRef),

    ?LOG_DEBUG(#{what=>"Prometheus remote write",
                 resp_body=>Body,
                 resp_body_l=>binary:bin_to_list(Body),
                 resp_headers=>RespHeaders,
                 req_headers=>ReqHeaders,
                 resp_code=>Code,
                 url=>URL}).


iterate_metrics(Callback, State) ->
    iterate_metrics(Callback, default, State).

iterate_metrics(Callback, Registry, State) ->
    RegistryCallback = fun(RCB_R, RCB_C) ->
        CollectorCallback = fun(MF) ->
            Callback(RCB_R, RCB_C, MF, State)
        end,
        prometheus_collector:collect_mf(RCB_R, RCB_C, CollectorCallback)
    end,

    prometheus_registry:collect(Registry, RegistryCallback),
    ok.


handle_metric_family(_Registry,
                     _Collector,
                     #'MetricFamily'{name=Name,
                                     help=Help,
                                     type=Type,
                                     metric=Metrics},
                     State) ->
    Metadata = [
      #'MetricMetadata'{type=prom_type_atom_to_cortex_atom(Type),
                        metric_name=Name,
                        help=Help}],
    Timestamp = os:system_time(millisecond),
    MapFun = fun(Metric) ->
        metric_to_timeseries_list(Name, Timestamp, Metric, State)
    end,
    TimeSeriesLists = lists:map(MapFun, Metrics),
    TimeSeries = lists:flatten(TimeSeriesLists),
    WriteRequest = #'WriteRequest'{
        timeseries=TimeSeries,
        metadata=Metadata},
    send_write_request(WriteRequest, State).

prom_type_atom_to_cortex_atom('UNTYPED') -> undefined;
prom_type_atom_to_cortex_atom(Type) -> Type.

prom_labels_to_cortex_labels(Name, Labels) ->
    [#'LabelPair'{name= <<"__name__">>,
                  value= Name}
      | Labels].

% TODO: pull timestamp from metric
metric_to_timeseries_list(Name, Timestamp, #'Metric'{label=Labels,
                                                gauge=#'Gauge'{value=Value}},
                          #state{default_labels=DefaultLabels}) ->
    CLabels = prom_labels_to_cortex_labels(
        Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp,
                          #'Metric'{label=Labels,
                                    untyped=#'Untyped'{value=Value}},
                          #state{default_labels=DefaultLabels}) ->
    CLabels = prom_labels_to_cortex_labels(Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp, #'Metric'{
                                label=Labels,
                                counter=#'Counter'{value=Value}},
                          #state{default_labels=DefaultLabels}) ->
    CLabels = prom_labels_to_cortex_labels(Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp,
                          #'Metric'{label=Labels,
                                    histogram=#'Histogram'{sample_count=Count,
                                                           sample_sum=Sum,
                                                           bucket=Buckets}},
                          #state{default_labels=DefaultLabels}) ->

    BucketTS = histogram_buckets_to_timeseries_list(
        Name, Timestamp, Labels, Buckets, DefaultLabels),
    Count_CLabels = prom_labels_to_cortex_labels(
        << Name/binary, <<"_count">>/binary >>, Labels ++ DefaultLabels),
    TS1 = build_timeseries(Timestamp, Count_CLabels, Count),
    Sum_CLabels = prom_labels_to_cortex_labels(
        << Name/binary, <<"_sum">>/binary >>, Labels ++ DefaultLabels),
    TS2 = build_timeseries(Timestamp, Sum_CLabels, Sum),
    [TS1| [TS2|BucketTS]];
metric_to_timeseries_list(Name,
                          Timestamp, #'Metric'{
                            label=Labels,
                            summary=#'Summary'{sample_count=Count,
                                               sample_sum=Sum}},
                          #state{default_labels=DefaultLabels}) ->
    Count_CLabels = prom_labels_to_cortex_labels(
        << Name/binary, <<"_count">>/binary >>, Labels ++ DefaultLabels),
    TS1 = build_timeseries(Timestamp, Count_CLabels, Count),
    Sum_CLabels = prom_labels_to_cortex_labels(
        << Name/binary, <<"_sum">>/binary >>, Labels ++ DefaultLabels),
    TS2 = build_timeseries(Timestamp, Sum_CLabels, Sum),
    [TS1, TS2].

histogram_buckets_to_timeseries_list(Name,
                                     Timestamp,
                                     Labels,
                                     Buckets,
                                     DefaultLabels) ->
    BucketFun = fun(#'Bucket'{cumulative_count=BCount, upper_bound=BBound}) ->
        BucketName = << Name/binary, <<"_bucket">>/binary >>,

        BucketLabel = #'LabelPair'{name="le",
                                   value=bound_to_label_value(BBound)},
        BucketLabels = prom_labels_to_cortex_labels(
                                BucketName,
                                [BucketLabel|Labels] ++ DefaultLabels),
        build_timeseries(Timestamp, BucketLabels, BCount)
    end,
    lists:map(BucketFun, Buckets).


build_timeseries(Timestamp, Labels, Value) ->
    #'TimeSeries'{
        labels = Labels,
        samples = [
          #'Sample'{
              value=Value,
              timestamp_ms=Timestamp
          }
        ]
    }.

bound_to_label_value(Bound) when is_number(Bound) ->
  io_lib:format("~p", [Bound]);
bound_to_label_value(infinity) ->
  "+Inf".

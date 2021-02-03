%%%-------------------------------------------------------------------
%%% @author $AUTHOR
%%% @copyright 2020 $OWNER
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(cortex_remote_write_server).
-include_lib("kernel/include/logger.hrl").
-include_lib("prometheus/include/prometheus_model.hrl").

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         tick/0,
         stop/0]).

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

stop() ->
    gen_server:stop(?MODULE, normal, 1000).

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

    DefaultLabels = lists:map(fun config_label_to_label_pair/1,
                              DefaultLabelsPL),

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
config_label_to_label_pair({K, {env, V}}) ->
    OSEnvVal = cortex_remote_write_os:getenv(V),
    #{name=>K, value=>OSEnvVal};
config_label_to_label_pair({K, V}) when is_list(V) ->
    #{name=>K, value=>V};
config_label_to_label_pair({K, V}) when is_binary(V) ->
    #{name=>K, value=>V}.


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
                     #{name:=Name,
                       help:=Help,
                       type:=Type,
                       metric:=Metrics},
                     State=#state{url=URL,
                                  username=Username,
                                  password=Password}) ->
    Metadata = [
      #{type=>prom_type_atom_to_cortex_atom(Type),
        metric_name=>Name,
        help=>Help}],

    Timestamp = cortex_remote_write_os:system_time(millisecond),
    MapFun = fun(Metric) ->
        metric_to_timeseries_list(Name, Timestamp, Metric, State)
    end,
    TimeSeriesLists = lists:map(MapFun, Metrics),
    TimeSeries = lists:flatten(TimeSeriesLists),
    WriteRequest = #{
        timeseries=>TimeSeries,
        metadata=>Metadata},
    ReqOpts = [{basic_auth, {Username, Password}}],
    cortex_remote_write_http:send_write_request(URL, ReqOpts, WriteRequest).

prom_type_atom_to_cortex_atom('UNTYPED') -> undefined;
prom_type_atom_to_cortex_atom(Type) -> Type.

prom_labels_to_cortex_labels(Name, Labels) ->
    [#{name=> <<"__name__">>,
                  value=> Name}
      | Labels].

% TODO: pull timestamp from metric
metric_to_timeseries_list(Name,
                          Timestamp,
                          Obj=#{gauge:=#{value:=Value}},
                          #state{default_labels=DefaultLabels}) ->
    Labels = maps:get(label, Obj, []),
    CLabels = prom_labels_to_cortex_labels(
        Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp,
                          Obj=#{label:=Labels,
                                    untyped:=#{value:=Value}},
                          #state{default_labels=DefaultLabels}) ->
    Labels = maps:get(label, Obj, []),
    CLabels = prom_labels_to_cortex_labels(Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp,
                          Obj=#{
                                counter:=#{value:=Value}},
                          #state{default_labels=DefaultLabels}) ->
    Labels = maps:get(label, Obj, []),
    CLabels = prom_labels_to_cortex_labels(Name, Labels ++ DefaultLabels),
    [build_timeseries(Timestamp, CLabels, Value)];
metric_to_timeseries_list(Name,
                          Timestamp,
                          Obj=#{histogram:=#{sample_count:=Count,
                                                           sample_sum:=Sum,
                                                           bucket:=Buckets}},
                          #state{default_labels=DefaultLabels}) ->

    Labels = maps:get(label, Obj, []),
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
                          Timestamp,
                          Obj=#{
                            summary:=#{sample_count:=Count,
                                               sample_sum:=Sum}},
                          #state{default_labels=DefaultLabels}) ->
    Labels = maps:get(label, Obj, []),
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
    BucketFun = fun(#{cumulative_count:=BCount, upper_bound:=BBound}) ->
        BucketName = << Name/binary, <<"_bucket">>/binary >>,

        BucketLabel = #{name=>"le",
                                   value=>bound_to_label_value(BBound)},
        BucketLabels = prom_labels_to_cortex_labels(
                                BucketName,
                                [BucketLabel|Labels] ++ DefaultLabels),
        build_timeseries(Timestamp, BucketLabels, BCount)
    end,
    lists:map(BucketFun, Buckets).


build_timeseries(Timestamp, Labels, Value) ->
    #{
        labels => Labels,
        samples => [
          #{
              value=>Value,
              timestamp_ms=>Timestamp
          }
        ]
    }.

bound_to_label_value(Bound) when is_number(Bound) ->
  io_lib:format("~p", [Bound]);
bound_to_label_value(infinity) ->
  "+Inf".

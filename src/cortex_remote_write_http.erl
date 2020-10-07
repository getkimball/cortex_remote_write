-module(cortex_remote_write_http).
-include_lib("kernel/include/logger.hrl").


-export([send_write_request/3]).

send_write_request(URL, ReqOpts, WriteRequest) ->
    Msg = cortex_pb:encode_msg(WriteRequest),
    {ok, ReqBody} = snappy:compress(Msg),

    ReqHeaders = [
      {<<"X-Prometheus-Remote-Write-Version">>, <<"0.1.0">>}
    ],

    Response = hackney:request(
      post,
      URL,
      ReqHeaders,
      ReqBody,
      ReqOpts),

    handle_response(Response).


handle_response({ok, Code, RespHeaders, ClientRef}) ->
    {ok, Body} = hackney:body(ClientRef),

    ?LOG_DEBUG(#{what=>"Prometheus remote write",
                 resp_body=>Body,
                 resp_body_l=>binary:bin_to_list(Body),
                 resp_headers=>RespHeaders,
                 resp_code=>Code});
handle_response({error, Reason}) ->
    ?LOG_INFO(#{what=>"Prometheus remote write error",
                 reason=>Reason}).


-module(time_tracking_rpc).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([handle_request/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Conn} =
        amqp_connection:start(#amqp_params_network{host =
                                                       application:get_env(time_tracking,
                                                                           rabbitmq_host,
                                                                           "localhost"),
                                                   port =
                                                       application:get_env(time_tracking,
                                                                           rabbitmq_port,
                                                                           5672)}),
    {ok, Chan} = amqp_connection:open_channel(Conn),
    Queue = application:get_env(time_tracking, rabbitmq_queue, <<"time_tracking_queue">>),
    amqp_channel:subscribe(Chan, #'basic.consume'{queue = Queue}, self()),
    {ok, #{channel => Chan, connection => Conn}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}},
            State = #{channel := Chan}) ->
    lager:debug("Received message: ~p", [Payload]),
    Json = jsx:decode(Payload),
    lager:debug("Decoded JSON: ~p", [Json]),
    Response = handle_request(maps:get(<<"method">>, Json), maps:get(<<"params">>, Json)),
    amqp_channel:cast(Chan,
                      #'basic.publish'{routing_key = maps:get(<<"reply_to">>, Json)},
                      #amqp_msg{payload = jsx:encode(Response)}),
    amqp_channel:cast(Chan, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_request(<<"/card/touch">>, Params) ->
    case time_tracking_validator:validate_card_touch(Params) of
        {ok, Validated} ->
            time_tracking_logic:card_touch(Validated);
        {error, Reason} ->
            #{error => Reason}
    end;
handle_request(<<"/card/assign">>, Params) ->
    case time_tracking_validator:validate_card_assign(Params) of
        {ok, Validated} ->
            time_tracking_logic:card_assign(Validated);
        {error, Reason} ->
            #{error => Reason}
    end;
handle_request(<<"/card/delete">>, Params) ->
    case time_tracking_validator:validate_card_delete(Params) of
        {ok, Validated} ->
            time_tracking_logic:card_delete(Validated);
        {error, Reason} ->
            #{error => Reason}
    end;
handle_request(_, _) ->
    #{error => <<"Unknown method">>}.

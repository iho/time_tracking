-module(time_tracking_test_client).
-export([send_request/3]).

-export([card_touch_test/0, card_assign_test/0]).

-include_lib("amqp_client/include/amqp_client.hrl").

send_request(Method, Params, ReplyQueue) ->
    % Start AMQP connection
    {ok, Conn} = amqp_connection:start(#amqp_params_network{
        host = application:get_env(time_tracking, rabbitmq_host, "localhost"),
        port = application:get_env(time_tracking, rabbitmq_port, 5672),
        username = list_to_binary(application:get_env(time_tracking, rabbitmq_user, "guest")),
        password = list_to_binary(application:get_env(time_tracking, rabbitmq_password, "guest"))
    }),
    {ok, Chan} = amqp_connection:open_channel(Conn),

    % Declare the reply queue
    #'queue.declare_ok'{queue = ReplyQueue} = amqp_channel:call(Chan, #'queue.declare'{
        queue = ReplyQueue,
        durable = false,
        auto_delete = true % Temporary queue for responses
    }),

    % Subscribe to the reply queue
    #'basic.consume_ok'{} = amqp_channel:subscribe(Chan, #'basic.consume'{queue = ReplyQueue}, self()),

    % Publish the request
    Request = #{
        <<"method">> => Method,
        <<"params">> => Params,
        <<"reply_to">> => ReplyQueue
    },
    Payload = jsx:encode(Request),
    amqp_channel:cast(Chan, #'basic.publish'{
        routing_key = list_to_binary(application:get_env(time_tracking, rabbitmq_queue, "time_tracking_queue"))
    }, #amqp_msg{props = #'P_basic'{content_type = <<"application/json">>}, payload = Payload}),

    % Wait for and handle the response
    Response = receive_response(Chan),
    amqp_connection:close(Conn),
    Response.

receive_response(Chan) ->
    receive
        {#'basic.deliver'{}, #amqp_msg{payload = Payload}} ->
            {ok, Json} = jsx:decode(Payload, [return_maps]),
            amqp_channel:close(Chan),
            {ok, Json};
        Other ->
            amqp_channel:close(Chan),
            {error, {unexpected_message, Other}}
    after 5000 -> % Timeout after 5 seconds
        amqp_channel:close(Chan),
        {error, timeout}
    end.



card_assign(#{<<"card_uid">> := CardUid, <<"user_id">> := UserId}) ->
    Params = #{<<"card_uid">> => CardUid, <<"user_id">> => UserId},
    send_request(<<"/card/assign">>, Params, <<"time_tracking_reply_queue">>).

card_assign_test() ->
    CardUid = <<"test_card_uid">>,
    UserId = 123,
    time_tracking_db:create_user_if_not_exists(123),
    case card_assign(#{<<"card_uid">> => CardUid, <<"user_id">> => UserId}) of
        {ok, Response} ->
            io:format("Card assigned successfully: ~p~n", [Response]);
        {error, Reason} ->
            io:format("Failed to assign card: ~p~n", [Reason])
    end.


card_touch(#{<<"card_uid">> := CardUid}) ->
    Params = #{<<"card_uid">> => CardUid},
    send_request(<<"/card/touch">>, Params, <<"time_tracking_reply_queue">>
).

card_touch_test() ->
    CardUid = <<"test_card_uid">>,
    case card_touch(#{<<"card_uid">> => CardUid}) of
        {ok, Response} ->
            io:format("Card touch logged successfully: ~p~n", [Response]);
        {error, Reason} ->
            io:format("Failed to log card touch: ~p~n", [Reason])
    end.
-module(time_tracking_logic_test).
-include_lib("eunit/include/eunit.hrl").

card_touch_test() ->
    meck:new(time_tracking_db, [passthrough]),
    meck:expect(time_tracking_db, get_card, fun(<<"card1">>) -> {ok, #{user_id => 1}} end),
    meck:expect(time_tracking_db, log_touch, fun(1, <<"card1">>) -> ok end),
    ?assertEqual(#{card_uid => <<"card1">>, user_id => 1}, time_tracking_logic:card_touch(#{card_uid => <<"card1">>})),
    meck:unload(time_tracking_db).
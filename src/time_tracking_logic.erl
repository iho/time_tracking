-module(time_tracking_logic).
-export([card_touch/1, card_assign/1]).

card_touch(#{card_uid := CardUid}) ->
    case time_tracking_db:get_card(CardUid) of
        {ok, #{user_id := UserId}} ->
            time_tracking_db:log_touch(UserId, CardUid),
            #{card_uid => CardUid, user_id => UserId};
        {error, not_found} ->
            #{error => <<"Card not assigned">>}
    end.

card_assign(#{card_uid := CardUid, user_id := UserId}) ->
    case time_tracking_db:assign_card(CardUid, UserId) of
        ok ->
            #{card_uid => CardUid, user_id => UserId};
        {error, Reason} ->
            #{error => Reason}
    end.

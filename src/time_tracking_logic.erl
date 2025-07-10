-module(time_tracking_logic).

-export([card_touch/1, card_assign/1, card_delete/1]).

card_touch(#{card_uid := CardUid}) ->
    case time_tracking_db:get_card(CardUid) of
        {ok, UserId} ->
            time_tracking_db:log_touch(UserId, CardUid),
            #{card_uid => CardUid, user_id => UserId};
        {error, not_found} ->
            #{error => <<"Card not assigned">>}
    end.

card_assign(#{card_uid := CardUid, user_id := UserId}) ->
    case time_tracking_db:assign_card(UserId, CardUid) of
        ok ->
            #{card_uid => CardUid, user_id => UserId};
        {error, Reason} ->
            #{error => Reason}
    end.

card_delete(#{card_uid := CardUid}) ->
    case time_tracking_db:get_card(CardUid) of
        {ok, UserId} ->
            time_tracking_db:delete_card(CardUid),
            #{card_uid => CardUid, user_id => UserId};
        {error, not_found} ->
            #{error => <<"Card not found">>}
    end.

-module(time_tracking_validator).
-export([validate_card_touch/1, validate_card_assign/1]).

validate_card_touch(#{<<"card_uid">> := CardUid}) when is_binary(CardUid) ->
    {ok, #{card_uid => CardUid}};
validate_card_touch(_) ->
    {error, <<"Missing or invalid card_uid">>}.

validate_card_assign(#{<<"card_uid">> := CardUid, <<"user_id">> := UserId}) when is_integer(UserId) ->
    case time_tracking_uuid_validation:is_valid_uuid(CardUid) of
        true -> {ok, #{card_uid => CardUid, user_id => UserId}};
        false -> {error, <<"Invalid card_uid">>}
    end;
validate_card_assign(_) ->
    {error, <<"Missing or invalid card_uid/user_id">>}.
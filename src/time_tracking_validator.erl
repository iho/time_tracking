-module(time_tracking_validator).

-export([validate_card_touch/1, validate_card_assign/1, validate_card_delete/1]).

validate_card_touch(#{<<"card_uid">> := CardUid}) ->
    case time_tracking_uuid_validation:is_valid_uuid(CardUid) of
        true ->
            {ok, #{card_uid => CardUid}};
        false ->
            {error, <<"Invalid card_uid">>}
    end;
validate_card_touch(#{<<"card_uid">> := CardUid, <<"user_id">> := UserId})
    when is_integer(UserId) ->
    case time_tracking_uuid_validation:is_valid_uuid(CardUid) of
        true ->
            {ok, #{card_uid => CardUid, user_id => UserId}};
        false ->
            {error, <<"Invalid card_uid">>}
    end;
validate_card_touch(_) ->
    {error, <<"Missing or invalid card_uid">>}.

validate_card_assign(#{<<"card_uid">> := CardUid, <<"user_id">> := UserId})
    when is_integer(UserId) ->
    case time_tracking_uuid_validation:is_valid_uuid(CardUid) of
        true ->
            {ok, #{card_uid => CardUid, user_id => UserId}};
        false ->
            {error, <<"Invalid card_uid">>}
    end;
validate_card_assign(_) ->
    {error, <<"Missing or invalid card_uid/user_id">>}.

validate_card_delete(#{<<"card_uid">> := CardUid}) ->
    case time_tracking_uuid_validation:is_valid_uuid(CardUid) of
        true ->
            {ok, #{card_uid => CardUid}};
        false ->
            {error, <<"Invalid card_uid">>}
    end;
validate_card_delete(_) ->
    {error, <<"Missing or invalid card_uid">>}.

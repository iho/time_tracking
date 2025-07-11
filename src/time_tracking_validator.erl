-module(time_tracking_validator).

-export([validate_card_touch/1, validate_card_assign/1, validate_card_delete/1]).
-export([validate_worktime_set/1]).
-export([is_valid_time/1]).
-export([binary_to_time/1]).
-export([validate_add_exclusion/1]).

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

% parse string with json datetime
validate_worktime_set(#{<<"user_id">> := UserId, <<"start_time">> := StartTime,
                                      <<"end_time">> := EndTime, <<"days">> := Days,
                                      <<"free_schedule">> := FreeSchedule}) ->
    UserIdValid = is_integer(UserId),
    StartTimeValid = time_tracking_time_validation:is_valid_simple_time(StartTime),
    EndTimeValid = time_tracking_time_validation:is_valid_simple_time(EndTime),
    DaysValid = is_list(Days),
    FreeScheduleValid = is_boolean(FreeSchedule),
    
    case {UserIdValid, StartTimeValid, EndTimeValid, DaysValid, FreeScheduleValid} of
        {true, true, true, true, true} ->
            {ok, #{user_id => UserId, start_time => binary_to_time(StartTime),
                   end_time => binary_to_time(EndTime), days => Days,
                   free_schedule => FreeSchedule}};
        _ ->
            lager:error("Invalid parameters for worktime set: ~p", [#{user_id => UserId, start_time => StartTime,
                                                                 end_time => EndTime, days => Days,
                                                                 free_schedule => FreeSchedule}]),
            {error, <<"Invalid parameters for worktime set">>}
    end;
validate_worktime_set(_) ->
    {error, <<"Missing or invalid parameters for worktime set">>}.

binary_to_time(Time) when is_binary(Time) ->
    case is_valid_time(Time) of
        true ->
            [Hour, Minute, Second] = binary:split(Time, <<":">>, [global]),
             {binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)};
        false ->
            {error, <<"Invalid time format">>}
    end.

is_valid_time(Time) when is_binary(Time) ->
    Pattern = <<"^[0-2][0-9]:[0-5][0-9]:[0-5][0-9]$">>,
    case re:run(Time, Pattern, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            lager:error("Invalid time format for ~p", [Time]),
            false
    end;
is_valid_time(Time) ->
    lager:error("Time must be a binary, got ~p", [Time]),
    false.


validate_add_exclusion(#{<<"user_id">> := UserId, <<"type_exclusion">> := TypeExclusion,
                                      <<"start_datetime">> := StartDatetime,
                                      <<"end_datetime">> := EndDatetime}) ->
    UserIdValid = is_integer(UserId),
    TypeExclusionValid = is_binary(TypeExclusion),
    StartDatetimeValid = time_tracking_time_validation:is_valid_simple_time(StartDatetime),
    EndDatetimeValid = time_tracking_time_validation:is_valid_simple_time(EndDatetime),

        case {UserIdValid, TypeExclusionValid, StartDatetimeValid, EndDatetimeValid} of
            {true, true, true, true} ->
                {ok, #{user_id => UserId, type_exclusion => TypeExclusion,
                       start_datetime => binary_to_time(StartDatetime),
                       end_datetime => binary_to_time(EndDatetime)}};
            _ ->
                lager:error("Invalid parameters for add exclusion: ~p", [#{user_id => UserId, type_exclusion => TypeExclusion,
                                                                          start_datetime => StartDatetime,
                                                                          end_datetime => EndDatetime}]),
                {error, <<"Invalid parameters for add exclusion">>}
        end;
validate_add_exclusion(_) ->
    {error, <<"Missing or invalid parameters for add exclusion">>}.

-module(time_tracking_db).

-export([start/0, get_card/1, assign_card/2, log_touch/2, create_user_if_not_exists/1,
         get_cards_by_user/1]).
-export([delete_card/1]).
-export([delete_all_cards_by_user/1]).
-export([delete_card_by_id/1]).
-export([set_worktime/5]).
-export([get_worktime_by_userid/1]).
-export([get_worktime_history_by_userid/1]).

start() ->
    pooler:start(),
    lager:info("Starting time_tracking_db..."),
    PoolConfig =
        #{name => pg_pool,
          max_count => 10,
          init_count => 5,
          start_mfa =>
              {epgsql,
               connect,
               [#{host => "localhost",
                  port => 5432,
                  database => "time_tracking",
                  username => "postgres",
                  password => "postgres"}]}},
    pooler:new_pool(PoolConfig),
    lager:info("time_tracking_db started successfully."),
    ok.

get_card(CardUid) ->
    % take a connection from the pool
    Pid = pooler:take_member(pg_pool),

    Query = "SELECT user_id FROM cards WHERE card_uid = $1",
    case epgsql:equery(Pid, Query, [CardUid]) of
        {ok, _Colums, Rows} ->
            pooler:return_member(pg_pool, Pid, ok),
            case Rows of
                [] ->
                    {error, not_found};
                [{UserId}] ->
                    {ok, UserId}
            end;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

create_user_if_not_exists(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "INSERT INTO users (user_id) VALUES ($1) ON CONFLICT DO NOTHING",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {ok, 1} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

assign_card(UserId, CardUid) ->
    Pid = pooler:take_member(pg_pool),
    Query =
        "INSERT INTO cards (card_uid, user_id) VALUES ($1, $2) ON CONFLICT "
        "(card_uid) DO UPDATE SET user_id = $2",
    case epgsql:equery(Pid, Query, [CardUid, UserId]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            {error, already_exists};
        {ok, 1} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

log_touch(UserId, CardUid) ->
    Pid = pooler:take_member(pg_pool),
    EventType = detect_in_or_out_event_type_by_time(os:timestamp()),
    lager:info("Logging touch for UserId: ~p, CardUid: ~p, EventType: ~p",
               [UserId, CardUid, EventType]),
    Query =
        "INSERT INTO work_history (user_id, card_uid, event_type) VALUES "
        "($1, $2, $3)",
    case epgsql:equery(Pid, Query, [UserId, CardUid, EventType]) of
        {ok, 0} ->
            lager:error("Failed to log touch for UserId: ~p, CardUid: ~p",
                        [UserId, CardUid]),
            pooler:return_member(pg_pool, Pid, ok),
            {error, not_logged};
        {ok, 1} ->
            lager:info("Touch logged successfully for UserId: ~p, CardUid: ~p",
                        [UserId, CardUid]),
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            lager:error("Failed to log touch for UserId: ~p, CardUid: ~p, Reason: ~p",
                        [UserId, CardUid, Reason]),
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

detect_in_or_out_event_type_by_time(Time) ->
    case Time of
        T when T >= {9, 0, 0} andalso T < {18, 0, 0} ->
            in;
        _ ->
            out
    end.

delete_card(CardUid) ->
    Pid = pooler:take_member(pg_pool),
    Query = "DELETE FROM cards WHERE card_uid = $1",
    case epgsql:equery(Pid, Query, [CardUid]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            {error, not_found};
        {ok, 1} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

get_cards_by_user(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "SELECT card_uid FROM cards WHERE user_id = $1",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, _Columns, Rows} ->
            pooler:return_member(pg_pool, Pid, ok),
            case Rows of
                [] ->
                    {error, not_found};
                _ ->
                    {ok, [CardUid || {CardUid} <- Rows]}
            end;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

delete_all_cards_by_user(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "DELETE FROM cards WHERE user_id = $1",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            {error, not_found};
        {ok, _} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

delete_card_by_id(CardUid) ->
    Pid = pooler:take_member(pg_pool),
    Query = "DELETE FROM cards WHERE card_uid = $1 RETURNING user_id",
    case epgsql:equery(Pid, Query, [CardUid]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            {error, not_found};
        {ok, [{UserId}]} ->
            pooler:return_member(pg_pool, Pid, ok),
            {ok, UserId};
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

set_worktime(UserId, StartTime, EndTime, Days, FreeSchedule) ->
    Pid = pooler:take_member(pg_pool),
    Query =
        "INSERT INTO work_schedules (user_id, start_time, end_time, days, free_schedule) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (user_id) DO UPDATE SET start_time = $2, end_time = $3, days = $4, free_schedule = $5",
    case epgsql:equery(Pid, Query, [UserId, StartTime, EndTime, Days, FreeSchedule]) of
        {ok, 0} ->
            pooler:return_member(pg_pool, Pid, ok),
            {error, not_logged};
        {ok, 1} ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

get_worktime_by_userid(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "SELECT start_time, end_time, days, free_schedule FROM work_schedules WHERE user_id = $1",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, _Columns, Rows} ->
            pooler:return_member(pg_pool, Pid, ok),
            case Rows of
                [] ->
                    {error, not_found};
                [{StartTime, EndTime, Days, FreeSchedule}] ->
                    {ok, #{start_time => StartTime,
                           end_time => EndTime,
                           days => Days,
                           free_schedule => FreeSchedule}}
            end;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.


get_worktime_history_by_userid(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "SELECT card_uid, touch_time, event_type FROM work_history WHERE user_id = $1 ORDER BY touch_time DESC",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, _Columns, Rows} ->
            pooler:return_member(pg_pool, Pid, ok),
            case Rows of
                [] ->
                    {error, not_found};
                Rows ->
                    History = [#{card_uid => CardUid,
                                 touch_time => TouchTime,
                                 event_type => EventType,
                                 user_id => UserId} || {CardUid, TouchTime, EventType} <- Rows],
                    {ok, History}
            end;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.
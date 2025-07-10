-module(time_tracking_db).

-export([start/0, get_card/1, assign_card/2, log_touch/2, create_user_if_not_exists/1]).

start() ->
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
                [] -> {error, not_found};
                [{UserId}] -> {ok, UserId}
            end;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end. 

create_user_if_not_exists(UserId) ->
    Pid = pooler:take_member(pg_pool),
    Query = "INSERT INTO users (user_id) VALUES ($1) ON CONFLICT DO NOTHING",
    case epgsql:equery(Pid, Query, [UserId]) of
        {ok, 0}  ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {ok, 1}  ->
            pooler:return_member(pg_pool, Pid, ok),
            ok;
        {error, Reason} ->
            pooler:return_member(pg_pool, Pid, fail),
            {error, Reason}
    end.

assign_card(UserId, CardUid) ->
    Pid = pooler:take_member(pg_pool),
    Query = "INSERT INTO cards (card_uid, user_id) VALUES ($1, $2) ON CONFLICT (card_uid) DO UPDATE SET user_id = $2",
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
    lager:info("Logging touch for UserId: ~p, CardUid: ~p, EventType: ~p", [UserId, CardUid, EventType]),
    Query = "INSERT INTO work_history (user_id, card_uid, event_type) VALUES ($1, $2, $3)",
    case epgsql:equery(Pid, Query, [UserId, CardUid, EventType]) of
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

detect_in_or_out_event_type_by_time(Time) ->
    case Time of
        T when T >= {9,0,0} andalso T < {18,0,0} -> in;
        _ -> out
    end.


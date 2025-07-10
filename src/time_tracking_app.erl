-module(time_tracking_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    time_tracking_sup:start_link().

stop(_State) ->
    ok.

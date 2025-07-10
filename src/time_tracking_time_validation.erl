-module(time_tracking_time_validation).

-export([is_valid_simple_time/1]).


% Validate simple time format like "09:00:00"
is_valid_simple_time(TimeStr) ->
    try
        case re:run(TimeStr, "^\\d{2}:\\d{2}:\\d{2}$", [{capture, none}]) of
            match -> 
                % Additional validation to check if time values are valid
                [HourStr, MinuteStr, SecondStr] = binary:split(TimeStr, <<":">>, [global]),
                Hour = binary_to_integer(HourStr),
                Minute = binary_to_integer(MinuteStr),
                Second = binary_to_integer(SecondStr),
                Hour >= 0 andalso Hour < 24 andalso 
                Minute >= 0 andalso Minute < 60 andalso 
                Second >= 0 andalso Second < 60;
            nomatch -> false
        end
    catch
        _:_ -> false
    end.
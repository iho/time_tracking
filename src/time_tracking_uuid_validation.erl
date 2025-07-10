-module(time_tracking_uuid_validation).
-export([is_valid_uuid/1]).

% Validates a UUID string (binary) in the format xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx
is_valid_uuid(Uuid) when is_binary(Uuid) ->
    case byte_size(Uuid) of
        36 ->
            % Check format using regular expression
            Pattern = <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$">>,
            case re:run(Uuid, Pattern, [{capture, none}]) of
                match ->
                    true;
                nomatch ->
                    false
            end;
        _ ->
            false
    end;
is_valid_uuid(_) ->
    false.
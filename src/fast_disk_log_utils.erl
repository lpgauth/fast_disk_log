-module(fast_disk_log_utils).
-include("fast_disk_log.hrl").

-export([
    env/2,
    error_msg/2,
    lookup/3,
    random/1
]).

%% public
env(Key, Default) ->
    application:get_env(?APP, Key, Default).

error_msg(Format, Data) ->
    error_logger:error_msg(Format, Data).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

random(N) ->
    erlang:phash2({self(), os:timestamp()}, N).

-module(fast_disk_log_utils).
-include("fast_disk_log.hrl").

-export([
    env/2,
    lookup/3
]).

%% public
env(Key, Default) ->
    application:get_env(?APP, Key, Default).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.
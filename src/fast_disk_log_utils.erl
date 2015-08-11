-module(fast_disk_log_utils).
-include("fast_disk_log.hrl").

-export([
    env/2,
    error_msg/2,
    lookup/3,
    random/1,
    warning_msg/2
]).

%% public
-spec env(atom(), term()) -> term().

env(Key, Default) ->
    application:get_env(?APP, Key, Default).

-spec error_msg(string(), [term()]) -> ok.

error_msg(Format, Data) ->
    error_logger:error_msg(Format, Data).

-spec lookup(atom(), [{atom(), term()}], term()) -> term().

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {_, Value} -> Value
    end.

-spec random(pos_integer()) -> pos_integer().

random(N) ->
    erlang:phash2({self(), os:timestamp()}, N).

-spec warning_msg(string(), [term()]) -> ok.

warning_msg(Format, Data) ->
    error_logger:error_msg(Format, Data).

-module(fast_disk_log).
-include("fast_disk_log.hrl").

-export([
    open/2,
    open/3,
    log/2
]).

%% public
-spec open(atom(), binary()) -> ok | {error, name_already_open}.
open(Name, File) ->
    open(Name, File, []).

-spec open(atom(), binary(), open_options()) -> ok | {error, name_already_open}.

open(Name, File, Opts) ->
    PoolSize = ?LOOKUP(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    case ets:insert_new(?TABLE_NAME, {Name, PoolSize}) of
        false -> {error, name_already_open};
        true ->
            case start_writer_child(Name, File) of
                {ok, _} ->
                    ok = start_buffer_children(Name, PoolSize),
                    ok;
                {error, _} ->
                    % TODO
                    ok
            end
    end.

-spec log(atom(), binary()) -> ok | {error, no_such_log}.

log(Name, Bin) ->
    case ets_lookup_element(Name) of
        undefined ->
            {error, no_such_log};
        PoolSize ->
            Buffer = random_buffer(Name, PoolSize),
            Buffer ! {log, Bin},
            ok
    end.

%% private
buffer_name(Name, N) ->
    list_to_atom(atom_to_list(Name) ++ "_buffer_" ++ integer_to_list(N)).

writer_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_writer").

start_writer_child(Name, File) ->
    Writer = writer_name(Name),
    Spec = ?CHILD(Writer, fast_disk_log_writer, [Writer, File]),
    supervisor:start_child(fast_disk_log_sup, Spec).

start_buffer_children(_Name, 0) ->
    ok;
start_buffer_children(Name, N) ->
    start_buffer_child(Name, N),
    start_buffer_children(Name, N - 1).

start_buffer_child(Name, N) ->
    Buffer = buffer_name(Name, N),
    Writer = writer_name(Name),
    Spec = ?CHILD(Buffer, fast_disk_log_buffer, [Buffer, Writer]),
    supervisor:start_child(fast_disk_log_sup, Spec).

ets_lookup_element(Key) ->
    try ets:lookup_element(?TABLE_NAME, Key, 2)
    catch
        error:badarg -> undefined
    end.

random_buffer(Name, PoolSize) ->
    Rand = random(PoolSize) + 1,
    list_to_existing_atom(atom_to_list(Name) ++ "_buffer_" ++ integer_to_list(Rand)).

random(N) ->
    erlang:phash2({self(), os:timestamp()}, N).

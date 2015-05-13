-module(fast_disk_log).
-include("fast_disk_log.hrl").

-export([
    close/1,
    log/2,
    open/2,
    open/3,
    sync/1
]).

%% public
-spec close(name()) -> ok | {error, no_such_log}.

close(Name) ->
    case lookup_element(Name) of
        undefined -> {error, no_such_log};
        PoolSize ->
            case ets:select_delete(?TABLE_NAME, ?MATCH_SPEC(Name)) of
                0 -> {error, no_such_log};
                1 ->
                    msg_buffer_workers(Name, PoolSize, close),
                    msg_writter_worker(Name, {close, PoolSize}),
                    ok
            end
    end.

-spec open(name(), filename()) -> ok | {error, name_already_open}.

open(Name, Filename) ->
    open(Name, Filename, []).

-spec open(name(), filename(), open_options()) -> ok | {error, name_already_open}.

open(Name, Filename, Opts) ->
    PoolSize = ?LOOKUP(pool_size, Opts, ?DEFAULT_POOL_SIZE),
    case ets:insert_new(?TABLE_NAME, {Name, PoolSize}) of
        false -> {error, name_already_open};
        true ->
            delete_buffer_children(Name, PoolSize),
            delete_writer_child(Name),
            start_writer_child(Name, Filename),
            start_buffer_children(Name, PoolSize),
            ok
    end.

-spec log(name(), binary()) -> ok | {error, no_such_log}.

log(Name, Bin) ->
    case lookup_element(Name) of
        undefined -> {error, no_such_log};
        PoolSize ->
            N = fast_disk_log_utils:random(PoolSize) + 1,
            msg_buffer_worker(Name, N, {log, Bin}),
            ok
    end.

-spec sync(name()) -> ok | {error, no_such_log}.

sync(Name) ->
    case lookup_element(Name) of
        undefined -> {error, no_such_log};
        PoolSize ->
            msg_buffer_workers(Name, PoolSize, sync),
            ok
    end.

%% private
buffer_worker(Name, N) ->
    Buffer = <<Name/binary, "_buffer_", (integer_to_binary(N))/binary>>,
    binary_to_atom(Buffer, latin1).

delete_buffer_child(Name, N) ->
    Buffer = buffer_worker(Name, N),
    supervisor:delete_child(?SUPERVISOR, Buffer).

delete_buffer_children(Name, PoolSize) ->
    [delete_buffer_child(Name, N) || N <- lists:seq(1, PoolSize)].

delete_writer_child(Name) ->
    Writer = writer_worker(Name),
    supervisor:delete_child(?SUPERVISOR, Writer).

lookup_element(Key) ->
    try ets:lookup_element(?TABLE_NAME, Key, 2)
    catch
        error:badarg -> undefined
    end.

msg_buffer_worker(Name, N, Msg) ->
    buffer_worker(Name, N) ! Msg.

msg_buffer_workers(Name, PoolSize, Msg) ->
    [msg_buffer_worker(Name, N, Msg) || N <- lists:seq(1, PoolSize)].

msg_writter_worker(Name, Msg) ->
    writer_worker(Name) ! Msg.

start_buffer_children(_Name, 0) ->
    ok;
start_buffer_children(Name, N) ->
    start_buffer_child(Name, N),
    start_buffer_children(Name, N - 1).

start_buffer_child(Name, N) ->
    Buffer = buffer_worker(Name, N),
    Writer = writer_worker(Name),
    Spec = ?CHILD(Buffer, fast_disk_log_buffer, [Buffer, Writer]),
    {ok, _Pid} = supervisor:start_child(?SUPERVISOR, Spec).

start_writer_child(Name, Filename) ->
    Writer = writer_worker(Name),
    Spec = ?CHILD(Writer, fast_disk_log_writer, [Writer, Filename]),
    {ok, _Pid} = supervisor:start_child(?SUPERVISOR, Spec).

writer_worker(Name) ->
    Writer = <<Name/binary, "_writer">>,
    binary_to_atom(Writer, latin1).

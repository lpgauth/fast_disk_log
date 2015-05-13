-module(fast_disk_log_writer).
-include("fast_disk_log.hrl").

-export([
    init/3,
    start_link/2
]).

-record(state, {
    fd,
    name
}).

%% public
-spec init(pid(), atom(), filename()) -> no_return().

init(Parent, Name, Filename) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),

    case file:open(Filename, [append, raw]) of
        {ok, Fd} ->
            loop(#state {
                name = Name,
                fd = Fd
            });
        {error, _Reason} ->
            % TODO
            ok
    end.

-spec start_link(atom(), filename()) -> {ok, pid()}.

start_link(Name, Filename) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Filename]).

%% private
close_wait(0) ->
    [];
close_wait(N) ->
    receive
        {write, Buffer} ->
            [Buffer | close_wait(N - 1)]
    after ?CLOSE_TIMEOUT ->
        []
    end.

handle_msg({close, PoolSize}, #state {
        fd = Fd,
        name = Name
    }) ->

    Buffer = lists:reverse(close_wait(PoolSize)),
    case file:write(Fd, Buffer) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_MSG("failed to write: ~p~n", [Reason])
    end,
    case file:close(Fd) of
        ok -> ok;
        {error, Reason2} ->
            ?ERROR_MSG("failed to close: ~p~n", [Reason2])
    end,
    supervisor:terminate_child(?SUPERVISOR, Name);
handle_msg({write, Buffer}, #state {fd = Fd} = State) ->
    case file:write(Fd, Buffer) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_MSG("failed to write: ~p~n", [Reason])
    end,
    {ok, State};
handle_msg(Msg, State) ->
    ?WARNING_MSG("unknown msg: ~p~n", [Msg]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

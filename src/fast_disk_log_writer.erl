-module(fast_disk_log_writer).
-include("fast_disk_log.hrl").

-export([
    init/3,
    start_link/2
]).

-record(state, {
    fd
}).

%% public
-spec init(pid(), atom(), filename()) -> no_return().

init(Parent, Name, Filename) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),

    case file:open(Filename, [append, raw]) of
        {ok, Fd} ->
            loop(#state {fd = Fd});
        {error, _Reason} ->
            % TODO
            ok
    end.

-spec start_link(atom(), filename()) -> {ok, pid()}.

start_link(Name, Filename) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Filename]).

%% private
handle_msg({write, Bin}, #state {fd = Fd} = State) ->
    Timestamp = os:timestamp(),
    case file:write(Fd, Bin) of
        ok ->
            Diff = timer:now_diff(os:timestamp(), Timestamp),
            case Diff of
                X when X > 10000 ->
                    io:format("time: ~p~n", [Diff]);
                _ -> ok
            end,
            ok;
        {error, _Reason} ->
            % TODO
            ok
    end,
    {ok, State};
handle_msg(Msg, State) ->
    io:format("unknown msg: ~p~n", [Msg]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

-module(fast_disk_log_writer).
-include("fast_disk_log.hrl").

-export([
    init/5,
    start_link/4
]).

-record(state, {
    fd,
    logger,
    name,
    timer_delay,
    timer_ref,
    write_count = 0,
    zstream
}).

%% public
-spec init(pid(), atom(), name(), filename(), open_options()) -> ok | no_return().

init(Parent, Name, Logger, Filename, Opts) ->
    case file:open(Filename, [append, raw]) of
        {ok, Fd} ->
            register(Name, self()),
            proc_lib:init_ack(Parent, {ok, self()}),

            State = #state {
                name = Name,
                fd = Fd,
                logger = Logger
            },

            State2 = case ?LOOKUP(auto_close, Opts, ?DEFAULT_AUTO_CLOSE) of
                true ->
                    AutoCloseDelay = ?ENV(max_delay, ?DEFAULT_MAX_DELAY) * 2,
                    State#state {
                        timer_delay = AutoCloseDelay,
                        timer_ref = new_timer(AutoCloseDelay, auto_close)
                    };
                false ->
                    State
            end,

            case ?LOOKUP(compression, Opts, ?DEFAULT_COMPRESSION) of
                true ->
                    loop(State2#state {
                        zstream = zlib_open()
                    });
                false ->
                    loop(State2)
            end;
        {error, Reason} ->
            ?ERROR_MSG("failed to open file: ~p ~p~n", [Reason, Filename]),
            ok
    end.

-spec start_link(atom(), name(), filename(), open_options()) -> {ok, pid()}.

start_link(Name, Logger, Filename, Opts) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Logger, Filename, Opts]).

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

handle_msg(auto_close, #state {
        write_count = 0,
        logger = Logger
    } = State) ->

    spawn(fun () -> fast_disk_log:close(Logger) end),
    {ok, State};
handle_msg(auto_close, #state {timer_delay = TimerDelay} = State) ->
    {ok, State#state {
        timer_ref = new_timer(TimerDelay, auto_close),
        write_count = 0
    }};
handle_msg({close, PoolSize, Pid}, #state {
        fd = Fd,
        name = Name,
        zstream = Zstream
    }) ->

    Buffer = lists:reverse(close_wait(PoolSize)),
    Buffer2 = zlib_close(Zstream, Buffer),

    case file:write(Fd, Buffer2) of
        ok -> ok;
        {error, Reason} ->
            ?ERROR_MSG("failed to write: ~p~n", [Reason])
    end,
    case file:sync(Fd) of
        ok -> ok;
        {error, Reason2} ->
            ?ERROR_MSG("failed to sync: ~p~n", [Reason2])
    end,
    case file:close(Fd) of
        ok -> ok;
        {error, Reason3} ->
            ?ERROR_MSG("failed to close: ~p~n", [Reason3])
    end,
    Pid ! {fast_disk_log, {closed, Name}},
    ok = supervisor:terminate_child(?SUPERVISOR, Name);
handle_msg({write, Buffer}, #state {
        fd = Fd,
        write_count = WriteCount,
        zstream = Zstream
    } = State) ->

    Buffer2 = zlib_compress(Zstream, Buffer),

    case file:write(Fd, Buffer2) of
        ok ->
            {ok, State#state {
                write_count = WriteCount + 1
            }};
        {error, Reason} ->
            ?ERROR_MSG("failed to write: ~p~n", [Reason]),
            {ok, State}
    end.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

new_timer(Delay, Msg) ->
    erlang:send_after(Delay, self(), Msg).

zlib_compress(undefined, Data) ->
    Data;
zlib_compress(Zstream, Data) ->
    zlib:deflate(Zstream, Data).

zlib_close(undefined, Data) ->
    Data;
zlib_close(Zstream, Data) ->
    Data2 = zlib:deflate(Zstream, Data, finish),
    ok = zlib:deflateEnd(Zstream),
    ok = zlib:close(Zstream),
    Data2.

zlib_open() ->
    Zstream = zlib:open(),
    zlib:deflateInit(Zstream, default, deflated, 31, 8, default),
    Zstream.

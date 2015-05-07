-module(fast_disk_log_buffer).
-include("fast_disk_log.hrl").

-export([
    init/3,
    start_link/2
]).

-record(state, {
    buffer = [],
    buffer_size = 0,
    max_buffer_size,
    max_delay,
    timer_ref,
    writer
}).

%% public
-spec init(pid(), atom(), atom()) -> no_return().

init(Parent, Name, Writer) ->
    register(Name, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    MaxBufferSize = ?ENV(buffer_size, ?DEFAULT_MAX_BUFFER_SIZE),
    MaxDelay = ?ENV(delay, ?DEFAULT_MAX_DELAY),
    loop(#state {
        max_buffer_size = MaxBufferSize,
        max_delay = MaxDelay,
        timer_ref = new_timer(MaxDelay),
        writer = Writer
    }).

-spec start_link(atom(), atom()) -> {ok, pid()}.

start_link(Name, Writer) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Writer]).

%% private
handle_msg({log, Bin}, #state {
        buffer = Buffer,
        buffer_size = BufferSize,
        max_buffer_size = MaxBufferSize,
        writer = Writer
    } = State) ->

    NewBuffer = [Bin | Buffer],
    case BufferSize + size(Bin) of
        X when X >= MaxBufferSize ->
            write(Writer, NewBuffer),

            {ok, State#state {
                buffer = [],
                buffer_size = 0
            }};
        NewBufferSize ->
            {ok, State#state {
                buffer = NewBuffer,
                buffer_size = NewBufferSize
            }}
    end;
handle_msg(timeout, #state {
        buffer = Buffer,
        max_delay = MaxDelay,
        writer = Writer
    } = State) ->

    write(Writer, Buffer),

    {ok, State#state {
        buffer = [],
        buffer_size = 0,
        timer_ref = new_timer(MaxDelay)
    }};
handle_msg(Msg, State) ->
    io:format("unknown msg: ~p~n", [Msg]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

new_timer(Time) ->
    erlang:send_after(Time, self(), timeout).

write(_Writer, []) ->
    ok;
write(Writer, Buffer) ->
    Writer ! {write, lists:reverse(Buffer)}.


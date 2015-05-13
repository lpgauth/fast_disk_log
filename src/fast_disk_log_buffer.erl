-module(fast_disk_log_buffer).
-include("fast_disk_log.hrl").

-export([
    init/3,
    start_link/2
]).

-record(state, {
    buffer = [],
    buffer_size = 0,
    name,
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
        name = Name,
        max_buffer_size = MaxBufferSize,
        max_delay = MaxDelay,
        timer_ref = new_timer(MaxDelay),
        writer = Writer
    }).

-spec start_link(atom(), atom()) -> {ok, pid()}.

start_link(Name, Writer) ->
    proc_lib:start_link(?MODULE, init, [self(), Name, Writer]).

%% private
handle_msg(close, #state {
        buffer = Buffer,
        name = Name,
        timer_ref = TimerRef,
        writer = Writer
    }) ->

    Writer ! {write, lists:reverse(Buffer)},
    erlang:cancel_timer(TimerRef),
    ok = supervisor:terminate_child(?SUPERVISOR, Name);
handle_msg(sync, #state {
        buffer = Buffer,
        writer = Writer
    } = State) ->

    write(Writer, Buffer),
    reset_buffer(State);
handle_msg(timeout, #state {
        buffer = Buffer,
        writer = Writer
    } = State) ->

    write(Writer, Buffer),
    reset_buffer(State);
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
            reset_buffer(State);
        NewBufferSize ->
            {ok, State#state {
                buffer = NewBuffer,
                buffer_size = NewBufferSize
            }}
    end;
handle_msg(Msg, State) ->
    ?WARNING_MSG("unknown msg: ~p~n", [Msg]),
    {ok, State}.

loop(State) ->
    receive Msg ->
        {ok, State2} = handle_msg(Msg, State),
        loop(State2)
    end.

new_timer(Time) ->
    erlang:send_after(Time, self(), timeout).

reset_buffer(#state {
        max_delay = MaxDelay,
        timer_ref = TimerRef
    } = State) ->

    erlang:cancel_timer(TimerRef),
    {ok, State#state {
        buffer = [],
        buffer_size = 0,
        timer_ref = new_timer(MaxDelay)
    }}.

write(_Writer, []) ->
    ok;
write(Writer, Buffer) ->
    Writer ! {write, lists:reverse(Buffer)}.

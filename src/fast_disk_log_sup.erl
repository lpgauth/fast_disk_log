-module(fast_disk_log_sup).
-include("fast_disk_log.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).
-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
    init_table(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
-spec init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

init([]) ->
    {ok, {{one_for_one, 5, 10}, []}}.

%% private
init_table() ->
    Opts = [named_table, public, set, {read_concurrency, true}],
    ?TABLE_NAME = ets:new(?TABLE_NAME, Opts).

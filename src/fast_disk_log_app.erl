-module(fast_disk_log_app).
-include("fast_disk_log.hrl").

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
-spec start() -> ok.
start() ->
    application:start(?APP).

%% application callbacks
start(_StartType, _StartArgs) ->
    fast_disk_log_sup:start_link().

stop(_State) ->
    ok.

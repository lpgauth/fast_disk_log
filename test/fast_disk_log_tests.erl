-module(fast_disk_log_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("fast_disk_log/include/fast_disk_log.hrl").

-compile(export_all).

-define(LOGGER_NAME, <<"test_logger">>).
-define(LOGGER_PATH, <<"./test.log">>).
-define(T, fun (Test) -> test(Test) end).

%% runners
fast_disk_logger_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        ?T(test_open)
    ]}.

%% tests
test_open() ->
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(?APP),
    error_logger:tty(true).

random() ->
    crypto:rand_bytes(24).

receive_loop(0) -> [];
receive_loop(N) ->
    receive
        {response, X} ->
            [X | receive_loop(N - 1)]
    end.

setup() ->
    setup([]).

setup(KeyVals) ->
    error_logger:tty(false),
    application:load(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- KeyVals],
    fast_disk_log_app:start(),
    error_logger:tty(true).

test(Test) ->
    {atom_to_list(Test), ?MODULE, Test}.
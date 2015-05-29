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
        ?T(test_open_close)
    ]}.

%% tests
test_open_close() ->
    {error, no_such_log} = fast_disk_log:close(?LOGGER_NAME),
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    {error, name_already_open} = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    ok = fast_disk_log:close(?LOGGER_NAME),
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(?APP),
    error_logger:tty(true).

random() ->
    crypto:rand_bytes(24).

read_loop(N) ->
    {ok, File} = file:open(?LOGGER_PATH, [read]),
    read_loop(N, File).

read_loop(0, _File) ->
    [];
read_loop(N, File) ->
    case file:read_line(File) of
        {ok, Line} ->
            [Line | read_loop(N - 1)];
        eof ->
            timer:sleep(500),
            read_loop(N)
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

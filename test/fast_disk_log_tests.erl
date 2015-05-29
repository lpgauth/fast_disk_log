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
        ?T(test_open_close),
        ?T(test_log_sync)
    ]}.

%% tests
test_open_close() ->
    {error, no_such_log} = fast_disk_log:close(?LOGGER_NAME),
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    {error, name_already_open} = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    ok = fast_disk_log:close(?LOGGER_NAME),
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    ok = fast_disk_log:close(?LOGGER_NAME).

test_log_sync() ->
    log_delete(),
    {error, no_such_log} = fast_disk_log:log(?LOGGER_NAME, <<"test">>),
    {error, no_such_log} = fast_disk_log:sync(?LOGGER_NAME),
    ok = fast_disk_log:open(?LOGGER_NAME, ?LOGGER_PATH),
    ok = fast_disk_log:log(?LOGGER_NAME, <<"test">>),
    ok = fast_disk_log:sync(?LOGGER_NAME),
    ?assertEqual(["test"], log_read(1)),
    ok = fast_disk_log:close(?LOGGER_NAME).

%% utils
cleanup() ->
    error_logger:tty(false),
    application:stop(?APP),
    error_logger:tty(true).

log_delete() ->
    file:delete(?LOGGER_PATH).

log_read(N) ->
    {ok, File} = file:open(?LOGGER_PATH, [read]),
    read_loop(N, File).

read_loop(0, _File) ->
    [];
read_loop(N, File) ->
    case file:read_line(File) of
        {ok, Line} ->
            [Line | read_loop(N - 1, File)];
        eof ->
            timer:sleep(500),
            read_loop(N, File)
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

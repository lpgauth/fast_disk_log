%% macros
-define(APP, fast_disk_log).
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(CLOSE_TIMEOUT, 5000).
-define(ENV(Key, Default), fast_disk_log_utils:env(Key, Default)).
-define(ERROR_MSG(Format), ?ERROR_MSG(Format, [])).
-define(ERROR_MSG(Format, Data), fast_disk_log_utils:error_msg(Format, Data)).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), fast_disk_log_utils:lookup(Key, List, Default)).
-define(MATCH_SPEC(Name), [{{Name, '_'}, [], [true]}]).
-define(SUPERVISOR, fast_disk_log_sup).
-define(TABLE_NAME, fast_disk_log_manager).

%% defaults
-define(DEFAULT_MAX_BUFFER_SIZE, 8388608).
-define(DEFAULT_MAX_DELAY, 2000).
-define(DEFAULT_POOL_SIZE, 4).

%% types
-type filename() :: binary(). % TODO: fixme
-type open_option() :: pool_size.
-type open_options() :: [open_option()].
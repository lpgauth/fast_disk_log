%% macros
-define(APP, fast_disk_log).
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(CLOSE_TIMEOUT, 5000).
-define(ENV(Key, Default), fast_disk_log_utils:env(Key, Default)).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), fast_disk_log_utils:lookup(Key, List, Default)).
-define(MATCH_SPEC(Name), [{{Name, '_'}, [], [true]}]).
-define(SUPERVISOR, fast_disk_log_sup).
-define(TABLE_NAME, fast_disk_log_manager).

%% defaults
-define(DEFAULT_MAX_SIZE, 8000000).
-define(DEFAULT_MAX_DELAY, 2000).
-define(DEFAULT_POOL_SIZE, 4).

%% types
-type filename() :: file:name_all().
-type name() :: binary().
-type open_option() :: {pool_size, pos_integer()}.
-type open_options() :: [open_option()].

%% logging
-define(ERROR_MSG(Format), ?ERROR_MSG(Format, [])).
-define(ERROR_MSG(Format, Data), fast_disk_log_utils:error_msg(Format, Data)).
-define(WARNING_MSG(Format), ?WARNING_MSG(Format, [])).
-define(WARNING_MSG(Format, Data), fast_disk_log_utils:warning_msg(Format, Data)).
%% macros
-define(APP, fast_disk_log).
-define(CHILD(Name, Mod, Args), {Name, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
-define(ENV(Key, Default), fast_disk_log_utils:env(Key, Default)).
-define(LOOKUP(Key, List), ?LOOKUP(Key, List, undefined)).
-define(LOOKUP(Key, List, Default), fast_disk_log_utils:lookup(Key, List, Default)).
-define(TABLE_NAME, fast_disk_log_manager).

%% defaults
-define(DEFAULT_MAX_BUFFER_SIZE, 2048).
-define(DEFAULT_MAX_DELAY, 2000).
-define(DEFAULT_POOL_SIZE, 4).

%% types
-type filename() :: binary(). % TODO: fixme
-type open_option() :: pool_size.
-type open_options() :: [open_option()].
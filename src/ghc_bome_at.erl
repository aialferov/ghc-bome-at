-module(ghc_bome_at).
-export([main/1]).

-define(Config, [
    {dir, "suites"},
    {logdir, "ghc-bome-at-logs"},
    {config, "priv/ghc-bome-at.conf"},
    {basic_html, true}
]).

main(Args) ->
    inets:start(),
    Config = config(Args),
    ensure_log_dir(Config),
    ct:run_test(Config).

config(Args) ->
    maps:to_list(maps:merge(
        maps:from_list(?Config),
        maps:from_list(lists:filtermap(fun config_arg/1, Args))
    )).

config_arg(Arg) ->
    case string:split(Arg, "=") of
        ["--config", Value] -> {true, {config, Value}};
        ["--logdir", Value] -> {true, {logdir, Value}};
        ["--suites", Value] -> {true, {dir, Value}};
        _Other -> false
    end.

ensure_log_dir(Config) ->
    LogDir = proplists:get_value(logdir, Config, false),
    is_list(LogDir) andalso filelib:ensure_dir(LogDir ++ "/").

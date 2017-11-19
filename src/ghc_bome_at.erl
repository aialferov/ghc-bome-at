-module(ghc_bome_at).
-export([main/1]).

-define(Info,
    "~n"
    "See results at: \033[4;36mhttp://localhost:~b/index.html\033[0m~n"
    "~n"
    "Note: the link will not be available after exit.~n"
    "~n"
).
-define(Prompt,
    "Press 'Enter' to repeat or 'q' to exit: "
).

-define(Config, [
    {dir, "suites"},
    {logdir, "_build/logs"},
    {config, "priv/ghc-bome-at.conf"}
]).

main(Args) ->
    Config = config(Args),
    apply_config(Config),
    application:ensure_all_started(?MODULE),
    ensure_log_dir(Config),
    test_loop(Config).

test_loop(Config) ->
    ct:run_test(Config),
    {ok, Port} = application:get_env(?MODULE, port),
    io:format(?Info, [Port]),
    prompt_loop(Config).

prompt_loop(Config) ->
    case io:get_line(?Prompt) of
        eof -> ok;
        "q\n" -> ok;
        "\n" -> test_loop(Config);
        _Other -> prompt_loop(Config)
    end.

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

apply_config(Config) ->
    LogDir = proplists:get_value(logdir, Config, false),
    is_list(LogDir) andalso application:set_env(
        ?MODULE, log_dir, LogDir, [{persistent, true}]).

ensure_log_dir(Config) ->
    LogDir = proplists:get_value(logdir, Config, false),
    is_list(LogDir) andalso filelib:ensure_dir(LogDir ++ "/").

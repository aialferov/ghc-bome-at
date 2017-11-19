-module('ghc-bome-at').
-export([main/1]).

-define(Info,
    "~n"
    "See details at: \033[4;36mhttp://localhost:~b/index.html\033[0m~n"
    "~n"
    "Note: the link will not be available after exit.~n"
    "~n"
).
-define(Prompt,
    "Press 'Enter' to repeat or 'q' to exit: "
).
-define(CmdRepeat, "\n").
-define(CmdExit, "q\n").

main(Args) ->
    apply_env(Args),
    application:ensure_all_started(?MODULE),

    TestConfig = test_config(),
    ensure_log_dir(proplists:get_value(logdir, TestConfig)),
    extract_files(TestConfig),
    run_test_loop(TestConfig).

run_test_loop(Config) ->
    ct:run_test(Config),
    io:format(?Info, [proplists:get_value(port, Config)]),
    prompt_loop(Config).

prompt_loop(Config) ->
    case io:get_line(?Prompt) of
        eof -> ok;
        ?CmdExit -> ok;
        ?CmdRepeat -> run_test_loop(Config);
        _Other -> prompt_loop(Config)
    end.

apply_env(Args) ->
    lists:foreach(fun apply_env_par/1, Args).

apply_env_par(Arg) ->
    case string:split(Arg, "=") of
        ["--config", Value] -> set_env(config, Value);
        ["--logdir", Value] -> set_env(logdir, Value);
        ["--suites", Value] -> set_env(suites, Value);
        _Other -> false
    end.

set_env(Par, Val) ->
    application:set_env(?MODULE, Par, Val, [{persistent, true}]).

ensure_log_dir(LogDir) ->
    is_list(LogDir) andalso filelib:ensure_dir(LogDir ++ "/").

test_config() ->
    {ok, Env} = application:get_key(?MODULE, env),
    lists:filtermap(fun
        ({port, Value}) ->   {true, {port, Value}};
        ({config, Value}) -> {true, {config, Value}};
        ({logdir, Value}) -> {true, {logdir, Value}};
        ({suites, Value}) -> {true, {dir, Value}};
        (_Other) -> false
    end, Env).

extract_files(Config) ->
    {ok, Binary} = file:read_file(escript:script_name()),
    [_Header, Zip] = binary:split(Binary, <<"PK">>),
    {ok, Files} = zip:extract(<<"PK", Zip/binary>>, [memory]),
    lists:foreach(extract_file_fun(Config), Files).

extract_file_fun(Config) -> fun({Name, Content}) ->
    SuitesDir = proplists:get_value(dir, Config),
    ConfigFile = proplists:get_value(config, Config),

    IsSuite = lists:suffix("SUITE.erl", Name) orelse
              lists:suffix("SUITE.beam", Name),
    IsConfig = lists:suffix(".conf", Name),

    IsSuite andalso begin
        is_list(SuitesDir) andalso filelib:ensure_dir(SuitesDir ++ "/"),
        SuiteFile = filename:join(SuitesDir, filename:basename(Name)),
        file:write_file(SuiteFile, Content)
    end,
    IsConfig andalso file:write_file(ConfigFile, Content)
end.

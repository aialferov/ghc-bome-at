-module(ghc_bome_at_SUITE).

-export([
    all/0, suite/0,

    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2,

    get_all/1,
    get_filter_all/1,
    get_filter_one/1,
    get_filter_not_exists/1,
    get_filter_empty/1,
    get_user_not_exists/1,
    get_filter_bad/1,

    put_existing/1,
    put_new/1,
    put_malformed_json/1,

    patch_one/1,
    patch_user_not_exists/1,
    patch_malformed_json/1,

    delete_few/1,
    delete_all/1,
    delete_metrics_not_exist/1,
    delete_user_not_exists/1,
    delete_malformed_json/1,

    bad/1
]).

-include_lib("common_test/include/ct.hrl").

-define(Endpoint, "http://~s:~b/v1/users/~s~s").

-define(ContentType, "application/json").
-define(Options, [{body_format, binary}]).

all() -> [
    get_all,
    get_filter_all,
    get_filter_one,
    get_filter_not_exists,
    get_filter_empty,
    get_user_not_exists,
    get_filter_bad,

    put_existing,
    put_new,
    put_malformed_json,

    patch_one,
    patch_user_not_exists,
    patch_malformed_json,

    delete_few,
    delete_all,
    delete_metrics_not_exist,
    delete_user_not_exists,
    delete_malformed_json,

    bad
].

suite() -> [
    {require, host},
    {require, port}
].

init_per_suite(Config) ->
    NewConfig = [{host, config(host)},
                 {port, config(port)}|Config],
    delete_users(10, 10, NewConfig),
    NewConfig.

end_per_suite(Config) -> Config.

init_per_testcase(_Case, Config) -> create_users(3, 3, Config), Config.
end_per_testcase(_Case, Config) -> delete_users(10, 10, Config), Config.

get_all(Config) ->
    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>,
                 <<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2">>,
                 <<"ghc_bome_at_metric3">> := <<"ghc_bome_at_value3">>}}} =
        get("ghc_bome_at_user1", [], Config).

get_filter_all(Config) ->
    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>,
                 <<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2">>,
                 <<"ghc_bome_at_metric3">> := <<"ghc_bome_at_value3">>}}} =
        get("ghc_bome_at_user1", [{"filter", "ghc_bome_at_metric1,"
                                             "ghc_bome_at_metric2,"
                                             "ghc_bome_at_metric3"}], Config).

get_filter_one(Config) ->
    {ok, {200, #{<<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2">>}}} =
        get("ghc_bome_at_user2", [{"filter", "ghc_bome_at_metric2"}], Config).

get_filter_not_exists(Config) ->
    {ok, {200, #{}}} =
        get("ghc_bome_at_user2", [{"filter", "ghc_bome_at_metric4"}], Config).

get_filter_empty(Config) ->
    {ok, {200, #{}}} = get("ghc_bome_at_user2", [{"filter", ""}], Config).

get_user_not_exists(Config) ->
    {ok, {404, #{<<"ghc_bome_at_user4">> := <<"not_found">>}}} =
        get("ghc_bome_at_user4", [], Config).

get_filter_bad(Config) ->
    {ok, {400, #{<<"reason">> := #{<<"unknown_option">> := <<"bad_filter">>}}}} =
        get("ghc_bome_at_user4", [{"bad_filter", ""}], Config).

put_existing(Config) ->
    {ok, {204, <<"">>}} = put("ghc_bome_at_user3", metrics(1), Config),
    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>}}} =
        get("ghc_bome_at_user3", [], Config).

put_new(Config) ->
    {ok, {201, <<"">>}} = put("ghc_bome_at_user4", metrics(1), Config),
    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>}}} =
        get("ghc_bome_at_user4", [], Config).

put_malformed_json(Config) ->
    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        put("ghc_bome_at_user1", <<"{">>, Config).

patch_one(Config) ->
    {ok, {204, <<"">>}} = patch("ghc_bome_at_user3", jsx:encode(
        #{<<"ghc_bome_at_metric2">> => <<"ghc_bome_at_value2_u">>}), Config),

    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>,
                 <<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2_u">>,
                 <<"ghc_bome_at_metric3">> := <<"ghc_bome_at_value3">>}}} =
        get("ghc_bome_at_user3", [], Config).

patch_user_not_exists(Config) ->
    {ok, {404, #{<<"ghc_bome_at_user4">> := <<"not_found">>}}} =
        patch("ghc_bome_at_user4", metrics(1), Config).

patch_malformed_json(Config) ->
    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        patch("ghc_bome_at_user4", <<"{">>, Config).

delete_few(Config) ->
    {ok, {204, <<"">>}} = delete("ghc_bome_at_user1", jsx:encode(
        [<<"ghc_bome_at_metric1">>,
         <<"ghc_bome_at_metric3">>]), Config),

    {ok, {200, #{<<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2">>}}} =
        get("ghc_bome_at_user1", [], Config).

delete_all(Config) ->
    {ok, {204, <<"">>}} = delete("ghc_bome_at_user2", jsx:encode(
        [<<"ghc_bome_at_metric1">>,
         <<"ghc_bome_at_metric2">>,
         <<"ghc_bome_at_metric3">>]), Config),

    {ok, {404, #{<<"ghc_bome_at_user2">> := <<"not_found">>}}} =
        get("ghc_bome_at_user2", [], Config).

delete_metrics_not_exist(Config) ->
    {ok, {204, <<"">>}} = delete("ghc_bome_at_user3", jsx:encode(
        [<<"ghc_bome_at_metric4">>,
         <<"ghc_bome_at_metric5">>]), Config),

    {ok, {200, #{<<"ghc_bome_at_metric1">> := <<"ghc_bome_at_value1">>,
                 <<"ghc_bome_at_metric2">> := <<"ghc_bome_at_value2">>,
                 <<"ghc_bome_at_metric3">> := <<"ghc_bome_at_value3">>}}} =
        get("ghc_bome_at_user3", [], Config).

delete_user_not_exists(Config) ->
    {ok, {404, #{<<"ghc_bome_at_user4">> := <<"not_found">>}}} =
        delete("ghc_bome_at_user4", metric_names(1), Config).

delete_malformed_json(Config) ->
    {ok, {400, #{<<"reason">> := <<"malformed_json">>}}} =
        delete("ghc_bome_at_user3", <<"{">>, Config).

bad(Config) ->
    {Host, Port} = {host(Config), port(Config)},
    lists:foreach(fun(Endpoint) ->
        {ok, {400, _Usage}} = request(get, format(Endpoint, [Host, Port]))
    end, [
        "http://~s:~b/v2/users/id",
        "http://~s:~b/v1/user/id",
        "http://~s:~b/v1/users/id/type"
    ]).

endpoint_fun(Config) ->
    {Host, Port} = {host(Config), port(Config)},
    fun(Id, Ql) -> format(?Endpoint, [Host, Port, Id, qs(Ql)]) end.

qs([]) -> "";
qs(Ql) -> [$?|tl(lists:flatten([[$&|K] ++ [$=|V] || {K, V} <- Ql]))].

put(UserId, Metrics, Config) ->
    request(put, (endpoint_fun(Config))(UserId, []), Metrics).

patch(UserId, Metrics, Config) ->
    request(patch, (endpoint_fun(Config))(UserId, []), Metrics).

get(UserId, MetricNames, Config) ->
    request(get, (endpoint_fun(Config))(UserId, MetricNames)).

delete(UserId, Metrics, Config) ->
    request(delete, (endpoint_fun(Config))(UserId, []), Metrics).

request(Method, Endpoint) -> request(Method, Endpoint, []).
request(Method, Endpoint, Body) ->
    Request = case Method of
        get -> {Endpoint, ""};
        _Other -> {Endpoint, ?ContentType, [], Body}
    end,
    code_body(httpc:request(Method, Request, [], ?Options)).

code_body(Response) -> case Response of
    {ok, {{_Version, Code, _Reason}, _Headers, <<"">>}} -> {ok, {Code, <<"">>}};
    {ok, {{_Version, Code, _Reason}, Headers, Body}} ->
        {ok, case is_content_type_json(Headers) of
            true -> {Code, jsx:decode(Body, [return_maps])};
            false -> {Code, Body}
        end};

    {error, Reason} -> {error, Reason}
end.

is_content_type_json(Headers) ->
    proplists:get_value("content-type", Headers, "") == "application/json".

config(Name) ->
    config(Name, case os:getenv(string:uppercase(atom_to_list(Name))) of
        Value when is_list(Value) -> Value;
        false -> ct:get_config(Name)
    end).

config(port, Value) when is_list(Value) -> list_to_integer(Value);
config(_Name, Value) -> Value.

host(Config) -> proplists:get_value(host, Config).
port(Config) -> proplists:get_value(port, Config).

create_users(UserCount, MetricsPerUserCount, Config) ->
    lists:foreach(fun(X) ->
        {ok, _} = put(user_id(X), metrics(MetricsPerUserCount), Config)
    end, lists:seq(1, UserCount)).

delete_users(UserCount, MetricsPerUserCount, Config) ->
    lists:foreach(fun(X) ->
        {ok, _} = delete(user_id(X), metric_names(MetricsPerUserCount), Config)
    end, lists:seq(1, UserCount)).

user_id(N) -> <<"ghc_bome_at_user", (integer_to_binary(N))/binary>>.

metrics(Count) ->
    AddMetric = fun(N, Metrics) -> maps:put(
        <<"ghc_bome_at_metric", (integer_to_binary(N))/binary>>,
        <<"ghc_bome_at_value", (integer_to_binary(N))/binary>>,
        Metrics
    ) end,
    jsx:encode(lists:foldl(AddMetric, #{}, lists:seq(1, Count))).

metric_names(Count) ->
    jsx:encode([<<"ghc_bome_at_metric", (integer_to_binary(N))/binary>> ||
                N <- lists:seq(1, Count)]).

format(Format, Args) -> lists:flatten(io_lib:format(Format, Args)).

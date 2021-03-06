-module(ghc_bome_at_app).
-behaviour(application).

-export([
    start/2,
    stop/1, prep_stop/1
]).

-define(Listener, ghc_bome_at_http).

-define(ResourceUsers, "/v1/users/:id/").
-define(Resource, '_').

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_key(env),
    Port = proplists:get_value(port, Env),
    LogDir = proplists:get_value(logdir, Env),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", cowboy_static, {dir, LogDir}}
        ]}
    ]),
    Opts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(?Listener, [{port, Port}], Opts).

stop(_State) -> ok.

prep_stop(_State) -> cowboy:stop_listener(?Listener).

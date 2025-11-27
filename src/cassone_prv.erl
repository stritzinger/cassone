-module(cassone_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, cassone).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},            % The 'user friendly' name of the task
        {module, ?MODULE},            % The module implementation of the task
        {bare, true},                 % The task can be run by the user, always true
        {deps, ?DEPS},                % The list of dependencies
        {example, "rebar3 cassone"}, % How to use the plugin
        {opts, []},                   % list of options understood by the plugin
        {short_desc, "A rebar plugin"},
        {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("cassone: do", []),
    Config = rebar_state:get(State, cassone, []),
    Type = get_option(type, Config, release),
    case Type of
        escript ->
            pack_escript(State);
        release ->
            pack_release(State)
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions ----------------------------------------------------------

pack_escript(State) ->
    rebar_prv_escriptize:do(State).

pack_release(State) ->
    rebar_prv_release:do(State).

get_option(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} -> Value;
        false -> Default
    end.

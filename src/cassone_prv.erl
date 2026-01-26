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
    EscriptDir = filename:join(rebar_dir:base_dir(State), "bin"),
    CassoneDir = filename:join(rebar_dir:base_dir(State), "cassone"),
    rebar_utils:sh(io_lib:format("rm -rf ~s", [CassoneDir]), []),
    ok = filelib:ensure_path(EscriptDir),
    ok = filelib:ensure_path(CassoneDir),
    Config = rebar_state:get(State, cassone, []),
    Mode = get_option(mode, Config, release),
    Targets = get_option(targets, Config, [current_machine]),
    Opts = #{
        mode => Mode,
        escript_dir => EscriptDir,
        cassone_dir => CassoneDir
    },
    {Opts2, State1} = release(Mode, Opts, State),
    [begin
        cleanup_working_dir(Opts2),
        assemble_target(TGT, Opts2)
    end
    || TGT <- Targets],
    {ok, State1}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions ----------------------------------------------------------

release(escript, Opts, State) ->
    {ok, S} = rebar_prv_escriptize:do(State),
    EscriptName = rebar_state:get(State, escript_name),
    {Opts#{escript_name => atom_to_list(EscriptName)}, S};
release(release, _, _) ->
    rebar_api:abort("cassone: release mode is not supported yet", []).

get_option(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} -> Value;
        false -> Default
    end.

cleanup_working_dir(#{cassone_dir := CassoneDir, escript_name := EscriptName}) ->
    rebar_api:info("cassone: cleaning up working directory", []),
    ReleaseDir = filename:join([CassoneDir, "working", EscriptName]),
    rebar_utils:sh(io_lib:format("rm -rf ~s", [ReleaseDir]), []),
    ok.

assemble_target({OS, Arch}, Options) ->
    OtpVersion = otp_version(),
    TargetOTPDir = cassone_erts:fetch(OtpVersion, OS, Arch),
    {PiadinaPath, AzdoraPath} = cassone_piadina:fetch(OS, Arch),
    copy_released_files(Options),
    copy_erts_and_libs(TargetOTPDir, Options),
    cook(AzdoraPath, PiadinaPath, Options);
assemble_target(Target, _) ->
    rebar_api:warning("cassone: unsupported target: ~p", [Target]).

copy_released_files(#{
    mode := escript,
    cassone_dir := CassoneDir,
    escript_dir := EscriptDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying released files", []),
    RelEscriptLocation = filename:join([EscriptDir, EscriptName]),
    RelEscriptDst = filename:join([CassoneDir, EscriptName, "bin", EscriptName]),
    filelib:ensure_dir(RelEscriptDst),
    cp_cmd(RelEscriptLocation, RelEscriptDst).

copy_erts_and_libs(TargetOTPDir, #{
    mode := escript,
    cassone_dir := CassoneDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying erts and libs from ~p to ~p", [TargetOTPDir, CassoneDir]),
    [ErtsFolder | _] = filelib:wildcard("*/erts-*", TargetOTPDir),
    ErtsFullPath = filename:join([TargetOTPDir, ErtsFolder]),
    cp_cmd(ErtsFullPath, filename:join([CassoneDir, EscriptName])),
    [LibFolder | _] = filelib:wildcard("*/lib", TargetOTPDir),
    LibDir = filename:join([TargetOTPDir, LibFolder]),
    cp_cmd(LibDir, filename:join([CassoneDir, EscriptName])),
    ok.

cp_cmd(Src, Dst) ->
    rebar_api:info("cassone: copying ~p to ~p", [Src, Dst]),
    case filelib:is_dir(Src) of
        true ->
            Flags = "-r";
        false ->
            Flags = ""
    end,
    rebar_utils:sh(io_lib:format("cp ~s ~s ~s", [Flags, Src, Dst]), []).

otp_version() ->
    Root = code:root_dir(),
    Major = erlang:system_info(otp_release),
    {ok, Version} = file:read_file(filename:join([Root, "releases", Major, "OTP_VERSION"])),
    string:trim(binary_to_list(Version)).

cook(AzdoraPath, PiadinaPath, Options) ->
    rebar_api:info("cassone: cooking", []),
    ok.

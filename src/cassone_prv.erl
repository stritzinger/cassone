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
    BinDir = filename:join(rebar_dir:base_dir(State), "bin"),
    WorkingDir = filename:join(rebar_dir:base_dir(State), "cassone"),
    rebar_utils:sh(io_lib:format("rm -rf ~s", [WorkingDir]), []),
    ok = filelib:ensure_path(BinDir),
    ok = filelib:ensure_path(WorkingDir),
    Config = rebar_state:get(State, cassone, []),
    Mode = get_option(mode, Config, release),
    Targets = get_option(targets, Config, [current_machine]),
    Opts = #{
        mode => Mode,
        bin_dir => BinDir,
        working_dir => WorkingDir
    },
    {Opts2, State1} = release(Mode, Opts, State),
    prepare_piadina(WorkingDir),
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
release(release, Opts, State) ->
    rebar_api:abort("cassone: release mode is not supported yet", []).

get_option(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} -> Value;
        false -> Default
    end.

cleanup_working_dir(#{working_dir := WorkingDir, escript_name := EscriptName}) ->
    rebar_api:info("cassone: cleaning up working directory", []),
    ReleaseDir = filename:join([WorkingDir, EscriptName]),
    rebar_utils:sh(io_lib:format("rm -rf ~s", [ReleaseDir]), []),
    rebar_api:info("cassone: cleanup piadina repository", []),
    PiadinaDir = filename:join([WorkingDir, "piadina"]),
    rebar_utils:sh("cd " ++ PiadinaDir ++ " && git clean -fdx", []),
    ok.

assemble_target(current_machine, Options) ->
    rebar_api:info("cassone: assembling current machine", []),
    LocalErlangReleaseDir = code:root_dir(),
    copy_released_files(Options),
    copy_erts_and_libs(LocalErlangReleaseDir, Options),
    build_piadina(Options);
assemble_target({OS, Arch}, Options) ->
    OtpVersion = otp_version(),
    TargetOTPBuildDir = cassone_erts:fetch(OtpVersion, Arch, OS),
    copy_released_files(Options),
    copy_erts_and_libs(TargetOTPBuildDir, Options),
    build_piadina(Options);
assemble_target(Target, Options) ->
    rebar_api:warning("cassone: unsupported target: ~p", [Target]).

copy_released_files(#{
    mode := escript,
    working_dir := WorkingDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying released files", []),
    RelEscriptLocation = filename:join([BinDir, EscriptName]),
    RelEscriptDst = filename:join([WorkingDir, EscriptName, "bin", EscriptName]),
    filelib:ensure_dir(RelEscriptDst),
    cp_cmd(RelEscriptLocation, RelEscriptDst).

copy_erts_and_libs(TargetOTPInstallation,#{
    mode := escript,
    working_dir := WorkingDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying erts and libs", []),
    [ErtsFolder | _] = filelib:wildcard("erts-*", TargetOTPInstallation),
    ErtsFullPath = filename:join([TargetOTPInstallation, ErtsFolder]),
    cp_cmd(ErtsFullPath, filename:join([WorkingDir, EscriptName])),
    LibDir = filename:join([TargetOTPInstallation, "lib"]),
    cp_cmd(LibDir, filename:join([WorkingDir, EscriptName])),
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

get_erts_version(LocalErlangReleaseDir) ->
    StartErlData = filename:join([LocalErlangReleaseDir, "releases", "start_erl.data"]),
    {ok, Bin} = file:read_file(StartErlData),
    [ErtVersion, _] = binary:split(Bin, <<" ">>, [global]),
    ErtVersion.

otp_version() ->
    Root = code:root_dir(),
    Major = erlang:system_info(otp_release),
    {ok, Version} = file:read_file(filename:join([Root, "releases", Major, "OTP_VERSION"])),
    string:trim(binary_to_list(Version)).

prepare_piadina(WorkingDir) ->
    rebar_api:info("cassone: preparing piadina", []),
    GitResource = {git, "https://github.com/stritzinger/piadina.git", {branch, "main"}},
    PiadinaDir = filename:join([WorkingDir, "piadina"]),
    case filelib:is_dir(PiadinaDir) of
        false -> rebar_git_resource:download(PiadinaDir, GitResource, []);
        true -> ok
    end.

build_piadina(#{working_dir := WorkingDir}) ->
    rebar_api:info("cassone: building piadina", []),
    PiadinaDir = filename:join([WorkingDir, "piadina"]),
    Opts = [{cd, PiadinaDir}],
    rebar_utils:sh("./autogen.sh", Opts),
    rebar_utils:sh("./configure ", Opts),
    rebar_utils:sh("make", Opts),
    ok.

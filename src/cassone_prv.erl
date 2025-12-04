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
    [assemble_target(TGT, Opts2) || TGT <- Targets],
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

assemble_target(current_machine, Options) ->
    assemble_for_current_machine(Options);
assemble_target({OS, Arch},
               #{working_dir := WorkingDir,
                 escript_name := EscriptName} = Options) ->
    OtpVersion = otp_version(),
    TargetOTPBuildDir = cassone_erts:fetch(OtpVersion, Arch, OS),
    copy_released_files(Options),
    copy_erlang_binaries(TargetOTPBuildDir, Options),
    copy_erts_and_libs(TargetOTPBuildDir, Options);
assemble_target(Target, Options) ->
    rebar_api:warning("cassone: unsupported target: ~p", [Target]).

assemble_for_current_machine(#{working_dir := WorkingDir,
                               escript_name := EscriptName} = Options) ->
    rebar_api:info("cassone: assembling current machine", []),
    LocalErlangReleaseDir = code:root_dir(),
    copy_released_files(Options),
    copy_erlang_binaries(LocalErlangReleaseDir, Options),
    copy_erts_and_libs(LocalErlangReleaseDir, Options).

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

copy_erlang_binaries(TargetOTPInstallation, #{
    mode := escript,
    working_dir := WorkingDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying binaries", []),
    LocalEscriptBin = filename:join([TargetOTPInstallation, "bin", "escript"]),
    CassoneEscriptBin = filename:join([WorkingDir, EscriptName, "bin", "escript"]),
    filelib:ensure_dir(CassoneEscriptBin),
    cp_cmd(LocalEscriptBin, CassoneEscriptBin).

copy_erts_and_libs(TargetOTPInstallation,#{
    mode := escript,
    working_dir := WorkingDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying erts and libs", []),
    [ErtsFolder | _] = filelib:wildcard("erts-*", TargetOTPInstallation),
    cp_cmd(ErtsFolder, filename:join([WorkingDir, EscriptName])),
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
    os:cmd(io_lib:format("cp ~s ~s ~s", [Flags, Src, Dst])).

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

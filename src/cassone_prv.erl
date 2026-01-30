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
    [AppInfo] = rebar_state:project_apps(State),
    Version = rebar_app_info:original_vsn(AppInfo),
    EscriptDir = filename:join(rebar_dir:base_dir(State), "bin"),
    CassoneDir = filename:join(rebar_dir:base_dir(State), "cassone"),
    rebar_utils:sh(io_lib:format("rm -rf ~s", [CassoneDir]), []),
    ok = filelib:ensure_path(EscriptDir),
    ok = filelib:ensure_path(CassoneDir),
    Config = rebar_state:get(State, cassone, []),
    Mode = get_option(mode, Config, release),
    Targets = get_option(targets, Config, [current_machine]),
    Opts = #{
        version => Version,
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
    run_otp_install(TargetOTPDir),
    MuslcRuntime = cassone_musl:fetch(OS,Arch),
    {PiadinaPath, AzdoraPath} = cassone_piadina:fetch(OS, Arch),
    rebar_utils:sh("chmod +x " ++ MuslcRuntime, []),
    rebar_utils:sh("chmod +x " ++ AzdoraPath, []),
    rebar_utils:sh("chmod +x " ++ PiadinaPath, []),
    copy_released_files(Options),
    Options2 = copy_target_otp(TargetOTPDir, Options),
    copy_musl_runtime(MuslcRuntime, Options2),
    MuslcRuntimeName = filename:basename(MuslcRuntime),
    cook(AzdoraPath, PiadinaPath, MuslcRuntimeName, Options2);
assemble_target(Target, _) ->
    rebar_api:warning("cassone: unsupported target: ~p", [Target]).

run_otp_install(TargetOTPDir) ->
    [InstallScript | _] = filelib:wildcard("*/Install", TargetOTPDir),
    FullInstallScriptName = filename:join([TargetOTPDir, InstallScript]),
    InstallDir = filename:dirname(FullInstallScriptName),
    rebar_utils:sh(FullInstallScriptName ++ " -minimal " ++ InstallDir, []).

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

copy_target_otp(TargetOTPDir, #{
    mode := escript,
    cassone_dir := CassoneDir,
    escript_name := EscriptName
} = Options) ->
    PayloadLocation = filename:join([CassoneDir, EscriptName]),
    rebar_api:info("cassone: copying erts, libs and binaries from ~p to ~p", [TargetOTPDir, PayloadLocation]),
    [ErtsFolder | _] = filelib:wildcard("*/erts-*", TargetOTPDir),
    [_, ErtsVersion] = string:split(ErtsFolder, "-"),
    ErtsFullPath = filename:join([TargetOTPDir, ErtsFolder]),
    cp_cmd(ErtsFullPath, PayloadLocation),
    [LibFolder | _] = filelib:wildcard("*/lib", TargetOTPDir),
    LibDir = filename:join([TargetOTPDir, LibFolder]),
    cp_cmd(LibDir, PayloadLocation),
    [BinFolder | _] = filelib:wildcard("*/bin", TargetOTPDir),
    BinDir = filename:join([TargetOTPDir, BinFolder]),
    cp_cmd(BinDir, PayloadLocation),
    % We replace epmd symlink with the real file to avoid an error from tar+gzip.
    rebar_api:info("cassone: removing epmd symlink", []),
    rebar_utils:sh("rm " ++ filename:join([PayloadLocation, "bin", "epmd"]), []),
    Options#{erts_version => ErtsVersion}.

copy_musl_runtime(MuslcRuntime, #{
    mode := escript,
    cassone_dir := CassoneDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying musl runtime from ~p to ~p", [MuslcRuntime, CassoneDir]),
    cp_cmd(MuslcRuntime, filename:join([CassoneDir, EscriptName])).

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

cook(AzdoraPath, PiadinaPath, MuslcRuntimeName, #{mode := escript} = Options) ->
    #{cassone_dir := CassoneDir,
      escript_name := EscriptName,
      version := Version,
      erts_version := ErtsVersion
    } = Options,
    MUSL = filename:join(["{PAYLOAD_ROOT}", MuslcRuntimeName]),
    EntryArgs = filename:join(["{PAYLOAD_ROOT}", "bin", EscriptName]),
    % TODO: do not hardcode paths containing erts and apps versions
    Command = AzdoraPath ++ " "
        "--launcher " ++ PiadinaPath ++ " "
        "--payload " ++ filename:join([CassoneDir, EscriptName]) ++ " "
        "--output " ++ EscriptName ++ ".bin "
        "--meta APP_NAME=" ++ EscriptName ++ " "
        "--meta APP_VER=" ++ Version ++ " "
        "--meta ENTRY_POINT=bin/escript "
        "--meta ENTRY_ARGS[]=" ++ EntryArgs ++ " "
        "--meta ENV.ERL_ROOTDIR=\"{PAYLOAD_ROOT}\" "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/escript:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=lib/erl_interface-5.6.1/bin/erl_call:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/run_erl:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/yielding_c_fun:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/erlc:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/epmd:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/dialyzer:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/erl_call:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/inet_gethost:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/beam.smp:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/erlexec:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/to_erl:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/erl_child_setup:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/heart:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/dyn_erl:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/ct_run:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/typer:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=erts-" ++ ErtsVersion ++ "/bin/escript:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/run_erl:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/erlc:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/dialyzer:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/erl_call:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/to_erl:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/ct_run:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/typer:" ++ MUSL ++ " "
        "--meta PATCHELF_SET_INTERPRETER[]=bin/escript:" ++ MUSL ++ " ",
    rebar_utils:sh(Command, []).

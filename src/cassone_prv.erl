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
    CassoneDir = filename:join(rebar_dir:base_dir(State), "cassone"),
    ok = filelib:ensure_path(BinDir),
    ok = filelib:ensure_path(CassoneDir),
    Config = rebar_state:get(State, cassone, []),
    Mode = get_option(mode, Config, release),

    Targets = get_option(targets, Config, [current_machine]),
    {ok, State1} = case Mode of
        escript ->
            rebar_prv_escriptize:do(State);
        release ->
            rebar_api:abort("cassone: release mode is not supported yet", [])
    end,
    Options = #{
        mode => Mode,
        escript_name => rebar_state:get(State, escript_name),
        bin_dir => BinDir,
        cassone_dir => CassoneDir
    },
    [assemble_target(TGT,Options) || TGT <- Targets],
    {ok, State1}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions ----------------------------------------------------------

get_option(Option, Config, Default) ->
    case lists:keyfind(Option, 1, Config) of
        {Option, Value} -> Value;
        false -> Default
    end.

assemble_target(Target, Options) ->
    case Target of
        current_machine ->
            assemble_for_current_machine(Options);
        _ ->
            rebar_api:warning("cassone: unsupported target: ~p", [Target])
    end.

assemble_for_current_machine(#{cassone_dir := CassoneDir} = Options) ->
    rebar_api:info("cassone: assembling current machine", []),
    copy_released_files(Options),
    copy_binaries(Options),
    copy_erts_and_libs(Options),
    {ok, Cwd} = file:get_cwd(),
    tar("cassone", CassoneDir).

copy_released_files(#{
    mode := escript,
    cassone_dir := CassoneDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying released files", []),
    RelEscriptLocation = filename:join([BinDir, EscriptName]),
    RelEscriptDst = filename:join([CassoneDir, "bin", EscriptName]),
    filelib:ensure_dir(RelEscriptDst),
    cp_cmd(RelEscriptLocation, RelEscriptDst).

copy_binaries(#{
    mode := escript,
    cassone_dir := CassoneDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying binaries", []),

    LocalErlangReleaseDir = code:root_dir(),
    LocalEscriptBin = filename:join([LocalErlangReleaseDir, "bin", "escript"]),
    CassoneEscriptBin = filename:join([CassoneDir, "bin", "escript"]),
    filelib:ensure_dir(CassoneEscriptBin),
    cp_cmd(LocalEscriptBin, CassoneEscriptBin).

copy_erts_and_libs(#{
    mode := escript,
    cassone_dir := CassoneDir,
    bin_dir := BinDir,
    escript_name := EscriptName
}) ->
    rebar_api:info("cassone: copying erts and libs", []),
    LocalErlangReleaseDir = code:root_dir(),
    ErtsVsn = get_erts_version(LocalErlangReleaseDir),
    ErtsFolder = <<"erts-", ErtsVsn/binary>>,
    LocalErlangErts = filename:join([LocalErlangReleaseDir, ErtsFolder]),
    cp_cmd(LocalErlangErts, CassoneDir),
    LibDir = filename:join([LocalErlangReleaseDir, "lib"]),
    cp_cmd(LibDir, CassoneDir),
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

tar(Name, Dir) ->
    {ok, BackupCwd} = file:get_cwd(),
    TarFilename = filename:join([BackupCwd, Name ++ ".tar.gz"]),
    BaseName = filename:basename(Dir),
    file:set_cwd(filename:dirname(Dir)),
    rebar_api:info("cassone: creating tar file ~p in ~p", [Name, BaseName]),
    ok = erl_tar:create(TarFilename, [BaseName], [compressed]),
    file:set_cwd(BackupCwd).

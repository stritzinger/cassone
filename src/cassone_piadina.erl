-module(cassone_piadina).

-export([fetch/2]).

-define(url_template,
"https://github.com/stritzinger/piadina/releases/download/{V}/{PROG}-{OS}-{ARCH}").

-spec fetch(OS :: cassone:os(), Arch :: cassone:arch()) ->
    {string(), string()}.
fetch(OS, Arch) ->
    PiadinaPath = fetch_piadina(OS, Arch),
    AzdoraPath = fetch_azdora(OS, Arch),
    {PiadinaPath, AzdoraPath}.

fetch_piadina(OS, Arch) ->
    URL = make_url("piadina", OS, Arch),
    cassone_cache:fetch(URL).
fetch_azdora(OS, Arch) ->
    URL = make_url("azdora", OS, Arch),
    cassone_cache:fetch(URL).

make_url(Prog, OS, Arch) ->
    Replacements = [
        {"{PROG}", Prog},
        {"{V}", "1.0.0"},
        {"{OS}", atom_to_list(OS)},
        {"{ARCH}", arch_to_string(Arch)}
    ],
    lists:foldl(
        fun({Key, Value}, Acc) ->
            string:replace(Acc, Key, Value, all)
        end, ?url_template, Replacements).

arch_to_string(x86_64) -> "amd64";
arch_to_string(aarch64) -> "arm64".

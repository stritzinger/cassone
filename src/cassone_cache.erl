-module(cassone_cache).

-export([fetch/1]).

fetch(URL) ->
    #{path := Path} = uri_string:parse(URL),
    PkgFilename = filename:basename(Path),
    case package_is_cached(PkgFilename) of
        true -> ok;
        false ->
            Body = try_to_download(URL),
            ok = store_package_in_cache(PkgFilename, Body)
    end,
    ExtractedFile = extract_if_necessary(PkgFilename),
    filename:join(cache_dir(), ExtractedFile).


extract_if_necessary(PkgFilename) ->
    case is_tar_or_gz(PkgFilename) of
        true ->
            ExtractionDirName = filename:rootname(filename:rootname(PkgFilename)),
            case package_is_extracted(ExtractionDirName) of
                true -> ok;
                false -> ok = extract_package(PkgFilename, ExtractionDirName)
            end,
            ExtractionDirName;
        false ->
            PkgFilename
    end.

is_tar_or_gz(Filename) ->
    filename:extension(Filename) =:= ".gz"
    orelse filename:extension(Filename) =:= ".tar".

try_to_download(URL) ->
    case download(URL) of
        {ok, Body} -> Body;
        {ok, Status, _, _} ->
            rebar_api:abort("cassone_cache: "
                            "download failed with status code: ~p", [Status])
    end.

download(URL) ->
    {ok, _} = application:ensure_all_started(hackney),
    rebar_api:info("cassone_cache: downloading ~s", [URL]),
    Headers = [],
    Payload = <<>>,
    Options = [
        {recv_timeout, infinity},
        {follow_redirect, true},
        {max_redirect, 2}
    ],
    maybe
        {ok, 200, _, ClientRef} ?= hackney:get(URL, Headers, Payload, Options),
        hackney:body(ClientRef)
    end.

package_is_cached(Filename) ->
    CachedFile = filename:join(cache_dir(), Filename),
    filelib:is_regular(CachedFile).

package_is_extracted(ExtractionDirName) ->
    ExtractedPkg = filename:join(cache_dir(), ExtractionDirName),
    filelib:is_dir(ExtractedPkg).

extract_package(Filename, ExtractionDirName) ->
    Cached = filename:join(cache_dir(), Filename),
    filelib:ensure_dir(Cached),
    ExtractionDir = filename:join(cache_dir(), ExtractionDirName),
    filelib:ensure_path(ExtractionDir),
    rebar_api:info("cassone_cache: extracting ~s", [Cached]),
    erl_tar:extract(Cached, [{cwd, ExtractionDir}, compressed]).

store_package_in_cache(Filename, Binary) ->
    Cached = filename:join(cache_dir(), Filename),
    filelib:ensure_dir(Cached),
    file:write_file(Cached, Binary).

cache_dir() ->
    filename:basedir(user_cache, "cassone").

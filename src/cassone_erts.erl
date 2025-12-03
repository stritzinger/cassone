-module(cassone_erts).

-export([fetch/3]).

% Borrowing packages from beam machine reelases for Burrito.
-define(linux_url, "https://beam-machine-universal.b-cdn.net/OTP-{OTP_VERSION}/linux/{ARCH}/any/otp_{OTP_VERSION}_linux_any_{ARCH}.tar.gz").
-define(mac_url, "https://beam-machine-universal.b-cdn.net/OTP-{OTP_VERSION}/macos/universal/otp_{OTP_VERSION}_macos_universal.tar.gz").
-define(please_do_not_abuse_these_downloads_bandwidth_costs_money, "?please-respect-my-bandwidth-costs=thank-you").

-doc("Returns the path to the cached OTP release.

The release is either downloaded or unpacked if needed.").
fetch(OtpVersion, Arch, OS) ->
    ArchString = atom_to_list(Arch),
    URL = make_url(OtpVersion, ArchString, OS),
    PkgFilename = filename:basename(URL),
    ExtractionDirName = burrito_pkg_extraction_dir_name(OtpVersion, ArchString, OS),
    case package_is_cached(PkgFilename) of
        true ->
            ok;
        false ->
            {ok, Body} = download(URL),
            ok = store_package_in_cache(PkgFilename, Body)
    end,
    case package_is_extracted(ExtractionDirName) of
        true ->
            ok;
        false ->
            ok = extract_package(PkgFilename)
    end,
    filename:join(cache_dir(), ExtractionDirName).

make_url(OtpVersion, ArchString, OS) ->
    Template = url_template(OS),
    string:replace(
        string:replace(Template, "{OTP_VERSION}", OtpVersion, all),
        "{ARCH}", ArchString, all).

download(URL) ->
    FinalUrl = URL ++ ?please_do_not_abuse_these_downloads_bandwidth_costs_money,
    rebar_api:info("cassone_erts: downloading ~s", [FinalUrl]),
    Headers = [],
    Payload = <<>>,
    Options = [{recv_timeout, infinity}],
    maybe
        {ok, 200, _, ClientRef} ?= hackney:get(FinalUrl, Headers, Payload, Options),
        hackney:body(ClientRef)
    end.

url_template(linux) -> ?linux_url;
url_template(macos) -> ?mac_url.

package_is_cached(Filename) ->
    CachedFile = filename:join(cache_dir(), Filename),
    filelib:is_regular(CachedFile).

package_is_extracted(ExtractionDirName) ->
    ExtractedPkg = filename:join(cache_dir(), ExtractionDirName),
    filelib:is_dir(ExtractedPkg).

store_package_in_cache(Filename, Binary) ->
    Cached = filename:join(cache_dir(), Filename),
    filelib:ensure_dir(Cached),
    file:write_file(Cached, Binary).

extract_package(Filename) ->
    Cached = filename:join(cache_dir(), Filename),
    filelib:ensure_dir(Cached),
    rebar_api:info("cassone: extracting ~s", [Cached]),
    erl_tar:extract(Cached, [{cwd, cache_dir()}, compressed]).

cache_dir() ->
    filename:basedir(user_cache, "cassone").

burrito_pkg_extraction_dir_name(OtpVersion, _, macos) ->
    Template = "otp_universal_apple_darwin_{OTP_VERSION}",
    string:replace(Template, "{OTP_VERSION}", OtpVersion);
burrito_pkg_extraction_dir_name(OtpVersion, ArchString, linux) ->
    Template = "otp_{ARCH}_linux_{OTP_VERSION}",
    string:replace(
        string:replace(Template, "{ARCH}", ArchString),
        "{OTP_VERSION}", OtpVersion).

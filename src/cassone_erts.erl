-module(cassone_erts).

-export([fetch/3]).

% Borrowing packages from beam machine relases for Burrito.
-define(linux_url, "https://beam-machine-universal.b-cdn.net/OTP-{OTP_VERSION}/linux/{ARCH}/any/otp_{OTP_VERSION}_linux_any_{ARCH}.tar.gz").
-define(mac_url, "https://beam-machine-universal.b-cdn.net/OTP-{OTP_VERSION}/macos/universal/otp_{OTP_VERSION}_macos_universal.tar.gz").
-define(please_do_not_abuse_these_downloads_bandwidth_costs_money, "?please-respect-my-bandwidth-costs=thank-you").


-doc("Returns the path to the cached OTP release.
The release is either downloaded or unpacked if needed.").
-spec fetch(OtpVersion :: string(), OS :: cassone:os(), Arch :: cassone:arch()) ->
    string().
fetch(OtpVersion, OS, Arch) ->
    ArchString = atom_to_list(Arch),
    URL = make_url(OtpVersion, OS, ArchString),
    FinalURL = URL ++ ?please_do_not_abuse_these_downloads_bandwidth_costs_money,
    cassone_cache:fetch(FinalURL).

make_url(OtpVersion, OS, ArchString) ->
    Template = url_template(OS),
    string:replace(
        string:replace(Template, "{OTP_VERSION}", OtpVersion, all),
        "{ARCH}", ArchString, all).

url_template(linux) -> ?linux_url;
url_template(macos) -> ?mac_url.

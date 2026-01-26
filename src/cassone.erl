-module(cassone).

-export([init/1]).

-export_types([os/0, arch/0]).

-type os() :: linux | macos.
-type arch() :: x86_64 | aarch64.

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = cassone_prv:init(State),
    {ok, State1}.

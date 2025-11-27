cassone
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {cassone, {git, "https://host/user/cassone.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 cassone
    ===> Fetching cassone
    ===> Compiling cassone
    <Plugin Output>

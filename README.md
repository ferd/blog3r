blog3r
=====

A rebar plugin attempting to re-make https://github.com/ferd/blogerl but as a rebar3
plugin for a custom compiler module so that it can reuse the DAG code and do partial
blog re-builds without having to handle the tracking itself.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {blog3r, {git, "https://host/user/blog3r.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 blog3r
    ===> Fetching blog3r
    ===> Compiling blog3r
    <Plugin Output>

-module(blog3r_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, blog3r).
-define(DEPS, [app_discovery]).

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
            {example, "rebar3 compile"},  % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin to generate blog posts"},
            {desc, "A rebar plugin to generate blog posts, by reusing the compiler "
                   "DAG in Rebar3 to handle partial builds with no friction"}
    ]),
    %% TODO: Check for pygments
    %% append compilers so if someone uses the `load' keyword it is
    %% already compiled.
    State1 = rebar_state:append_compilers(State, [blog3r_compile]),
    {ok, rebar_state:add_provider(State1, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

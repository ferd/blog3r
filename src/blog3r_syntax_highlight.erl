-module(blog3r_syntax_highlight).
-export([render_code/1]).
-define(TMP_CODE_FILE, "/tmp/blogerl-pre.tmp").

render_code(BaseHTML) ->
    case has_pygments() of
        true ->
            map_pre(unicode:characters_to_binary(BaseHTML));
        false ->
            warn_once("Pygments not installed, will not "
                      "auto-generate syntax highlighting"),
            BaseHTML
    end.

has_pygments() ->
    case get(has_pygments) of
        undefined ->
            case re:run(os:cmd("pygmentize"), "command not found") of
                nomatch ->
                    put(has_pygments, true),
                    true;
                _ ->
                    put(has_pygments, false),
                    false
            end;
        Res ->
            Res
    end.

warn_once(Str) ->
    case get({warn, Str}) of
        undefined ->
            put({warn, Str}, true),
            rebar_api:warn("~ts~n", [Str]);
        _ ->
            ok
    end.

%% Because pygments uses semantic linebreaks and that the
%% mochihtml library doesn't preserve it, we need to substitute
%% all <pre> tag contents by hand.
%% TODO: optimize parser & pygments calls
map_pre(<<"<pre>", Rest/binary>>) ->
    {Until, More} = until_end_pre(Rest),
    <<"<pre>", Until/binary, "</pre>", (map_pre(More))/binary>>;
map_pre(<<"<pre ", Rest/binary>>) ->
    {Lang, More} = get_lang_attrs(Rest),
    {Content, Tail} = until_end_pre(More),
    {ok, Fd} = file:open(?TMP_CODE_FILE, [write, raw]),
    ok = file:write(Fd, unescape_code(Content)),
    file:close(Fd),
    Cmd = unicode:characters_to_list(
        ["pygmentize ", ["-l ", Lang], " -f html ", ?TMP_CODE_FILE]
    ),
    Code = os:cmd(Cmd),
    NewCode = unicode:characters_to_binary(Code),
    <<NewCode/binary, (map_pre(Tail))/binary>>;
map_pre(<<Char, Rest/binary>>) ->
    <<Char, (map_pre(Rest))/binary>>;
map_pre(<<>>) ->
    <<>>.

until_end_pre(Bin) -> until_end_pre(Bin, <<>>).
until_end_pre(<<"</pre>", Rest/binary>>, Acc) ->
    {Acc, Rest};
until_end_pre(<<Char, Rest/binary>>, Acc) ->
    until_end_pre(Rest, <<Acc/binary, Char>>).

get_lang_attrs(<<"class=", Delim, LangStr/binary>>) ->
    {Str, Rest} = extract_str(LangStr, Delim),
    Lang = handle_pre_class(Str),
    {Lang, drop_until_tag_close(Rest)};
get_lang_attrs(<<">", Rest/binary>>) ->
    {undefined, Rest};
get_lang_attrs(<<_, Rest/binary>>) ->
    get_lang_attrs(Rest).

extract_str(Str, Delim) -> extract_str(Str, Delim, <<>>).

extract_str(<<$\\, Quoted, Rest/binary>>, Delim, Acc) when Quoted == Delim ->
    extract_str(Rest, Delim, <<Acc/binary, $\\, Quoted>>);
extract_str(<<Char, Rest/binary>>, Delim, Acc) when Char == Delim ->
    {Acc, Rest};
extract_str(<<Char, Rest/binary>>, Delim, Acc) ->
    extract_str(Rest, Delim, <<Acc/binary, Char>>).

handle_pre_class(<<"brush:erl">>) -> <<"erlang">>;
handle_pre_class(<<"brush:eshell">>) -> <<"erlang">>;
handle_pre_class(<<"brush:", Lang/binary>>) -> Lang;
handle_pre_class(Lang) -> Lang.

drop_until_tag_close(<<">", Rest/binary>>) ->
    Rest;
drop_until_tag_close(<<_, Rest/binary>>) ->
    drop_until_tag_close(Rest).

unescape_code(OrigStr) ->
    lists:foldl(fun({Pat, Repl}, Str) ->
                    re:replace(Str, Pat, Repl, [global])
                end,
                OrigStr,
                [{"&gt;", ">"},
                 {"&lt;", "<"},
                 {"&amp;", "\\&"}]).

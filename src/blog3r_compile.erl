-module(blog3r_compile).
-export([context/1, dependencies/4, needed_files/4, compile_and_track/4, clean/2]).
-define(TMP_CODE_FILE, "/tmp/blogerl-pre.tmp").

%% specify what kind of files to find and where to find them. Rebar3 handles
%% doing all the searching from these concepts.
context(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    SrcDirs = [filename:join([AppDir, "posts"]),
               filename:join([AppDir, "templates"])],
    InclDirs = [filename:join([AppDir, "templates"])],
    OutMaps = [{".html", filename:join([AppDir, "compiled"])}],
    Opts = rebar_app_info:get(AppInfo, blog3r, unconfigured),
    #{src_dirs     => SrcDirs,
      include_dirs => InclDirs,
      src_ext      => ".tpl",
      out_mappings => OutMaps,
      dependencies_opts => Opts}.

%% Define which files each of the files depends on, including includes and whatnot.
%% This is then used to create a digraph of all existing files to know how to propagate
%% file changes. The Digraph is passed to other callbacks as `G' and annotaes all files
%% with their last changed timestamp
%% Prior to 3.14, the `State' argument was not available.
dependencies(SrcFile, _SrcDir, AllDirs, _Opts) ->
    BaseNames = find_deps(SrcFile),
    [FullName || Name <- BaseNames,
                         Dir <- AllDirs,
                         FullName <- [filename:join([Dir, Name])],
                         filelib:is_file(FullName)].

%% do your own analysis aided by the graph to specify what needs re-compiling.
%% You can use this to add more or fewer files (i.e. compiler options changed),
%% and specify how to schedule their compilation. One thing we do here for
%% erlang files is look at the digraph to only rebuild files with newer
%% timestamps than their build artifacts (which are also in the DAG after the
%% first build) or those with compiler options that changed (the
%% compile_and_track callback lets you annotate artifacts)
needed_files(G, AllFiles, [{Ext, OutDir}], AppInfo) ->
    Opts = rebar_app_info:get(AppInfo, blog3r, []),
    IndexCfg = cfg(index, Opts, []),
    Mappings = cfg(mappings, IndexCfg, []),
    Vars = cfg(vars, Opts),
    {Mapped, NeededFiles} = needed(G, AllFiles, {Ext, OutDir}, Mappings, Vars),
    RebuildShared = case NeededFiles of
        [] ->
            [];
        _ ->
            IndexSrc = expand_name(cfg(template, IndexCfg), AllFiles),
            FeedSrc = expand_name(cfg(template, cfg(feed, Opts)), AllFiles),
            [IndexSrc, FeedSrc] % TODO: only rebuild feed if needed files changed
    end,
    %% Sort the files in build order
    SubGraph = digraph_utils:subgraph(G, NeededFiles),
    Ordered = lists:sort(digraph_utils:topsort(SubGraph)), % give us an order of it all
    %% the returned files to build essentially specify a schedule and priority with special
    %% option sets
     %% We use ErlyDTL in module mode, which means only one module at a time exists
     %% and we therefore must do a sequential build.
    {{Ordered, Mapped},
     %% Finish with the special files which must come after the others
     {{RebuildShared, []}, {Mapped, Vars}}}.

%% Compilation callback with the ability to track build artifacts in the DAG itself.
compile_and_track(File, [OutMap], AppOpts, {Mapped, Vars}) ->
    %% Rebuilding the index.html and feed.rss files, built last.
    rebar_api:debug("Compiling: ~p", [File]),
    Opts = rebar_opts:get(AppOpts, blog3r),
    FeedSrc = cfg(template, cfg(feed, Opts)),
    IndexSrc = cfg(template, cfg(index, Opts)),
    DocRoot = filename:join(lists:reverse(tl(lists:dropwhile(
       fun("posts") -> false;
          ("templates") -> false;
          (_) -> true
       end, lists:reverse(filename:split(File)))
    )) ++ ["templates"]),
    case filename:basename(File) of
        IndexSrc ->
            Index = [[{date, Date}, {title, Title}, {slug, sluggify(Title)}]
                     || {{Date, _LongDate, Title}, _Src, _Artifact, _Opts} <- Mapped],
            BuildOpts = [{doc_root, DocRoot},
                         {default_vars, [{pages, lists:reverse(lists:sort(Index))}
                                         | Vars]}],
            {ok, tpl} = erlydtl:compile(File, tpl, BuildOpts),
            {ok, Text} = tpl:render([]),
            Artifact = out(OutMap, "index"),
            filelib:ensure_dir(Artifact),
            ok = file:write_file(Artifact, Text),
            {ok, [{File, Artifact, BuildOpts}]};
        FeedSrc ->
            Num = cfg(num_entries, cfg(feed, Opts), 5),
            Entries = lists:sublist(lists:reverse(lists:sort(Mapped)), Num),
            Articles = [[{sort, Date}, {date, LongDate}, {title, Title},
                         {slug, sluggify(Title)}, {desc, rss_entry(Artifact)}]
                        || {{Date, LongDate, Title}, _Src, Artifact, _Opts} <- Entries],
            [_, {date, LatestDate}|_] = hd(Articles),
            BuildOpts = [{doc_root, DocRoot},
                         {default_vars, [{articles, Articles},
                                         {latest_date, LatestDate}] ++ Vars},
                         {auto_escape, false}],
            {ok, tpl} = erlydtl:compile(File, tpl, BuildOpts),
            {ok, Text} = tpl:render([]),
            {_Ext, Dir} = OutMap,
            Artifact = filename:join(Dir, "feed.rss"),
            filelib:ensure_dir(Artifact),
            ok = file:write_file(Artifact, Text),
            {ok, [{File, Artifact, [LatestDate, Num]}]}
    end;
compile_and_track(File, _OutMaps, _AppConfig, Mapped) ->
    %% Regular blog posts
    rebar_api:debug("Compiling: ~p", [File]),
    {_Key, Source, Artifact, BuildOpts} = lists:keyfind(File, 2, Mapped),
    {ok, Bin} = file:read_file(Source),
    MD = markdown(Bin),
    {ok, tpl} = erlydtl:compile(MD, tpl, BuildOpts),
    {ok, Text} = tpl:render(BuildOpts),
    CodeText = render_code(Text),
    rebar_api:debug("Writing ~p", [Artifact]),
    filelib:ensure_dir(Artifact),
    file:write_file(Artifact, CodeText),
    {ok, [{File, Artifact, BuildOpts}]}.

%% Just delete files however you need to
clean(_Files, _AppInfo) -> ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
find_deps(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Tokens} = erlydtl_scanner:scan(Bin),
    lists:usort(find_deps_(Tokens)).

find_deps_([]) ->
    [];
    %% Extends literal
find_deps_([{open_tag, _, _}, {extends_keyword, _, _},
            {string_literal, _, Str}, {close_tag, _, _}|Rest]) ->
    [string:trim(Str, both, "\"") | find_deps_(Rest)];
    %% include literal
find_deps_([{open_tag, _, _}, {include_keyword, _, _},
            {string_literal, _, Str}, {close_tag, _, _}|Rest]) ->
    [string:trim(Str, both, "\"") | find_deps_(Rest)];
find_deps_([_|Rest]) ->
    find_deps_(Rest).

expand_name(Name, Files) ->
    {value, F} = lists:search(fun(F) -> lists:suffix(Name, F) end, Files),
    F.

cfg(Key, Proplist) ->
    proplists:get_value(Key, Proplist).

cfg(Key, Proplist, Default) ->
    proplists:get_value(Key, Proplist, Default).

needed(G, Found, OutInfo, Mappings, Vars) ->
    %% Only build files in mappings, but expand paths to those found.
    %% For each of them, assemble the build options and the build target.
    %% Check if the file specifically needs re-building.
    ToCheck = [{{format_date(Date), Date, Title}, Source, out(OutInfo, Title),
                build_opts(Source, Date, Title, Vars)}
               || {Date, Title, BaseName} <- Mappings,
                   Source <- [expand_name(BaseName, Found)]],
    ToBuild = lists:filter(
        fun({_Key, Source, Artifact, Opts}) ->
            digraph:vertex(G, Source) > {Source, filelib:last_modified(Artifact)}
            orelse opts_changed(G, Opts, Artifact)
        end,
        ToCheck
    ),
    {ToCheck, [Source || {_Key, Source, _, _} <- ToBuild]}.

out({Ext, Dir}, Title) ->
    filename:join([Dir, sluggify(Title) ++ Ext]).

build_opts(File, Date, Title, Vars) ->
    Meta = {meta, [{date, format_date(Date)},
                   {title, Title}]},
    DocRoot = filename:join(lists:reverse(tl(lists:dropwhile(
       fun("posts") -> false;
          ("templates") -> false;
          (_) -> true
       end, lists:reverse(filename:split(File)))
    )) ++ ["templates"]),
    [{doc_root, DocRoot},
     {default_vars, [Meta|Vars]}].

opts_changed(G, Opts, Artifact) ->
    case digraph:vertex(G, Artifact) of
        {_Artifact, {artifact, Opts}} -> false;
        _ -> true
    end.

%% no need to be efficient, and only worrying about latin characters
%% for now. Some sort of unicode normalization would be more
%% accurate in the long run.
sluggify(Str) ->
    Patterns = [{"[âäàáÀÄÂÁ]", "a"},
               {"[éêëèÉÊËÈ]", "e"},
               {"[ïîíìÏÎÌÍ]", "i"},
               {"[öôòóÖÔÒÓ]", "o"},
               {"[üûùúÜÛÙÚ]", "u"},
               {"[ÿýÝ]", "y"},
               {"[çÇ]", "c"},
               {"[^\\w\\d_-]", "-"},
               {"[-]{2,}", "-"},
               {"(?:^-)|(?:-$)", ""}],
    Slug = lists:foldl(fun({Pat, Rep}, Slug) ->
                       re:replace(Slug, Pat, Rep, [global, {return, binary}])
                       end,
                       Str,
                       Patterns),
    string:to_lower(binary_to_list(Slug)).

%% "Thu, 22 Jul 2010 00:00:00 EST" -> 2010/07/22
format_date(Str) ->
    {match, [Day,Month,Year]} = re:run(Str,
                                       "([\\d]{1,2}) ([\\w]{3}) ([\\d]{4})",
                                       [{capture, all_but_first, list}]),
    string:join([Year, month(Month), Day], "/").

month("Jan") -> "01";
month("Feb") -> "02";
month("Mar") -> "03";
month("Apr") -> "04";
month("May") -> "05";
month("Jun") -> "06";
month("Jul") -> "07";
month("Aug") -> "08";
month("Sep") -> "09";
month("Oct") -> "10";
month("Nov") -> "11";
month("Dec") -> "12".

markdown(Bin) ->
    iolist_to_binary(parse(binary_to_list(Bin))).

% ideally we'd get a more clever algorithm but eh
parse([]) -> [];
parse("{% markdown %}" ++ Rest) ->
    {MD, Other} = markdown_conv(Rest),
    [MD | parse(Other)];
parse([Char | Rest]) ->
    [Char | parse(Rest)].

markdown_conv(Str) ->
    case string:split(Str, "{% endmarkdown %}", leading) of
        [MarkDown, Rest] ->
            {markdown:conv_utf8(MarkDown), Rest};
        _ ->
            error("Markdown closing tag ({% endmarkdown %}) not found")
    end.

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


rss_entry(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    "<![CDATA[" ++
    mochiweb_html:to_html(
      {<<"div">>, [],
       article(mochiweb_html:parse(Bin))}
    )
    ++"]]>".

article({<<"article">>, _, Content}) -> Content;
article({_, _, Tree}) -> article(Tree);
article({comment, _}) -> not_found;
article(Bin) when is_binary(Bin) -> not_found;
article([]) -> not_found;
article([H|T]) ->
    case article(H) of
        not_found -> article(T);
        Article -> Article
    end.

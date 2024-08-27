-module(blog3r_compile).
-export([context/1, dependencies/4, needed_files/4, compile_and_track/4, clean/2]).
-define(TMP_CODE_FILE, "/tmp/blogerl-pre.tmp").

-type index() :: #{template := file:filename_all(),
                   out := file:filename_all(),
                   section := atom()}.
-type out_mappings() :: [{string(), file:filename_all()}, ...].

-type sections() :: #{atom() => section()}.
-type section() :: {file:filename_all(), file:filename_all(), [page()]}.
-type page() :: {longdate(), title(), file:filename_all()}.
-type longdate() :: string().
-type title() :: string().

-type vars() :: [{doc_root, file:filename_all()}
                 | {default_vars, default_vars()}
                 | {atom(), term()}].
-type default_vars() :: [{pages, [page_vars()]}
                        |{page, page_vars()}
                        |{atom(), term()}].
-type page_vars() :: [{date, string()} | {longdate, longdate()} |
                      {title, title()} | {slug, title()} | {out, file:filename_all()}].

%% specify what kind of files to find and where to find them. Rebar3 handles
%% doing all the searching from these concepts.
context(AppInfo) ->
    {_Opts, AppDir, Sections, Indices, Vars} = from_app_info(AppInfo),
    PostDirs = lists:usort([SrcDir || {SrcDir, _OutDir, _Posts} <- maps:values(Sections)]),
    SrcDirs = [filename:join([AppDir, PostDir]) || PostDir <- PostDirs]
              ++ [filename:join([AppDir, "templates"])],
    InclDirs = [filename:join([AppDir, "templates"])],
    OutMaps = [{".html", filename:join([AppDir, "compiled"])},
               {".rss", filename:join([AppDir, "compiled"])}],
    #{src_dirs     => lists:usort(SrcDirs),
      include_dirs => InclDirs,
      src_ext      => ".tpl",
      out_mappings => OutMaps,
      dependencies_opts => {AppDir, Vars, Sections, Indices}}.

%% Define which files each of the files depends on, including includes and whatnot.
%% This is then used to create a digraph of all existing files to know how to propagate
%% file changes. The Digraph is passed to other callbacks as `G' and annotaes all files
%% with their last changed timestamp
%% Prior to 3.14, the `State' argument was not available.
dependencies(SrcFile, _SrcDir, AllDirs, {AppDir, _Vars, Sections, Indices}) ->
    BaseNames = find_tpl_deps(SrcFile),
    TplDeps = [FullName
               || Name <- BaseNames,
                  Dir <- AllDirs,
                  FullName <- [filename:join([Dir, Name])],
                  filelib:is_file(FullName)],
    IndexDeps = find_index_deps(SrcFile, Indices, Sections, AppDir),
    lists:usort(TplDeps ++ IndexDeps).

%% do your own analysis aided by the graph to specify what needs re-compiling.
%% You can use this to add more or fewer files (i.e. compiler options changed),
%% and specify how to schedule their compilation. One thing we do here for
%% erlang files is look at the digraph to only rebuild files with newer
%% timestamps than their build artifacts (which are also in the DAG after the
%% first build) or those with compiler options that changed (the
%% compile_and_track callback lets you annotate artifacts)
needed_files(G, _AllFiles, OutMaps, AppInfo) ->
    {_Opts, AppDir, Sections, Indices, Vars} = from_app_info(AppInfo),
    Annotated = indices_annotations(Indices, AppDir, OutMaps, Sections, Vars)
              ++ pages_annotations(Sections, AppDir, OutMaps, Vars),
    ToBuild = lists:filter(
        fun({Source, Artifact, FileOpts}) ->
            digraph:vertex(G, Source) > {Source, filelib:last_modified(Artifact)}
            orelse opts_changed(G, FileOpts, Artifact)
        end,
        Annotated
    ),
    NeededFiles = [Source || {Source, _, _} <- ToBuild],
    %%%
    %% Sort the files in build order
    SubGraph = digraph_utils:subgraph(G, NeededFiles),
    Ordered = lists:reverse(digraph_utils:topsort(SubGraph)), % give us an order of it all
    rebar_api:debug("ordered: ~p", [Ordered]),
    %% the returned files to build essentially specify a schedule and priority with special
    %% option sets
     %% We use ErlyDTL in module mode, which means only one module at a time exists
     %% and we therefore must do a sequential build.
    {{Ordered, Annotated}, {{[], []}, {Annotated, Vars}}}.

%% Compilation callback with the ability to track build artifacts in the DAG itself.
compile_and_track(File, OutMaps, _AppConfig, Annotated) ->
    %% Regular blog posts
    rebar_api:debug("Compiling: ~p", [File]),
    {Source, Artifact, BuildOpts} = lists:keyfind(File, 1, Annotated),
    {ok, Bin} = file:read_file(Source),
    MD = markdown(Bin),
    {ok, tpl} = erlydtl:compile(MD, tpl, BuildOpts),
    {ok, Text} = tpl:render(BuildOpts),
    Expanded = expand_content(OutMaps, Text),
    CodeText = render_code(Expanded),
    rebar_api:debug("Writing ~p", [Artifact]),
    filelib:ensure_dir(Artifact),
    file:write_file(Artifact, CodeText),
    {ok, [{File, Artifact, BuildOpts}]}.

%% Just delete files however you need to
clean(_Files, _AppInfo) -> ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% @private find dependencies coming from the templating structure
find_tpl_deps(File) ->
    {ok, Bin} = file:read_file(File),
    {ok, Tokens} = erlydtl_scanner:scan(Bin),
    lists:usort(find_tpl_deps_(Tokens)).

find_tpl_deps_([]) ->
    [];
    %% Extends literal
find_tpl_deps_([{open_tag, _, _}, {extends_keyword, _, _},
            {string_literal, _, Str}, {close_tag, _, _}|Rest]) ->
    [string:trim(Str, both, "\"") | find_tpl_deps_(Rest)];
    %% include literal
find_tpl_deps_([{open_tag, _, _}, {include_keyword, _, _},
            {string_literal, _, Str}, {close_tag, _, _}|Rest]) ->
    [string:trim(Str, both, "\"") | find_tpl_deps_(Rest)];
find_tpl_deps_([_|Rest]) ->
    find_tpl_deps_(Rest).

%% @private find dependencies for index files, based on their sections
find_index_deps(_, [], _, _) ->
    [];
find_index_deps(File, [Index|Indices], Sections, AppDir) ->
    #{template := Basename, section := Key} = Index,
    case filename:basename(File) of
        Basename -> % matching!
            #{Key := {SrcDir, _OutDir, Entries}} = Sections,
            [filename:join([AppDir, SrcDir, EntrySrc])
             || {_Date, _Title, EntrySrc} <- Entries];
        _ ->
            find_index_deps(File, Indices, Sections, AppDir)
    end.

%% @private create sections between source files and their artifacts and options
-spec indices_annotations([index()], AppDir, out_mappings(), sections(), vars()) ->
    [{Src, Artifact, vars()}]
    when AppDir :: file:filename_all(),
         Src :: file:filename_all(),
         Artifact :: file:filename_all().
indices_annotations(Indices, AppDir, OutMaps, Sections, Vars) ->
    [{_DefaultExt, DefaultOutDir}|_] = OutMaps,
    [{filename:join([AppDir, "templates", Tpl]),
      filename:join([cfg(filename:extension(Out), OutMaps, DefaultOutDir), safe_rel(Out)]),
      [{doc_root, filename:join(AppDir, "templates")},
       {default_vars, [pages_vars(MapName, Sections)|Vars]}]}
     || #{template := Tpl, out := Out, section := MapName} <- Indices].

-spec pages_annotations(sections(), AppDir, out_mappings(), vars()) ->
    [{Src, Artifact, vars()}]
    when AppDir :: file:filename_all(),
         Src :: file:filename_all(),
         Artifact :: file:filename_all().
pages_annotations(Sections, AppDir, OutMaps, Vars) ->
    [{DefaultExt, DefaultOutDir}|_] = OutMaps,
    [{filename:join([AppDir, safe_rel(SrcDir), safe_rel(Tpl)]),
      filename:join([DefaultOutDir, safe_rel(OutSub), sluggify(Title)++DefaultExt]),
      [{doc_root, filename:join(AppDir, "templates")},
       {default_vars, [{page, [{date, format_date(Date)}, {longdate, Date}, {title, Title},
                       {slug, sluggify(Title)}, {out, safe_rel(OutSub)}]}
                      | Vars]}]}
     || {SrcDir, OutSub, Pages} <- maps:values(Sections),
        {Date, Title, Tpl} <- Pages].

%% @private make a path safe for relative use; basically any root path is
%% turned into a relative one by prepending a period.
safe_rel("/"++Path) -> "./"++Path;
safe_rel(Path) -> Path.

-spec pages_vars(atom(), sections()) -> {pages, [page_vars()]}.
%% @private generate the template variables for all possible pages within a section
pages_vars(Name, Map) ->
    #{Name := {_In, RawOut, Posts}} = Map,
    Out = safe_rel(RawOut),
    Attrs = [[{date, format_date(Date)}, {longdate, Date},
              {title, Title}, {slug, sluggify(Title)}, {out, Out}]
             || {Date, Title, _EntrySrc} <- Posts],
    {pages, Attrs}.

cfg(Key, Proplist) ->
    proplists:get_value(Key, Proplist).

cfg(Key, Proplist, Default) ->
    proplists:get_value(Key, Proplist, Default).

cfgs(Key, Proplist) ->
    proplists:get_all_values(Key, Proplist).

%% @private extract common required elements for various callbacks
from_app_info(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    Opts = rebar_app_info:get(AppInfo, blog3r, []),
    Sections = cfg(sections, Opts, #{}),
    Indices = cfgs(index, Opts),
    Vars = cfg(vars, Opts, []),
    {Opts, AppDir, Sections, Indices, Vars}.

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

expand_content(OutMap, Str) ->
    maybe
        %% Find the spots with {! inline <filename> Node[#id] !} in them and
        %% split the components
        [PreExpand, TmpStr] ?= string:split(Str, "{! inline ", leading),
        [Opts, PostExpand] ?= string:split(TmpStr, " !}", leading),
        [File, Node] ?= string:split(Opts, " ", trailing),
        {Pre, Mid, Post} =
          maybe
              %% Happy path substitution
              FileName = unicode:characters_to_list(File),
              Path = case cfg(filename:extension(FileName), OutMap) of
                  undefined -> FileName;
                  OutDir -> filename:join(OutDir, FileName)
              end,
              %% These are most faillible calls
              {ok, FileContent} ?= file:read_file(Path),
              {ok, Content} ?= get_node(Node, FileContent),
              {PreExpand, Content, PostExpand}
          else
              Reason ->
                %% Eat up all errors by leaving things the same.
                rebar_api:warn("post-processing failed for file ~ts and node ~ts for reason ~p",
                               [File, Node, Reason]),
                {PreExpand, ["{! inline ", Opts, " !}"], PostExpand}
          end,
        unicode:characters_to_binary([Pre, Mid, expand_content(OutMap, Post)])
    else
        %% no valid processing declaration found, skip this file
        _ -> Str
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

%% @private Go down a mochiweb_html tree, and extract the first node that
%% matches the passed in argument. The node may either be a name for the
%% element (such as `article`) or a name and an id (such as `div#id`).
get_node(Node, Data) ->
    case string:split(Node, "#") of
        [NodePart, Id] ->
            get_node(unicode:characters_to_binary(NodePart),
                     unicode:characters_to_binary(Id),
                     Data);
        _ ->
            get_node(unicode:characters_to_binary(Node), undefined, Data)
    end.

get_node(Node, Id, Data) ->
    case tree_node(Node, Id, mochiweb_html:parse(Data)) of
        not_found ->
            {error, {node_not_found, {Node, Id}}};
        Content ->
            {ok, mochiweb_html:to_html({<<"div">>, [], Content})}
    end.

tree_node(Node, Id, {Node, Attrs, Content}) ->
    case cfg(<<"id">>, Attrs) of
        Id -> Content;
        _ when Id =:= undefined -> Content;
        _ -> tree_node(Node, Id, Content)
    end;
tree_node(Node, Id, {_, _, Tree}) ->
    tree_node(Node, Id, Tree);
tree_node(_, _, {comment, _}) ->
    not_found;
tree_node(_, _, Bin) when is_binary(Bin) ->
    not_found;
tree_node(_, _, []) ->
    not_found;
tree_node(Node, Id, [H|T]) ->
    case tree_node(Node, Id, H) of
        not_found -> tree_node(Node, Id, T);
        Content -> Content
    end.

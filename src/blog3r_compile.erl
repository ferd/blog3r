-module(blog3r_compile).
-export([context/1, dependencies/4, needed_files/4, compile_and_track/4, clean/2]).

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
    %% the returned files to build essentially specify a schedule and priority with special
    %% option sets
    %% We use ErlyDTL in module mode, which means only one module at a time exists
    %% and we therefore must do a sequential build.
    {{Ordered, Annotated}, {{[], []}, {Annotated, Vars}}}.

%% Compilation callback with the ability to track build artifacts in the DAG itself.
compile_and_track(File, OutMaps, _AppConfig, Annotated) ->
    %% Regular blog posts
    try
        rebar_api:debug("Compiling: ~p", [File]),
        {Source, Artifact, BuildOpts} = lists:keyfind(File, 1, Annotated),
        {ok, Bin} = file:read_file(Source),
        PreText = pre_process(Bin),
        Mod = compile_template({File, PreText}, BuildOpts),
        {ok, Text} = Mod:render(BuildOpts),
        PostText = post_process(Text, OutMaps),
        rebar_api:debug("Writing ~p", [Artifact]),
        filelib:ensure_dir(Artifact),
        file:write_file(Artifact, PostText),
        {ok, [{File, Artifact, BuildOpts}]}
    catch
        error:erlydtl_error -> error
    end.

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
%% 'extends' literal
find_tpl_deps_([{open_tag, _, _}, {extends_keyword, _, _},
            {string_literal, _, Str}, {close_tag, _, _}|Rest]) ->
    [string:trim(Str, both, "\"") | find_tpl_deps_(Rest)];
%% 'include' literal
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

%% @private Compile the ErlyDTL template and return the module name
%% for it
-spec compile_template({file:filename_all(), string()|binary()}, vars()) -> module().
compile_template({File, TplStr}, BuildOpts) ->
    case erlydtl:compile(TplStr, tpl, [return|BuildOpts]) of
        {ok, tpl} ->
            tpl;
        {ok, tpl, _Warns} ->
            tpl;
        error ->
            error(erlydtl_error);
        {error, [{_, Reasons}], _} ->
            %% rich errors from syntactic issues
            [rebar_api:error("template error:~n~ts", [
                rebar_compiler_format:format(File, {Ln, Col}, "", Str, [rich])
            ]) || {{Ln,Col}, _Mod, Str} <- Reasons],
            %% other errors (eg. template dependencies not found)
            [rebar_api:error("template error in ~ts:~n~p", [File, Term])
             || {none, _Mod, Term} <- Reasons],
            error(erlydtl_error)
    end.

%% @private pre-processing of text, before erlydtl template execution takes
%% place. Right now, only `{% markdown %} ... {% endmarkdown %}` tags are
%% handled, and this is done here because I was too lazy to actually do it
%% within the erlydtl tag framework.
pre_process(Bin) ->
    blog3r_markdown:compile(Bin).

%% @private post-processing of text, after erlydtl template execution takes
%% place. This currently does content expansion (including HTML nodes by ID
%% with `{! inline <file> <node>[#<id>] !}` syntax) to get literal HTML
%% content that ignores all template processing, and to do syntax highlighting
%% by calling out to pygments.
post_process(Text, OutMaps) ->
    Expanded = blog3r_inline_html:expand(OutMaps, Text),
    blog3r_syntax_highlight:render_code(Expanded).


-spec pages_vars(atom(), sections()) -> {pages, [page_vars()]}.
%% @private generate the template variables for all possible pages within a section
pages_vars(Name, Map) ->
    #{Name := {_In, RawOut, Posts}} = Map,
    Out = safe_rel(RawOut),
    Attrs = [[{date, format_date(Date)}, {longdate, Date},
              {title, Title}, {slug, sluggify(Title)}, {out, Out}]
             || {Date, Title, _EntrySrc} <- Posts],
    {pages, Attrs}.

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

%% @private check if build options changed by comparing current
%% ones to those stored in the previously built artifact, if any
opts_changed(G, Opts, Artifact) ->
    case digraph:vertex(G, Artifact) of
        {_Artifact, {artifact, Opts}} -> false;
        _ -> true
    end.

%% @private make a path safe for relative use; basically any root path is
%% turned into a relative one by prepending a period.
safe_rel("/"++Path) -> "./"++Path;
safe_rel(Path) -> Path.

%% @private no need to be efficient, and only worrying about latin characters
%% for now. Some sort of unicode normalization would be more accurate in the
%% long run.
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


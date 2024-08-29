-module(blog3r_inline_html).
-export([expand/2]).

-type out_mappings() :: [{string(), file:filename_all()}, ...].

-spec expand(out_mappings(), string()|binary()) -> binary().
expand(OutMap, Str) ->
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
              Path = case proplists:get_value(filename:extension(FileName), OutMap) of
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
        unicode:characters_to_binary([Pre, Mid, expand(OutMap, Post)])
    else
        %% no valid processing declaration found, skip this file
        _ -> Str
    end.

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
    case proplists:get_value(<<"id">>, Attrs) of
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


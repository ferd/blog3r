-module(blog3r_markdown).
-export([compile/1]).

compile(Bin) ->
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

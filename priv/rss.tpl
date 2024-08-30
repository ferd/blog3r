<?xml version="1.0"?>
<rss version="2.0">
  <channel>
    <title>My Blog</title>
    <link>{{ url.base }}</link>
    <description>My own blog about programming and whatnot.</description>
    <language>en-us</language>
    <pubDate>{{ pages.1.longdate }}</pubDate>
    <lastBuildDate>{{ pages.1.longdate }}</lastBuildDate>
    <ttl>60</ttl>

    {% for article in pages %}{% if forloop.counter <= 10 %}
    <item>
      <title>{{ article.title|escape }}</title>
      <link>{{ url.base }}{{ article.slug }}.html</link>
      <description>
        <![CDATA[
        {! inline {{ article.out}}{{ article.slug }}.html div#article !}
        ]]>
    </description>
      <pubDate>{{ article.longdate }}</pubDate>
      <guid>{{ url.base }}{{ article.slug }}.html</guid>
    </item>
    {% endif %}{% endfor %}
 
  </channel>
</rss>


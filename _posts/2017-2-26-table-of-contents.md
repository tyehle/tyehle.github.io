---
layout: post
title: Automatic Table of Contents in Jekyll
---

# Edit from 2020:

Kramdown now makes this easy! Checkout [these gitlab docs](https://about.gitlab.com/blog/2016/07/19/markdown-kramdown-tips-and-tricks/#table-of-contents-toc) for a modern way to do this.

---



I recently wrote a [very long post]({% post_url 2017-01-23-lambda-compiler %}) and I wanted a table of contents to appear at the top.

I also came up with a good test for the length of a post; if a table of contents would be helpful, its too long.

All the code for this project is [on github](https://github.com/tyehle/toc-generation). There is also [an example page](http://tobin.yehle.io/toc-generation/).

-----

Results
-----

{% raw %}

Kramdown adds an `id` attribute to all headers automatically. The table of contents contains links to these headers.

The most basic interface is:

```markdown
{% include toc.html %}
```

The user should also be able to specify which tags to link to and to manually ignore specific tags. The include takes arguments to achieve this.

```markdown
{% include toc.html tags="H2,H3" ignore="#sections" %}
```

-------

Details
-------

The html include loads jQuery from Google's CDN if it can otherwise it falls back to the local copy. I do this because the CDN is probably faster to load, but if it fails, then the page will still load. I like this because one of the big advantages of Jekyll is offline development. I shamelessly stole this idea from [this question](http://stackoverflow.com/a/1014251).

The html include file uses liquid tags translate the arguments into JavaScript.

```html
<ul id="_toc">
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.slim.min.js"></script>
  <script type="text/javascript">
    if(!window.jQuery) {
      document.write('<script src="{{ "/js/jquery-3.1.1.slim.min.js" | prepend: site.baseurl }}"><\/script>')
    }
  </script>
  <script src='{{ "/js/toc.js" | prepend: site.baseurl }}'></script>
  <script>
    let tags = {%if include.tags%}"{{include.tags}}"{%else%}"H1,H2,H3,H4"{%endif%};
    let ignore = {%if include.ignore%}"{{include.ignore}}"{%else%}''{%endif%};
    $(document).ready(function () {
      toc($("#_toc"), tags, ignore);
    });
  </script>
</ul>
```

The id of the `<ul>` to put the table in is `_toc` because, as far as I can tell, Kramdown will not generate that id automatically. If the document contained a header called ToC, and it appeared before the include call, then the entire table would be inserted into the header tag. Not pretty. This is still technically possible if someone manually set an id to `_toc` anywhere before the include call.

{% endraw %}

### JavaScript

The first step is to get the headers and what level those headers should be displayed at. The order of the input tags is used to determine the level.

```javascript
function getHeaders(tagNames, ignore) {
  let tags = tagNames.split(",");
  return $(tagNames.toString()).filter("[id]").not(ignore).map(function() {
    return {
      level: tags.indexOf($(this).prop("tagName")),
      link: "#" + $(this).attr("id"),
      text: $(this).text()
    };
  });
}
```

Next I used a linked list of buffers to build a nested array header information.

```javascript
function nestHeaders(headers) {
  let base = [];
  let buffer = {level: 0, list: base, prev: undefined};
  headers.each(function(_, header) {
    if(header.level >= 0) {
      while(header.level > buffer.level) {
        let newBuffer = {level: buffer.level + 1, list: [], prev: buffer};
        buffer = newBuffer;
      }

      while(header.level < buffer.level) {
        buffer.prev.list.push(buffer.list);
        buffer = buffer.prev;
      }

      buffer.list.push(header);
    }
  });

  while(buffer.prev != undefined) {
    buffer.prev.list.push(buffer.list);
    buffer = buffer.prev;
  }

  return base;
}
```

Using that nested array, I can recursively build the table off of a given base `ul` element. On each call it either generates an `li` element for that header or a `ul` element that is filled by a recursive call to `genToc`.

```javascript
function genToc(base, nestedHeaders) {
  nestedHeaders.forEach(function(elem) {
    if(elem instanceof Array) {
      let ul = $("<ul/>");
      genToc(ul, elem);
      base.append(ul);
    } else {
      base.append($("<li/>").append($("<a/>", {
        href: elem.link,
        text: elem.text
      })));
    }
  });
}
```

These pieces are combined in the function `toc` which is called from the included html.

```javascript
function toc(base, tagNames, ignore) {
  genToc(base, nestHeaders(getHeaders(tagNames, ignore)));
}
```

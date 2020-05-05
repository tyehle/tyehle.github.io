---
layout: page
title: Articles
permalink: /articles/
weight: 20
---

<ul class="post-list">
{% for post in site.posts %}
  {% capture short_name %}{{ post.id | remove_first: "/articles/" }}{% endcapture %}
  {% if site.unlisted_posts contains post.title or site.unlisted_posts contains short_name %}
    <!-- Do not show hidden posts -->
  {% else %}
    <li>
      <span class="post-meta">{{ post.date | date: "%b %-d, %Y" }}</span>

      <h2>
        <a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>
      </h2>
    </li>
  {% endif %}
{% endfor %}
</ul>

<p class="rss-subscribe">subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a></p>

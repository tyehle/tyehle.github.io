---
layout: page
title: Articles
permalink: /articles/
weight: 3
---

This is a list of bits of code or pieces of information I found useful or interesting when I learned them. They are here either because I think others may find them useful, or I suspect I will forget them and want to refer back here in the future.

***

<ul class="post-list">
{% for post in site.posts %}
  {% if site.hidden_posts contains post.title %}
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

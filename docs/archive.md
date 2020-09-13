---
layout: simple
title: Archive
header: Post Archive
group: navigation
maint: true
---
{% include JB/setup %}

## Blog
{% for post in site.posts %}
* [{{ post.title }}]({{ post.url }}) <small>{{ post.date | date: "%Y-%m-%d" }}</small>
{%- endfor %}

## Article
{% assign pages_list
      = site.pages
        | where_exp: 'item', 'item.redirect_to == nil'
        | where_exp: 'item', 'item.maint == nil'
        | where_exp: 'item', 'item.title != nil'
        | sort: 'title'
        | sort: 'dir' %}

{% for page in pages_list %}
* <small>{{ page.dir | slice: 1,100 }}</small>[{{ page.title }}]({{ page.url }})
{%- endfor %}

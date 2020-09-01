---
layout: page
title: Archive
header: Post Archive
group: navigation
maint: true
---
{% include JB/setup %}

<h2>Blog</h2>
{%- assign article_list = site.posts -%}
{%- include custom/article_list -%}

<h2>Article</h2>
{%- assign pages_list
      = site.pages
        | where_exp: 'item', 'item.redirect_to == nil'
        | where_exp: 'item', 'item.maint == nil'
        | sort: 'title' -%}
{%- include custom/pages_list %}

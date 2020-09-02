---
layout: page
title: Emacsパッケージ紹介
---
{% include JB/setup %}

Emacs24に標準搭載されている `package.el` を利用してインストール可能なパッケージの中からおすすめのパッケージを紹介します。

{% assign pages_list
     = site.pages
        | where_exp: 'item', 'item.dir == "/packages/"'
        | where_exp: 'item', 'item.name != "index.md"'
        | where_exp: 'item', 'item.redirect_to == nil' %}

{% for page in pages_list %}
* [{{ page.name | remove: ".md"}}]({{ page.url }}): {{ page.title }}
{%- endfor %}

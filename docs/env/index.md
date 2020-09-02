---
layout: page
title: 各言語向けの環境構築
---
{% include JB/setup %}

各言語向けの環境構築について扱います。

おすすめパッケージセットとして紹介されることが多いので、別途[packages](/packages)の内容も参照して頂ければと思います。

{% assign pages_list
     = site.pages
        | where_exp: 'item', 'item.dir == "/env/"'
        | where_exp: 'item', 'item.name != "index.md"'
        | where_exp: 'item', 'item.redirect_to == nil' %}

{% for page in pages_list %}
* [{{ page.name | remove: ".md"}}]({{ page.url }}): {{ page.title }}
{%- endfor %}

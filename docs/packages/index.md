---
layout: page
title: 'パッケージ紹介'
descrition: 'package.el でinstallできるおすすめ melpa package紹介'
---
{% include JB/setup %}

Emacs24に標準搭載されている `package.el` を利用してインストール可能なパッケージの中からおすすめのパッケージを紹介します。

{% assign pages_list
     = site.pages
        | where_exp: 'item', 'item.dir == "/packages/"'
        | where_exp: 'item', 'item.name != "index.md"'
        | where_exp: 'item', 'item.redirect_to == nil' %}

{% for page in pages_list %}
* [{{ page.title }}]({{ page.url }})
   {% for tag in page.tags %}<span class="label label-info">{{ tag }}</span> {% endfor %}
  <br>{{ page.description }}
{% endfor %}

{% assign pages_list = nil %}

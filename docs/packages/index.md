---
layout: page
title: 'パッケージ紹介'
descrition: 'package.el でinstallできるおすすめ melpa package紹介'
---
{% include JB/setup %}

Emacs24に標準搭載されている `package.el` を利用してインストール可能なパッケージの中からおすすめのパッケージを紹介します。

{% for page in site.pages %}
  {% if page.package %}
    {% if page.redirect_to %}
    {% else %}

* [{{ page.title }}]({{ page.url }})
   {% for tag in page.tags %}<span class="label label-info">{{ tag }}</span> {% endfor %}
  <br>{{ page.description }}

    {% endif %}
  {% endif %}
{% endfor %}

---
layout: simple
title: 日本のEmacsユーザーのためのハブサイト
---
{% include JB/setup %}

**{{ site.title }}**はEmacsと日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。  
Emacsと{{ site.title }}についての詳細は[このサイトについて](/about)をご覧ください。

## コンテンツ

{% assign p_page_list
     = site.pages | concat: site.posts
       | where_exp: 'item', 'item.dir == "/packages/"'
       | where_exp: 'item', 'item.last_modified'
       | sort: "last_modified"
       | reverse %}

{% assign t_page_list
     = site.pages | concat: site.posts
       | where_exp: 'item', 'item.dir == "/tips/"'
       | where_exp: 'item', 'item.last_modified'
       | sort: "last_modified"
       | reverse %}

* [2020年代のEmacs入門](/tips/emacs-in-2020)
* [{{ site.title }}で管理しているパッケージ](/maintenances)
* [Emacsのバージョン](/tips/versions)
* [パッケージ紹介](/packages) <small>最近10記事</small>
{% for page in p_page_list limit:10 %}
  * [{{ page.title }}]({{ page.url }})
{%- endfor %}
<div style="margin-bottom:2rem;"></div>
* [Emacsに関する雑多な話題](/tips) <small>最近10記事</small>
{% for page in t_page_list limit:10 %}
  * [{{ page.title }}]({{ page.url }})
{%- endfor %}

## お知らせ

{% for post in site.posts limit:5 %}
* {{ post.date | date: "%Y-%m-%d" }} &raquo; [{{ post.title }}]({{ post.url }})
{%- endfor %}

## 更新情報

{% assign pages_list
     = site.pages | concat: site.posts
       | where_exp: 'item', 'item.last_modified'
       | sort: "last_modified"
       | reverse %}

{% for page in pages_list limit:10 %}
* {{ page.last_modified | date: "%Y-%m-%d" }} &raquo; <small>{{ page.dir | slice: 1,100 }}</small>[{{ page.title }}]({{ page.url }})
{%- endfor %}


## Slack <small>- [emacs-jp.slack.com](https://emacs-jp.slack.com/)</small>

Emacs JPのSlack teamには多くのEmacsユーザーが常駐しています。  
どなたでも <https://slack-emacs-jp.herokuapp.com/> からサインアップして参加できます。

Emacsについて、

* 分からないことがある
* おすすめの拡張は？
* Emacs Lispを書いたので、読んでみて欲しい
* こんな拡張はないの？
* この拡張の設定がわからない
* 他のIDEやエディタの機能をEmacsで実現するには？
* こんなカラーテーマが欲しい

……など、Slackで質問すれば解決できるかもしれません。

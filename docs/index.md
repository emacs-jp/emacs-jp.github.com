---
layout: page
title: 日本のEmacsユーザーのためのハブサイト
---
{% include JB/setup %}

**{{ site.title }}**はEmacsと日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。  
Emacsと{{ site.title }}についての詳細は[このサイトについて](/about)をご覧ください。

## コンテンツ

* [2020年代のEmacs入門](/tips/emacs-in-2020)
* [**{{ site.title }}**で管理しているパッケージ](/maintenances)
* [Emacsのバージョン](/tips/versions)
* [おすすめパッケージ紹介](/packages)
* helm とは
* いろいろな日本語入力環境
* [Linuxでのbuild方法](/build-linux)
* [EmacsとVimの機能対応表](https://docs.google.com/spreadsheets/d/184i0Cmnfd0CdmPw2AVMMvmmnl7Gz5ryPqTaxnpIyqRE/edit?usp=sharing)

## お知らせ

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date: "%Y-%m-%d" }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## 更新情報

{% assign target
     = site.pages | concat: site.posts
       | where_exp: 'item', 'item.last_modified'
       | sort: "last_modified"
       | reverse %}

{%- for post in target limit:10 %}
* {{ post.last_modified | date: "%Y-%m-%d" }} &raquo; <small>{{ post.dir | slice: 1,100 -}}</small>[{{ post.title }}]({{ post.url }})
{%- endfor -%}

{% assign target = nil %}

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

---
layout: page
title: 日本のEmacsユーザーのためのハブサイト
---
{% include JB/setup %}

**{{ site.title }}**はEmacsと日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。  
Emacsと{{ site.title }}についての詳細は[このサイトについて](/about.html)をごらんください。

## コンテンツ

* [**{{ site.title }}**で管理しているパッケージ](/maintenances/)
* [Emacsのバージョン](/tips/versions.html)
* [おすすめパッケージ紹介](/packages/)
* [Emacsビギナーのためのページ](/beginner.html)
* helm とは
* いろいろな日本語入力環境
* [Linuxでのbuild方法](/build-linux.html)
* [EmacsとVimの機能対応表](https://docs.google.com/spreadsheets/d/184i0Cmnfd0CdmPw2AVMMvmmnl7Gz5ryPqTaxnpIyqRE/edit?usp=sharing)

## お知らせ

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date: "%Y-%m-%d" }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## Slack <small>- [emacs-jp.slack.com](https://emacs-jp.slack.com/)</small>

Emacs JPのSlack teamには多くのEmacsユーザーが常駐しています。  
どなたでも <https://slack-emacs-jp.herokuapp.com/> からサインアップして参加できます。

Emacsについて、

* わからないことがある
* おすすめの拡張は？
* Emacs Lispを書いたので、読んでみて欲しい
* こんな拡張はないの？
* この拡張の設定がわからない
* ほかのIDEやエディタの機能をEmacsで実現するには？
* こんなカラーテーマが欲しい

……など、Slackで質問すれば解決できるかもしれません。

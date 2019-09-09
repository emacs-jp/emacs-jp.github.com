---
layout: page
title: 日本のEmacsユーザーのためのハブサイト
---
{% include JB/setup %}

**{{ site.title }}**はEmacsと日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。  
Emacsと{{ site.title }}についての詳細は[このサイトについて](/about.html)をごらんください。

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

## お知らせ

<ul class="posts emacs-jp-timeline">
  {% for post in site.posts %}
    <li class="emacs-jp-timeline-item">
      <div class="emacs-jp-timeline-item-tail"></div>
      <div class="emacs-jp-timeline-item-head emacs-jp-timeline-item-head-red"></div>
      <div class="emacs-jp-timeline-item-content">
        <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a><br/>
        <div class="emacs-jp-timeline-item-content-date">{{ post.date | date_to_string }}</div>
      </div>
    </li>
  {% endfor %}
</ul>

## コンテンツ

* [**{{ site.title }}**で管理しているパッケージ](/maintenances/)
* [Emacsのバージョン](/tips/versions.html)
* [おすすめパッケージ紹介](/packages/)
* [Emacsビギナーのためのページ](/beginner.html)
* helm とは
* いろいろな日本語入力環境
* [Linuxでのbuild方法](/build-linux.html)
* [EmacsとVimの機能対応表](https://docs.google.com/spreadsheets/d/184i0Cmnfd0CdmPw2AVMMvmmnl7Gz5ryPqTaxnpIyqRE/edit?usp=sharing)

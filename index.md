---
layout: page
title: Emacs JP
tagline: 日本の Emacs ユーザーのためのハブサイト
---
{% include JB/setup %}

Emacsについて,

* わからないことがある
* おすすめの拡張は ?
* elisp書いたんで, 読んでみて欲しい.
* こんな拡張はないの ?
* この拡張の設定がわからない.
* Vim, Sublime Textにあるあの機能を Emacsで実現するには ?
* こんな Color Themeが欲しい.

なんでも構いませんので, 何かございましたら [issues](https://github.com/emacs-jp/issues/issues)に登録してください. 頑張って回答します.

本サイトのリポジトリは[こちら](https://github.com/emacs-jp/emacs-jp.github.com)です。
サイト作成を行いたい等で, push権限が欲しい方は [@syohex](https://twitter.com/syohex/)までお願いします.

## このサイトについて
{{ site.title }} は Emacs と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。
Emacs と {{ site.title }} についての詳細は [こちら](/about/)をごらんください。

## Slack

[https://emacs-jp.slack.com](https://emacs-jp.slack.com)

参加したい方は [https://slack-emacs-jp.herokuapp.com/](https://slack-emacs-jp.herokuapp.com/) からどうぞ.

## お知らせ

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date: "%Y-%m-%d" }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## コンテンツ

* [{{ site.title }} で管理しているパッケージ](/maintenances/)
* [おすすめパッケージ紹介](/packages/)
* [Emacsビギナーのためのページ](/beginner/)
* helm とは
* いろいろな日本語入力環境
* [Linuxでのbuild方法](/build-linux/)

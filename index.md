---
layout: page
title: Emacs JP
tagline: 日本の Emacs ユーザのためのハブサイト
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

サイト作成を行いたい等で, push権限が欲しい方は [@syohex](https://twitter.com/syohex/)までお願いします.

## このサイトについて
{{ site.title }} は Emacs と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。
Emacs と {{ site.title }} についての詳細は [こちら](./about.html) をごらんください。

## お知らせ

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date: "%Y-%m-%d" }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>

## コンテンツ

* [{{ site.title }} で管理しているパッケージ](/maintenances/)
* [おすすめパッケージ紹介](/packages/)
* helm とは
* いろいろな日本語入力環境
* Linuxでのbuild方法
* [Macでの\\(バックスラッシュ)入力について](/osx_backslash.html)


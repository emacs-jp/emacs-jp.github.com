---
layout: layout
title: emacs-jp.github.com
---
Emacsについて,

* わからないことがある
* おすすめの拡張は ?
* elisp書いたんで, 読んでみて欲しい.
* こんな拡張はないの ?
* この拡張の設定がわからない.
* Vim, Sublime Textにあるあの機能を Emacsで実現するには ?
* こんな Color Themeが欲しい.

なんでも構いませんので, 何かございましたら [issues](https://github.com/emacs-jp/emacs-jp.github.com/issues)に登録してください. 頑張って回答します.

サイト作成を行いたい等で, push権限が欲しい方は [@syohex](https://twitter.com/syohex/)までお願いします.

## なにか書く
### なにか書いてください

## blogっぽいもの
### 書いて
<ul>
{% for post in site.posts %}
  <li>
    <a href="{{ post.url }}">{{ post.date | date_to_long_string }} : {{ post.title }}</a>
  </li>
{% endfor %}
</ul>

## りんくとか
### hoge
* piyo
* foo
* bar

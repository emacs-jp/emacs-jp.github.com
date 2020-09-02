---
layout: page
author: syohex
title: "helm-ag: The Silver Seacherの helmインタフェース"
tags: [helm]
date: 2013-03-22
last_modified: 2013-05-10
---
{% include JB/setup %}

## 概要

[helm-ag.el](https://github.com/syohex/emacs-helm-ag)は [The Silver Searcher](https://github.com/ggreer/the_silver_searcher)の [helm](https://github.com/emacs-helm/helm)インタフェースです.

The Silver Searcherは ack likeな grepツールで, ackよりも高速に検索を行うことができます.


## スクリーンショット

![helm-ag](/images/helm-ag.png)


## インストール方法

helm-agは MELPAに登録されているので, package.elを使ってインストールすることができます.


## 必要要件

* Emacs 23以降
* The Silver Searcher

MacOSXユーザーの方であれば, homebrewで The Silver Searcherをインストールすることが可能です.
その他の環境の方については, ソースコードを入手し, ビルド, インストールを行なってください.


## 各種コマンド

#### helm-ag

カレントディレクトリ以下を検索する

#### helm-ag-this-file

現在開いているファイルを検索する

#### helm-ag-pop-stack

ジャンプ前の場所に戻る

#### helm-ag-clear-stack

ジャンプ元を保存しているスタックをクリアする


## カスタマイズ

#### helm-ag-base-command(デフォルト: `ag --nocolor --nogroup`)

`ag`コマンドを実行する際のコマンドと基本オプションです.


#### helm-ag-command-option(デフォルト: `nil`)

基本コマンドの後ろに追加されるコマンドラインオプション


#### helm-ag-thing-at-point(Default: `'nil`)

この値が `non nil`である場合, カーソル以下のオブジェクトを検索パターンとして
自動的に挿入します. `helm-ag-thing-at-point`が取る値は, `thing-at-point`に
指定できるものと同じです(`'word`, `'symbol`等).


## 設定例

```common-lisp
(require 'helm-config)
(require 'helm-ag)

(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-thing-at-point 'symbol)
```

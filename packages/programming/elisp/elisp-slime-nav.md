---
layout: page
title: "elisp-slime-nav"
description: "Slime-style navigation for Emacs Lisp"
package: true
category: "elisp"
tags: ["programming","elisp"]
---
{% include JB/setup %}

## 概要

`elisp-slime-nav`は [slime](http://common-lisp.net/project/slime/)風に関数の定義元へ
ジャンプ, (定義元から)元の場所に戻る, ドキュメントの閲覧, を行うことができます.
関数の定義元へのジャンプは elispを書く際に重宝しますので, slimeに慣れていない人, 使った
ことない人でも Emacs Lispを書く人にはおすすめの拡張です.


## インストール方法

`elisp-slime-nav` MELPAに登録されているので, package.elを使ってインストールすることができます.


## 設定

`elisp-slime-nav.el`をロードすることで, `emacs-lisp-mode-hook`, `ielm-mode-hook`に
`elisp-slime-nav-mode`関数が登録されるので, 基本的には設定は不要です.
`lisp-interaction-mode-hook`等でも使いたい場合は下記を設定ファイルに追加してください.


```common-lisp
(add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)
```


## 各種コマンド

#### `elisp-slime-nav-find-elisp-thing-at-point`

関数の定義元へジャンプ. `M-.`にバインドされる

#### `pop-tag-mark`

定義元へのジャンプから元いた位置に戻る. `M-,`にバインドされる.
(`elisp-slime-nav`のコマンドでなく, `etags.el`のコマンド)

#### `elisp-slime-nav-describe-elisp-thing-at-point`

カーソル下の関数, シンボルのドキュメントを表示する.
`C-c C-d d`及び `C-c C-d C-d`にバインドされる.

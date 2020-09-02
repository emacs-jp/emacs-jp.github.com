---
layout: page
author: syohex
title: "ac-ispell: ispellの auto-complete source"
tags: [auto-complete, ispell, ac-ispell]
date: 2014-04-15
last_modified: 2014-04-15
---
{% include JB/setup %}

## 概要

[ac-ispell.el](https://github.com/syohex/emacs-ac-ispell)は ispellの `auto-complete` sourceです.
各種単語辞書の中から補完を行うことができます. キャメルケースやすべて大文字の単語の補完も行うことが
できます.


## スクリーンショット

![screenshot1](/images/auto-complete/ac-ispell/ac-ispell.png)

## 必要要件

- Emacs 23 or higher
- auto-complete


## インストール方法

`MELPA`からインストールすることができます.

```
M-x package-install ac-ispell
```

## 設定

#### `ac-ispell-requires`(Default `3`)

補完を開始する最小文字数.


#### `ac-ispell-setup`

`auto-complete` sourceのセットアップを行う. `ac-ispell-setup`を呼び出す前に
設定をする必要がある.


#### `ac-ispell-ac-setup`

ispellの auto-complete sourceを `ac-sources`に追加する.
また `auto-complete-mode`を有効にする.


## 設定例

```lisp
;; 4文字以上の場合, 補完するようにする
(custom-set-variables
  '(ac-ispell-requires 4))

(eval-after-load "auto-complete"
  '(progn
      (ac-ispell-setup)))

(add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
(add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
```

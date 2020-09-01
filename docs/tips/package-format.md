---
layout: page
author: syohex
title: "パッケージフォーマット"
description: "Package Format"
category: "package"
tags: ["package", "tips"]
date: 2013-10-11
last_modified: 2013-10-12
---
{% include JB/setup %}

## 概要
Emacs Lispのパッケージを配布する際は, 公式のパッケージフォーマットに
従うべきです. ここではパッケージのフォーマットについて示します.


## 1行目はパッケージファイル名とパッケージの説明を書く
1行目には, パッケージ名とパッケージの説明を書きます. バッファローカル変数を
1行目に書いているものが稀にありますが, それは誤りです(ただし `lexical-binding`のみ例外).


以下に [anzu.el](https://github.com/syohex/emacs-anzu)を例として示します.

```common-lisp
;;; anzu.el --- Show number of matches in mode-line while searching
```

パッケージ名と説明の間のハイフンは 3つです. 2つや 4つではありませんので
注意してください. パッケージの説明は `package-list-packages`で表示される
パッケージの概要となりますので, 手抜きせずわかりやすい説明を考えてつけましょう.


## メタ情報
メタ情報は `Author`, `Version`, (依存があれば)`Package-Requires`を必ず書くように
しましょう(あとは `URL`もあると良いです).

以下に `helm-gtags.el`を例として示します

```common-lisp
;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-gtags
;; Version: 0.9.9
;; Package-Requires: ((helm "1.0"))
```

## Commentaryセクション
Commentaryセクションはパッケージの説明, 簡単な利用方法, 設定方法について示します.
このセクションの情報は `package-list-packages`で `?`で見ることができます.
以下に `deferred.el`の Commentaryセクションの内容を示します.

```common-lisp
;;; Commentary:

;; 'deferred.el' is a simple library for asynchronous tasks.
;; [https://github.com/kiwanami/emacs-deferred]

;; The API is almost the same as JSDeferred written by cho45. See the
;; JSDeferred and Mochikit.Async web sites for further documentations.
;; [https://github.com/cho45/jsdeferred]
;; [http://mochikit.com/doc/html/MochiKit/Async.html]

;; A good introduction document (JavaScript)
;; [http://cho45.stfuawsc.com/jsdeferred/doc/intro.en.html]
```


## バッファローカル変数の宣言
上述の `lexical-binding`を除いたものについては, 後述のファイルの末尾を示すコメントの
前に宣言すると良いでしょう.
以下に `helm-mode.el`を例として示します.

```common-lisp
;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mode.el ends here
```

### `lexical-binding`を定義する場合

`lexical-binding`は現在のところ, 1行目に定義しなければ意味がありません.
そのため `lexical-binding`を設定する場合は, 1行目のパッケージの説明に続けて書くように
してください. 以下は `flycheck.el`の例です.

```common-lisp
;;; flycheck.el --- On-the-fly syntax checking (Flymake done right) -*- lexical-binding: t; -*-
```

## ファイル末尾

ファイルの最後は `;;; ファイル名 ends here`とします. 以下に例を示します

```common-lisp
;;; quickrun.el ends here
```


## 補足
これらをいちいち気にして書くのは大変なので, パッケージを書く際は[yasnippet](https://github.com/capitaomorte/yasnippet)や
[autoinsert](http://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html)機能を使い, ひな形から
作成するのが良いでしょう.

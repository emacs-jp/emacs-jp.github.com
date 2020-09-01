---
layout: page
author: syohex
title: "環境変数の設定"
description: "環境変数の設定"
category: "shell"
tags: ["shell"]
date: 2013-10-13
last_modified: 2013-10-13
---
{% include JB/setup %}

## 概要
Emacsでの環境変数の設定方法について示します. 特に PATHに関する設定は複雑なので,
PATHの設定について詳しく述べます.


## 対象
対象は Macと Linux等の Unix系の環境でシェル以外から Emacsを立ち上げる方です.
後述の通り, シェルから Emacsを立ち上げる場合は, シェルの環境変数が引き継がれるので,
Emacs側で環境変数を設定する必要はありません. Windowsを使う場合も同様で, システムの
環境変数の設定が行えていれば, Emacs側で設定する必要はありません.


## 環境変数を設定する
環境変数を使う場合は `setenv`関数を使います. `setenv`は 2引数関数で, 第 1引数は
環境変数名で, 第 2引数はその値となります. また型は第 1, 2引数共に文字列です.


以下に例を示します(環境変数 `FOO`に値 `VALUE`を設定).

```common-lisp
(setenv "FOO" "VALUE")
```


## 環境変数 PATHの設定
`PATH`以外については, 上述の `setenv`をするだけで設定完了ですが, `PATH`については
`exec-path`変数の設定も行う必要があります. シェルを使わずコマンドを起動する場合
(`call-process`等)は, `exec-path`変数の値が参照されます. `PATH`は設定したのに,
コマンドが見つからない, 期待するものと違うものが利用されてしまうといった場合は,
`exec-path`が正しく設定されているかどうかを確認してください.


`exec-path`は, 文字列のリストです. 環境変数 `PATH`のコロン区切りとは違うので,
注意してください. 以下に設定例を示します. `parse-colon-path`関数は, コロン区切りの
文字列を分割し, リストとして返す関数です.


```common-lisp
(setq exec-path (parse-colon-path (getenv "PATH")))
```


### eshellの PATHの設定
`eshell`での `PATH`の設定は `eshell-path-env`により行います. 形式は環境変数 `PATH`と
同じですので, `PATH`の値をそのまま代入すると良いです.

```common-lisp
(setq eshell-path-env (getenv "PATH"))
```


## exec-path-from-shellを使って環境変数を設定する
シェルと Emacsで共に環境変数を設定すると重複が生じ, 一方の設定忘れが発生してしまう恐れが
あります. [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)はそのような問題を
解決してくれるパッケージです. 利用したい環境変数を指定するだけでその環境変数の設定を
行なってくれます. また上述の `PATH`周りの設定もすべて行なってくれます.


以下に利用例を示します.
```common-lisp
(exec-path-from-shell-copy-envs '("PATH" "VIRTUAL_ENV" "GOROOT" "GOPATH"))
```

`exec-path-from-shell-copy-envs`関数に設定したい環境変数名のリストを指定すると,
その環境変数の設定を行なってくれます.


`exec-path-from-shell`のインタフェースは, 一度の呼び出しで一回シェルを起動するので,
リストでまとめて渡すことで, シェルの起動回数を減らすことができます.


## (補足)シェルから Emacsを起動する
`-nw`や `--no-window`オプションで起動した場合や GUI版でもシェルから起動した場合は
シェルの環境変数がすべて Emacsに引き継がれるので, 明示的に環境変数を設定する必要が
なくなります. 設定忘れなどを一切気にしたくない方はこちらの方法がおすすめです.

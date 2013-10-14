---
layout: page
title: "anzu"
description: "検索情報をモードラインに表示"
package: true
category: "mode-line"
tags: ["mode-line", "search"]
---
{% include JB/setup %}

## 概要

[anzu.el](https://github.com/syohex/emacs-anzu)はモードラインに検索コマンド
実行中に, 現在の入力にマッチするバッファ内の語数と現在のマッチ位置を表示する拡張です.
元ネタは [@manga_osyo](https://twitter.com/manga_osyo)さんの [vim-anzu](https://github.com/osyo-manga/vim-anzu)です.


`anzu.el`は migemo対応しており, 日本語のインクリメンタル検索でも利用することができます.


## スクリーンショット
モードラインの左端に注目してください

![anzu-gif](https://github.com/syohex/emacs-anzu/raw/master/image/anzu.gif)


## インストール方法

anzuは MELPAに登録されているので, package.elを使ってインストールすることができます.


## 必要要件

* Emacs 24 or higher

Emacs23では, 動作しません.


## 各種コマンド

### global-anzu-mode

`global-anzu-mode`の有効/無効の切り替え. 常に anzuを有効にしたい場合は,
以下の設定を, `init.el`等に追加してください

```common-lisp
(global-anzu-mode +1)
```

### anzu-mode

マイナーモード `anzu-mode`の有効/無効の切り替え. あまりないと思いますが,
特定のモードで利用したいというような場合は, 各種モードの hookでこの関数を
実行するようにしてください.


## migemo対応
デフォルトでは migemo対応は無効化されていますので, migemoを利用している
場合は, `anzu-use-migemo`に `t`を設定してください.


## 低速化を回避するための設定
`anzu.el`は検索に先立ち, 現在の入力がバッファでいくつの語にマッチするかを
数え上げます. そのため, バッファが巨大で, マッチする数が膨大な数になる場合,
どうしても低速化してしまいます. 特に migemoでは顕著です.
`anzu.el`では, *最少入力文字数*と *最大検索数*により, 低速化を回避することが
できます. 具体的な設定については後述の「カスタマイズ」章で示す.


*最少入力文字数*は anzu情報を表示する最少の文字数です(デフォルト: 1).
この値を少し大きめに設定することで, 1-2文字入力段階での膨大なマッチ数の数え上げを
回避することができます.


*最大検索数*は, それ以上マッチ数の数え上げを行わないというしきい値です.
例えば最大検索数に 1000を設定しているときに, 現在の入力にマッチする数が, 2000個
あった場合, 1000個マッチ数ものを見つけた時点で数え上げを打ち切ります. そのため,
一定時間で計算を打ち切ることができ, 低速化を招く原因を回避することができます.
一方でマッチ数を正確に求めることができませんし, カーソル位置が最大検索数以降の位置に
ある場合, 正確な位置も表示することができないといった問題もあります(以下の画像のように,
1000個以上あるといった表現で示される).


[anzu-threshold](https://github.com/syohex/emacs-anzu/blob/master/image/anzu-threshold.png)


## カスタマイズ

### anzu-mode-line-update-function(デフォルト: `nil`)

モードラインの表示を更新する関数を指定します. デフォルトの設定を変えたいという
場合は, この変数に更新用の関数を設定してください. 以下に更新関数の例を示します.

```common-lisp
(defun my/anzu-update-func (here total)
  (propertize (format "<%d/%d>" here total)
              'face '((:foreground "yellow" :weight bold))))
(setq anzu-mode-line-update-function 'my/update-func)
```

第 1引数は現在の位置, 第 2引数はトータルのマッチ数です.


### anzu-cons-mode-line-p(デフォルト: `t`)
anzu情報をモードラインに示すかどうかを意味する変数です.
デフォルトでは, モードラインの左端に anzu情報を表示しますが, 自分な
好きな位置に表示したいというような場合は, この変数に `nil`を設定してください.


### anzu-use-migemo(デフォルト: `nil`)
migemoを利用するかどうかを示すフラグ. 利用する場合は, `t`を設定してください

```common-lisp
(setq anzu-use-migemo t)
```

### anzu-search-threshold(デフォルト: `nil`)
最大検索数. `nil`の場合は, マッチするものすべて数え上げます.

```common-lisp
;; 入力に 1000個以上マッチする場合はそれ以上数え上げを行わない
(setq anzu-search-threshold 1000)
```


### anzu-minimum-input-length(デフォルト: 1)
最少入力数.

```common-lisp
;; anzu情報の表示は 3文字以上入力した場合にする
(setq anzu-minimum-input-length 3)
```


### anzu-regexp-search-commands
正規表現を入力とする検索コマンドのリストです. 入力が正規表現かどうかを判定する
ために利用します. 基本的には設定不要ですが, もし独自の正規表現を入力とする検索関数を
利用しているという場合は, このリストにその関数を追加してください.


デフォルト値は `'(isearch-forward-regexp isearch-backward-regexp)`です.


## face

### anzu-mode-line
モードラインの anzu情報の face.

```common-lisp
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
```

## 設定例

```common-lisp
(require 'anzu) ;; package.elからインストールした場合は不要

(global-anzu-mode t)
(setq anzu-search-threshold 1000)

;; migemoを利用している場合
(setq anzu-use-migemo t)
```

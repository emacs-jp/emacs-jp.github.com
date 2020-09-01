---
layout: page
title: "helm-gtags"
description: "GNU Globalの helmインタフェース"
category: "helm"
tags: ["helm","tagjump"]
---
{% include JB/setup %}

## 概要

[helm-gtags.el](https://github.com/syohex/emacs-helm-gtags)は GNU globalの [helm](https://github.com/emacs-helm/helm)インタフェースです.

`helm-gtags.el`は `anything-gtags.el`と互換性はありませんが, より高速に動作するように設計されています.

`anything-gtags.el`は `gtags.el`の出力結果を加工した上で anythingインタフェースを
提供するため, Linuxカーネル, Android等の非常に大規模なソースツリー下で利用した場合,
表示までに時間を要することがあります. `helm-gtags.el`は `gtags.el`を利用せず,
自前で GNU globalの結果を処理する分処理が高速化されます.


GNU Globalに関する説明は[こちら](http://www.gnu.org/software/global/)を参照してください


## スクリーンショット

![helm-gtags](/images/helm/helm-gtags/helm-gtags.png)


## 必要要件

* Emacs 23以降
* helm 1.0以降
* GNU Global 5.7.1以降

GNU Globalは 5.9で高速化が行われているので, 5.9以降の利用を推奨します.


## 各種コマンド

#### helm-gtags-mode

`helm-gtags-mode`を有効にする

#### helm-gtags-find-tag

入力されたタグの定義元へジャンプ

#### helm-gtags-find-rtag

入力タグを参照する場所へジャンプ

#### helm-gtags-find-symbol

入力したシンボルを参照する場所へジャンプ

#### helm-gtags-find-files

入力したファイルを開く

#### helm-gtags-select

タグ一覧からタグを選択し, その定義元にジャンプする(複数アクションあり)

#### helm-gtags-pop-stack

ジャンプ前の場所に戻る

#### helm-gtags-clear-stack

ジャンプ元を保存しているスタックをクリアする


## カスタマイズ

#### helm-gtags-path-style

候補を表示する際の表示形式. 以下の中から選択する(デフォルトは `'root`)

* `'root`(タグファイルがあるディレクトリを頂点としたパス)
* `'absolete`(絶対パス)
* `'relative`(カレントディレクトリからの相対パス)


#### helm-gtags-ignore-case

タグから検索を行う際, 大文字小文字を無視する(デフォルトは `nil`)

#### helm-gtags-read-only

タグ検索で選択したファイルを *read-only*で開く


## 設定例

```common-lisp
(require 'helm-config)
(require 'helm-gtags)

(add-hook 'c-mode-hook 'helm-gtags-mode)

;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))
```

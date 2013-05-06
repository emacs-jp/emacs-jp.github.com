---
layout: page
title: "helm-descbinds"
description: "describe-bindingsの helmインタフェース"
package: true
category: "helm"
tags: ["helm"]
---
{% include JB/setup %}

## 概要

`helm-descbinds.el`は `describe-bindings`の helmインタフェースです.
Emacsはデフォルトでも膨大なキーバインドが存在するため, `describe-bindings`を使っても
目的のコマンドを見つけ出すのはなかなか大変ですが, helmインタフェースを利用することで
コマンドの検索が比較的容易になります.


また`helm-descbinds.el`は以下の機能が提供されます

* コマンドの実行
* ドキュメントの表示
* 定義元への移動


persistent-action(`C-z`)はドキュメントの表示となっています.


## スクリーンショット

![helm-descbinds](/images/helm/helm-descbinds/helm-descbinds.png)


バインドされたコマンドを検索する際は, キー(`C-c`等)とコマンド名を意識して検索すると良いでしょう.


## インストール方法

helm-descbindsは MELPAに登録されているので, package.elを使ってインストールすることができます.


## 必要要件

* Emacs 23以上
* helm


## 設定

helm-descbinds-modeを実行することで, `describe-bindings`のキーバインドである `C-x b`が上書きされ, `helm-descbinds`になります.

```common-lisp
(require 'helm-descbinds)
(helm-descbinds-mode)
```

Emacs 23をお使いの方は `helm-descbinds-mode`の部分を以下のように変更してください

```common-lisp
(helm-descbinds-mode 1)
```

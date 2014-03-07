---
layout: page
title: package.el
description: "パッケージ管理ツール"
package: true
category: "package"
tags: ["package"]
---
{% include JB/setup %}

## 概要

`package.el`について示す.

## インストール

Emacs 24では標準添付されているので, Emacs 24以降を使用している場合は,
インストールは不要である.

### Emacs 23ユーザーの方

[http://bit.ly/pkg-el23](http://bit.ly/pkg-el23)から Emacs23向けの package.elをダウンロードし,
load-pathの通ったディレクトリに配置してください.

## 各リポジトリについて

### [GNU ELPA](http://elpa.gnu.org/packages/)

公式のリポジトリ. デフォルトの設定では, このリポジトリしか利用することはできない.
登録に手間がかかるためか, 登録されているパッケージ数は少なめであるが, 他の
リポジトリに比べると登録されているパッケージの信頼度は高い.

### [Marmalade](http://marmalade-repo.org/packages/)

非公式のリポジトリ. アカウントを作れば誰でもパッケージを登録することが可能.
MELPAにも登録されているパッケージの場合, 安定版のパッケージを Marmaladeにアップロードすると
いう使われ方が多い.

### [MELPA](http://melpa.milkbox.net/packages/)

非公式のリポジトリ. [githubのリポジトリ](https://github.com/milkypostman/melpa)に pull requestを
送ることで, パッケージを登録することができる. リポジトリに変更がある度に更新されるので, 登録されて
いるパッケージの最新版をインストールすることができる(stable版もインストールできるようにする流れが
あるが 2013年 9月 24日時点で未実装).


## リポジトリの登録

`package-archives`変数(連想リスト)にリポジトリ情報が格納されている.
新規に追加する場合は `("リポジトリ名" . "URL")`をこの変数に `add-to-list`すればよい.


## 設定例

```common-lisp
(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))

;; 初期化
(package-initialize)
```

## 利用方法

### `M-x package-install`

指定したパッケージをインストールする.


### `M-x package-refresh-contents`

パッケージ情報を更新する. 最新のものをインストールしたい場合は, 事前に実行して置くと
良い. 後述の `package-list-packages`からインストールを行う場合は不要.

### `M-x package-list-packages`

パッケージ操作バッファを開く.

![package-list-packages](/images/package-manager/package-list-packages.png)


#### `package-list-packages`での操作一覧

<table class="table-striped table-bordered table-condensed">
<thead>
<tr><th>キー</th><th>概要</th></tr>
</thead>
<tbody>
<tr><td style="text-align: center">`r`</td><td>パッケージ一覧の更新</td></tr>
<tr><td style="text-align: center">`x`</td><td>マークの実行</td></tr>
<tr><td style="text-align: center">`U`</td><td>Upgrade</td></tr>
<tr><td style="text-align: center">`i`</td><td>インストールマークをつける</td></tr>
<tr><td style="text-align: center">`d`</td><td>削除マークをつける</td></tr>
<tr><td style="text-align: center">`~`</td><td>古いパッケージに削除マークをつける</td></tr>
<tr><td style="text-align: center">`u`</td><td>マークを除去する</td></tr>
<tr><td style="text-align: center">`?`</td><td>パッケージの概要を表示</td></tr>
<tr><td style="text-align: center">`n`</td><td>次の行へ移動</td></tr>
<tr><td style="text-align: center">`p`</td><td>前の行へ移動</td></tr>
<tr><td style="text-align: center">`h`</td><td>help</td></tr>
<tr><td style="text-align: center">`q`, `z`</td><td>終了</td></tr>
</tbody>
</table>

<hr />

#### NOTE

`package-list-packages`では `package-refresh-contents`がまず行われるので,
パッケージ更新の時間を要する. パッケージ情報が既に更新済み等の場合は,
パッケージ情報の更新を行わない `package-list-packages-no-fetch`を実行すると良い.


## package.elを使ったパッケージ管理例

package.elを使ったパッケージ管理方法について示す.

`(package-installed-p package-name)`で `package-name`がインストール済みで
あるかを確認することができるので, `package-installed-p`が `nil`を返すパッケージを
インストールすれば, 同じ環境を構築することができる.


以下のようなファイルを用意し, `M-x eval-buffer`を実行することで, インストールされて
いないパッケージをインストールすることができる.


```common-lisp
(require 'package)
;; MELPAのみ追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; パッケージ情報の更新
(package-refresh-contents)

;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    ;;;; for auto-complete
    auto-complete fuzzy popup pos-tip

    ;;;; buffer utils
    popwin elscreen yascroll buffer-move

    ;;;; flymake
    flycheck flymake-jslint

    ;;;; go
    go-mode

    ;;;; python
    jedi

    ;;;; helm
    helm

    ;;;; git
    magit git-gutter
    ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
```


#### NOTE

`package-installed-p`関数はあくまでインストールしているかどうかを
知らせるだけなので, アップグレードできるかどうかを検知できるわけではない.


### packageの更新

`package-list-packages`を実行し, `U`, `x`とすることで, インストール済みの
パッケージをすべて upgradeできる.


## 関連 URL
* [packages](http://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html)
* [el-get](https://github.com/dimitri/el-get)

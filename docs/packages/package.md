---
layout: page
author: shohex
title: package
description: "パッケージ管理ツール"
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

[http://bit.ly/pkg-el23](http://bit.ly/17aWZEw)から Emacs23向けの package.elをダウンロードし,
load-pathの通ったディレクトリに配置してください.

## 各リポジトリについて

### [GNU ELPA](http://elpa.gnu.org/packages/)

公式のリポジトリ. デフォルトの設定では, このリポジトリしか利用することはできない.
登録に手間がかかるためか, 登録されているパッケージ数は少なめであるが, 他の
リポジトリに比べると登録されているパッケージの信頼度は高い.

### [MELPA](https://melpa.org/)

非公式のリポジトリ. [githubのリポジトリ](https://github.com/melpa/melpa)に pull request
を送ることで, パッケージを登録することができる. リポジトリに変更がある度に更新されるので,
登録されているパッケージの最新版をインストールすることができる.

### [MELPA-stable](https://stable.melpa.org/)

安定版 MELPA. 最新のリビジョンでなく, 最新のタグのバージョンが取得できる.

推奨されていない. (参考: [melpa/melpa - MELPA Stable](https://github.com/melpa/melpa#melpa-stable))

### [Org](http://orgmode.org/elpa.html)

最新版 Org modeのリポジトリ.


## リポジトリの登録

`package-archives`変数(連想リスト)にリポジトリ情報が格納されている.
新規に追加する場合は, この変数を `("リポジトリ名" . "URL")`のリストで上書きすればよい.


## 設定例

```common-lisp
(require 'package)

;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

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

MELPAは頻繁に更新が行われているため, 手元のキャッシュが古くなり, インストールに失敗することがよくある.
その場合,  `M-x package-refresh-contents` を行った後で再度インストールを実行すると良い.

なお, 後述の `leaf` を使う場合, この作業を自動で行うので手元のキャッシュが古いことは考えなくて良い.

## package.elを使ったパッケージ管理例

package.elを使ったパッケージ管理方法について示す.

`(package-installed-p package-name)`で `package-name`がインストール済みで
あるかを確認することができるので, `package-installed-p`が `nil`を返すパッケージを
インストールすれば, 同じ環境を構築することができる.


以下のようなファイルを用意し, `M-x eval-buffer`を実行することで, インストールされて
いないパッケージをインストールすることができる.


```common-lisp
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
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

## leafを使ったパッケージ管理例
[leaf.el](https://github.com/conao3/leaf.el)は[@conao3](https://twitter.com/conao_3)さんによって開発されたパッケージである.
パッケージ設定で良く使われる「イディオム」をラップし, 宣言的にパッケージの設定を行うことができる.

多数のキーワードが実装されているが, この項ではpackage.elに関連するキーワードである`:ensure`キーワードを解説する.

キャッシュがない場合やキャッシュが古い場合のみ更新を行い, 不必要なパッケージ情報の更新は行わない.

(M)ELPAからインストールしたいパッケージに `:ensure t` をつける. それだけで`leaf`が適切なS式を生成してくれる.

```common-lisp
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

(leaf auto-complete :ensure t)
(leaf magit
  :ensure t
  :custom ((magit-refresh-verbose . t)))
```

## packageの更新

`package-list-packages`を実行し, `U`, `x`とすることで, インストール済みの
パッケージをすべて upgradeできる.


## 関連 URL
* [packages](http://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html)
* [el-get](https://github.com/dimitri/el-get)

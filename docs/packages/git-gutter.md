---
layout: page
author: syohex
title: "git-gutter.el"
description: "GitGutterの Emacs版"
category: "vcs"
tags: ["vcs"]
date: 2013-03-17
last_modified: 2020-06-05
---
{% include JB/setup %}

## 概要

[git-gutter.el](https://github.com/syohex/emacs-git-gutter)は Sublime Textの [GitGutter](https://github.com/jisaacks/GitGutter)の Emacs版です.
導入することで前回の変更点からの差分を視覚的に確認することができるようになります.


## スクリーンショット

![screenshot1](/images/vcs/git-gutter/git-gutter-default.png)


## インストール方法

`git-gutter.el`は MELPAに登録されているので, Emacs24以降を利用している場合は,
package.elを使って簡単にインストールすることができます. また el-getのレシピとしても登録されているので,
レシピを自分で書くことなく, インストールすることができます.


```
M-x package-refresh-contents
M-x package-install git-gutter
```

手動でのインストールは推奨しませんが, 単一ファイルの構成なのでインストールは容易に行うことが
できます.


## 必要要件

* Emacs 23以降
* Git 1.7.0以降


## 設定

以下は package.elもしくは el-getでインストールした場合を想定しています.
手動でインストールした場合は, `load-path`の設定及び `git-gutter.el`のロードを行う必要があります.


### global-minor-mode

`git-gutter.el`が提供するグローバルマイナーモード(`global-git-gutter-mode`)を有効にすると,
git管理下のファイルについてすべて `git-gutter-mode`を有効にします. git管理下のファイルに
ついて常時利用するという場合は, グローバルマイナーモードを有効にするのが良いでしょう.

グローバルマイナーモードを有効にするには, 設定ファイルに以下を追加します.

```common-lisp
(global-git-gutter-mode t)
```

### minor-mode

あまりないと思いますが, 特定のモードで `git-gutter`を有効にしたい場合は,
該当するモードの hookに `git-gutter-mode`を追加してください.

```common-lisp
(add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'python-mode-hook 'git-gutter-mode)
```

## 更新のタイミング

差分情報の表示が更新されるのは以下のタイミングです.

* after-save-hook
* after-revert-hook
* window-configuration-change-hook


定期的に更新したい方は, [auto-save-buffers](http://0xcc.net/misc/auto-save/)などと併用すると良いでしょう.
またバッファ切替時に更新されるので, magitと相性が良いです. magitの操作を
行った後, 元のバッファに戻ることで差分情報が更新されます.


### git-gutter.elの動作が重く感じられるとき

デフォルトでは, 期待通りに差分情報の表示が更新されるように, かなりの頻度で `git diff`コマンドが
実行されます. そのため, Linuxカーネル等の大規模なソースツリー下においては, 動作が重く感じる
ことがあると思います. 現状 windowの更新を行うと必ず呼ばれるので, 画面をたくさんの windowに
分割している場合は特にそう感じると思います.


#### 更新頻度を下げる
`window-configuration-change-hook`経由で更新関数が呼ばれることが多いので,
これを更新ポイントから除去することで大幅に `git diff`が呼ばれる回数が減ります.
`window-configuration-change-hook`を除去する場合は以下を設定ファイルに
追加してください. 表示されるものが意図したものと違う場合は手動で `M-x git-gutter`を
実行してみてください

```common-lisp
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
```

`git-gutter:update-hooks`には好きな hookが登録できるので, さらに
除去することや別途追加することも可能です.


#### git-gutter-modeを利用しない

自動でなく, 更新して欲しいと思ったタイミングで `git-gutter`を実行するという
スタイルにすることで, `git diff`の呼び出しを最少化できます.


## 各種コマンド

### git-gutter

差分情報を更新します. `git-gutter-mode`を有効にしている場合は不要です.

### git-gutter:next-hunk

次の差分があるブロックに移動します. 現在が最後の差分ブロックの場合は, 先頭の差分ブロックに移動します.
prefix argumentを使うことで, 指定した数だけ移動します.

### git-gutter:previous-hunk

前の差分ブロックに移動します. `git-gutter:next-hunk`の逆方向版です.


### git-gutter:popup-hunk

現在の差分ブロックの内容をポップアップ(`pop-to-buffer`)します. 元のコードと現在のコードの
差を見たい場合に利用します.


`git-gutter:popup-hunk`でバッファをポップアップした状態で,
`git-gutter:next-hunk`, `git-gutter:previous-hunk`を実行するとポップアップされた
バッファの内容も連動して更新されます.


### git-gutter:revert-hunk

現在の差分ブロックの修正を更新前の状態に戻します. 間違って戻してしまった場合でも
undoを利用することで取り消せます.


### git-gutter:toggle

差分情報の表示/非表示を切り替えます.


## カスタマイズ

### 表示する文字, 色の変更

<table class="table-striped table-bordered table-condensed">
<thead>
<tr><th>概要</th><th>記号</th><th>face</th></tr>
</thead>
<tbody>
<tr><td>追加</td><td>git-gutter:added-sign(Default '+')   </td><td>git-gutter:added      </td></tr>
<tr><td>削除</td><td>git-gutter:deleted-sign(Default '-') </td><td>git-gutter:deletedline</td></tr>
<tr><td>変更</td><td>git-gutter:modified-sign(Default '=')</td><td>git-gutter:modified   </td></tr>
</tbody>
</table>

以下はその設定例となります. 文字数に特に制限はありません.

```common-lisp
(setq git-gutter:added-sign "++")
(setq git-gutter:deleted-sign-sign "--")
(setq git-gutter:modified-sign "  ") ;; 空白 2つ
(set-face-foreground 'git-gutter:added  "green")
(set-face-foreground 'git-gutter:deleted  "yellow")
(set-face-background 'git-gutter:modified "magenta")
```


### 全角幅文字の設定

全角幅文字を記号として使う場合, `git-gutter:window-width`で明示的に幅を指定することを
推奨します. 一部全角幅の文字は Emacsの持つ関数で計算できるのですが, 多くの文字では
計算を誤ってしまうためです. 全角幅文字を signとして指定する例を以下に示します.


```common-lisp
(setq git-gutter:window-width 2)
(setq git-gutter:modified-sign "⇔")
(setq git-gutter:added-sign "⇒")
(setq git-gutter:deleted-sign "⇐")
```

そのときのスクリーンショットは以下のようになります.

![screenshot1](/images/vcs/git-gutter/git-gutter-fullwidth.png)


## linum-modeと併用について

`git-gutter.el`は linum-modeと同じ方法を使って差分情報を表示しているので,
`linum-mode`と併用することが現状出来ません. `linum-mode`と併用したい, と
考えられている方には [git-gutter-fringe](https://github.com/syohex/emacs-git-gutter-fringe)があります.
`git-gutter-fringe.el`はフリンジに表示する以外は `git-gutter.el`と同様です.
カスタマイズ性がやや低いですが, `linum-mode`とは干渉しないので, 併用して
利用することができます.



## 質問, バグレポートについて

質問, バグレポート, 要望等については [githubの issues](https://github.com/syohex/emacs-git-gutter/issues)までにお願いします.


---
layout: page
title: "パッケージの管理方法"
description: "Package Management"
---
{% include JB/setup %}


## パッケージマネージャ
### package.el
* Emacs24から標準添付
* Emacs23でも別途インストールすることで利用可能
* ELPA(本家, marmalade, MELPA)にあるパッケージしか基本インストールできない(ただし不可能ではない)
* UIから比較的簡単に操作できる
* 更新削除はできるが状態管理(リストの保存, リストア)はできない
* インストール前後に任意の処理ができない
* HTTPが使えればインストールできる
* バージョン指定でインストールはできない
* 外部ツールはインストールできない(同じリポジトリのファイルなら可能)

### [el-get](https://github.com/dimitri/el-get)
* ELPAを含むあらゆるソースを管理できる
* 標準添付ではない
* package.elよりは管理(記述)が複雑
* 更新削除も可能。状態管理は使い方次第
* インストール前後に任意の処理を追加できる
* 現状 Windowsでの利用に難あり
* インストールにVCSを使える必要がある
* (一部)バージョン指定でインストールすることもできる
* 外部ツールもインストールできる
* その他様々な機能あり

### auto-install
* 以前から使われているパッケージインストーラ
* URLがあればインストールを自動化
* パッケージの管理機能(更新,削除,状態管理)は無い
* 現在はpackage.elやel-getの利用が推奨される

## ラッパーパッケージ
### [Cask](https://github.com/cask/cask)
* package.elのラッパー
* パッケージ間の依存関係を記述可能
* 状態管理できる(むしろそれが主目的?)

### [bundle.el](https://github.com/tarao/bundle-el)
* el-getのラッパー
* el-getの記述をシンプルにしたもの
* 状態管理も容易

## 参考リンク
* [Issue #31](https://github.com/emacs-jp/emacs-jp.github.com/issues/31)

---
layout: page
author: syohex
title: "helm-open-github"
description: "Githubユーティリティ"
category: "helm"
tags: ["helm", "github"]
date: 2014-03-02
last_modified: 2016-09-04
---
{% include JB/setup %}

## 概要

[helm-open-github.el](https://github.com/syohex/emacs-helm-open-github)は Githubの各種 URL(コミット, ファイル, issues, pull request)を開くためのユーティリティです. ブラウザを操作することなく, 現在開いているバッファから, 目的のページをブラウザで表示することができます.


## スクリーンショット

![helm-open-github](/images/helm/helm-open-github/helm-open-github.png)


## インストール

MELPAから package.elを使ってインストールすることができます.


## 必要要件

* Emacs 24以降
* [helm](https://github.com/emacs-helm/helm) 1.0以降
* [gh.el](https://github.com/sigma/gh.el) 1.0移行



## 各種コマンド

### `helm-open-github-from-commit`

**コミット ID**から該当するページを開く


### `helm-open-github-from-file`

**ファイル名**から該当するページを開く


### `helm-open-github-from-issues`

**Issue ID**から該当するページを開く


### `helm-open-github-from-pull-requests`

**Pull Request ID**から該当するページを開く


## カスタマイズ

#### helm-open-github-commit-limit

`helm-open-github-from-commit`で表示する **issues**の上限(デフォルト `100`)


## 設定例

```lisp
(require 'helm-open-github)
(global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
(global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
(global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
(global-set-key (kbd "C-c p p") 'helm-open-github-from-pull-requests)
```

---
layout: page
title: "Building Emacs for Linux"
description: "Linuxでのビルド方法"
redirect_to: /tips/install-emacs
---
{% include JB/setup %}

## Ubuntu, Debian の場合
Ubuntu 14.04 LTSでのemacs25のビルド方法を説明します。

### 必要なパッケージのインストールとソースコードの取得

Terminalアプリを開き、以下を実行します。

```shell
apt install wget make gcc libncurses-dev git autoconf automake texinfo
git clone git://git.savannah.gnu.org/emacs.git
```

GUI版など機能の拡張が必要な場合は下記を実行します。

```shell
apt-get build-dep emacs24
```

### コンパイル

```shell
cd emacs
make
```

### インストール

```shell
make install
```

### 拡張機能の確認方法

```shell
emacs -Q --batch --eval '(message "%s" system-configuration-features)'
```

## Arch の場合

<!--
This file has been left for redirection.
Please do not add any content.
Redirect to /tips/install-emacs.
This file will be deleted after 6 month (2020/03/01).

;; Local Variables:
;; buffer-read-only: t
;; End:
-->

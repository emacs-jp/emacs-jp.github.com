# Linuxでのビルド方法

Ubuntu 14.04 LTSを使った場合のビルド方法を説明します。

「14.04のemacs24パッケージではmagitを使えないので自分でビルドする必要がある」
そんな方をターゲットとしています。

## 必要なパッケージのインストールとソースコードの取得

Terminalアプリを開き、以下を実行します。

```shell
apt install wget make gcc libncurses-dev
wget http://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-24.5.tar.gz
tar xf emacs-24.5.tar.gz
```

GUI版などの拡張が必要な場合は下記を実行します。

```shell
apt-get build-dep emacs24
```

## コンパイル

```shell
cd emacs-24.5
./configure
make
```

## インストール

```shell
make install
```

## インストール後の拡張機能の確認について

Emacsの場合 Vimでの 「```vim --version``` による機能が有効か否かの確認」に相当するものは用意されておらず
「機能に関連する関数や変数が定義されているかを見る」しかないようです。

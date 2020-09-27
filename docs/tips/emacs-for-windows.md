---
layout: page
author: mopemope
title: "GNU Emacs for Windows再入門"
date: 2020-09-28
last_modified: 2020-09-28
---

{% include JB/setup %}

## はじめに

名だたるハッカーに愛されている Emacs、自分も使ってみたいと思う人は多いことでしょう。

しかし、Emacs は GNU/Linux における基本的なソフトウェアという位置づけから、入門するだけでも初心者には敷居が高く感じられるかも知れません。特に Windows ユーザーからすると Emacs を使うにはいろいろと大変そうだとイメージがあるかも知れません。

そこで今回は 2020 年における GNU Emacs for Windows 再入門として Windows ユーザー向けに Emacs の環境構築を解説していきたいと思います。
これから Emacs を使ってみよう、既にある Emacs の環境を見直してみようという Windows ユーザーの方の参考になれば幸いです。

## Windows で Emacs をセットアップする事は本当に大変なのか？

Emacs が Windows ユーザーに優しくないような噂をよく耳にしますが果たして本当なのでしょうか？
確かに Emacs と Windows は相性が悪そうです。Emacs は Unix のツールをユーティリティとして使っている事は確かです。
過去には、Emacs を使用する以前にインストール、セットアップの時点のハードルが高く入門前に挫折する方もいた事でしょう。

ですが 2020 年にもなってエディタをセットアップするのが大変みたいな事は本当にあるのでしょうか？

実はそんなことはありません。他の OS とほとんど同じような手順でセットアップできます。
実は私自身、この記事を書く際に初めて Windows に Emacs をセットアップしました。
作業してみると驚くほどあっけなく環境構築できたため、「記事の内容が薄くなるな…」と公開を少しためらったぐらいです。

## Emacs for Windows のインストール

まずは PewerShell を起動します。PowerShell は Windows ユーザーにとっては効率化行うための必須ツールです。
必須ではありませんが、WindowsTerminal をインストールしておくと作業効率があがります。

### Scoop

Windows で開発環境を構築する際に必須のツールと言えるのが[scoop](https://scoop.sh/)です。
scoop は簡潔にいうと Windows 版のパッケージマネージャーです。詳細はここでは割愛しますが、scoop を使えば管理者権限は必要ありませんし、他の OS のパッケージマネージャーのようにユーザー環境にツールを簡単にインストールできます。

scoop 自体のインストールも簡単です。以下を PowerShell で実行します。

```
Invoke-Expression (New-Object System.Net.WebClient).DownloadString('https://get.scoop.sh')
```

scoop は bucket という単位でインストールできるツールを管理しています。インストールできるツールを増やす場合には bucket を追加登録します。

Emacs は extra bucket にあるので scoop のインストール後、extras bucket を登録します。

```
scoop bucket add extras
```

あとは scoop install で必要なツールをインストールするだけです。
scoop の便利な所は scoop でインストールしたツールには PATH が自動で設定される点です。インストール後、追加設定することなくそのコマンドを実行することができます。

### 各種ツール、Emacs をインストールする

先述した通り、Emacs は Unix ツールの使用を前提している部分があります。
そのため、Emacs をインストールする前に依存するツール、そして git など開発でよく使うツールをインストールします。
あくまで一例ですが、インストールするツールを以下にあげます。

- 7zip
- ag
- direnv
- fzf
- git
- global
- go
- nodejs-lts
- openssh
- ripgrep
- gow
- fd
- gpg

PowerShell で以下を入力し、インストールします。

```
scoop update
scoop install ag direnv fzf git global go nodejs-lts openssh ripgrep gow fd gpg
```

歴史的経緯もあり、Emacs である以上、UNIX で使うツールのインストールは仕方ありません。
ですが、あくまで補助ツールとしての位置づけですので、[Gow](https://github.com/bmatzelle/gow)入れておけばほぼ事足りると思います。
Gow (Gnu On Windows)は軽量な Unix ツール群です。詳細や含まれるツールなどは github のサイトを参照して下さい。
同様な Unix ツール群をインストールできるパッケージには unxtools などもあります。

また go や nodejs もインストールします。これらの言語で書かれたその他のツールをインストールすることが多いため、予めインストールしておきます。
その後、必要に応じて各種 language server をインストールします。

依存ツールのインストール後、Emacs 自体も scoop でインストールします。
Emacs は extras にあり、バージョンは 27.1 です。

```
scoop install emacs
```

インストール後、runemacs.exe を実行して GUI Emacs が起動することを確認します。

## Emacs のセットアップ

Emacs をセットアップしていきます。
Windows の場合、Emacs の設定は以下にあります。そのパスにいつも通り init.el を置いてカスタマイズします。

```
C: \Users\xxx\AppData\Roaming\.emacs.d;
```

古い以前のバージョンで不安視されていた package.el も動作し、melpa などから package install することも可能です。
また日本語環境特有の問題、IME での日本語の入力も問題ありません。
日本語のディレクトリ名も表示されますし、all-the-icons もダウンロードしたフォントを Windows 側でインストールすればアイコンも表示されます。

![](/images/44e9c68d-a062-4794-b2f5-17ffb551100a.png)

### その他の Tips

OS の依存をなるべく減らすような設定にすると無駄にハマらなくて済みます。

- shell は eshell を使う
- パスはドライブ込みのフルパスで指定する。
- daemon モードも使って起動速度を速くする

### 既知の問題

これはセットアップ後、私の方で気づいた既知の問題です。

### docker のコンテナに入れない

これは以前からの問題ですが、tty がないので docker-tramp でコンテナに入れないという問題です。根本的になおすのは厳しいように思います。

[https://github.com/emacs-pe/docker-tramp.el/issues/7](https://github.com/emacs-pe/docker-tramp.el/issues/7)

### lsp のサーバーの初期化で失敗する

go の開発で lsp を使用していて遭遇した問題です。gopls の初期化で失敗します。
lsp-mode.el 内の ht-get\*が正しくないようなので lsp-mode.el に以下の関数を追加します。

```lisp
(defun ht-get* (table &rest keys)
  "Look up KEYS in nested hash tables, starting with TABLE.
The lookup for each key should return another hash table, except
for the final key, which may return any value."
  (when table
    (if (cdr keys)
        (apply #'ht-get* (ht-get table (car keys)) (cdr keys))
      (ht-get table (car keys)))))
```

## まとめ

GNU Emacs for Windows の再入門として Windows で Emacs をセットアップする方法を紹介しました。先述の通り、Emacs のインストールも数行の入力で済みますし、以前のバージョンで必要であった泥臭い設定も必要なくなっています。

Windows ユーザーも自由にハックできるパワフルなエディタ Emacs をインストールしてみてはいかがでしょうか。

Emacs の[Slack グループ](https://emacs-jp.github.io/#slack---emacs-jpslackcom) にも#windows チャンネルがありますのでぜひ気軽に質問していただければと思います。

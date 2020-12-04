---
layout: page
author: blackenedgold
title: "Rustプログラミングのための環境構築"
tags: [env, rust]
date: 2013-09-26
last_modified: 2020-07-01
---
{% include JB/setup %}

## 概要

EmacsでのRust言語をプログラミングする際の環境構築について示します。

このページは以下の記事をemacs-jpのために再編集し投稿したものです。

* [Rustの環境構築（Emacs） \| κeenのHappy Hacκing Blog](https://keens.github.io/blog/2020/12/01/rustnokankyoukouchiku_emacs_/)

## ツールのインストール

[Rustup](https://rustup.rs)によるrustツールチェーンのセットアップは済んでいるものとして、他のツールの準備方法を案内します。

### フォーマッタ、リンタ

公式で配布されている[rustfmt](https://github.com/rust-lang/rustfmt)（フォーマッタ）と[clippy](https://github.com/rust-lang/rust-clippy)（リンタ）が鉄板です。
インストールは…既に上記の方法でインストールされています。
確認してみましょう。

``` console
$ which rustfmt
/home/shun/.cargo/bin/rustfmt
$ which cargo-clippy
/home/shun/.cargo/bin/cargo-clippy
```

もしインストールされていなかったら下記のコマンドでインストールできます。

``` console
$ rustup component add rustfmt clippy
```

## LSPサーバ

[LSP](https://microsoft.github.io/language-server-protocol/)はマイクロソフトが提唱した言語処理系とエディタ/IDEがやりとりするためのプロトコルです。
ざっくり言うとLSPをサポートしている言語ならEmacsがEclipseやIntelliJ並にリッチな環境になります。

さて、RustのLSPサーバの状況なのですが、ツールが2つあります。

1つが[rls](https://github.com/rust-lang/rls)で現行の公式推奨のLSPサーバです。

もう1つが[rust-analyzer](https://github.com/rust-analyzer/rust-analyzer)で、一応実験的な実装とされています。
しかし出来がよく、[rust-analyzerを公式のツールにしよう](https://github.com/rust-analyzer/rust-analyzer/issues/4224)とする動きもあります。

ここでは両方のインストール方法を紹介するので好きな方をインストールしてみて下さい。

#### RLS

rustupでインストールできます。

``` console
$ rustup component add rls
```

#### rust-analyzer

毎週[バイナリリリースがGitHubに作られる](https://github.com/rust-analyzer/rust-analyzer/releases)ので、そこからダウンロードして使います。

rust-analyzerを `/PATH/TO/rust-analyzer` に保存するとして、以下のようなコマンドを毎週叩くことになるでしょう。


``` console
$ curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o /PATH/TO/rust-analyzer
```


rust-analyzerは毎週更新されるので使う方はGitHubの右上にある[Watch]から[Custom]の[Releases]にチェックを入れて、毎週のリリースの通知を受け取るとよいでしょう[^1]

[^1]: 毎週のリリースの他に毎晩のプレリリースもあり、そちらも通知されてしまいます。頑張って無視しましょう。

### その他

`cargo install` でRust製ツールをインストールできます。

例えばCargo.tomlをコマンドから編集できる[`cargo-edit`](https://github.com/killercup/cargo-edit)は以下のコマンドでインストールできます。

``` console
$ cargo install cargo-edit
```


あとで紹介するcargo-minor-modeでもサポートがあるので `cargo-edit` のインストールを推奨します。

## Emacsのセットアップ

Rustの開発で一番使われているエディタはVSCodeらしいですが、LSPのおかげでEmacsでも遜色なく開発できます。
使うパッケージは以下です。

* [rust-mode](https://github.com/rust-lang/rust-mode): Rustのメジャーモード
* [lsp-mode](https://github.com/emacs-lsp/lsp-mode): 上述のLSPのEmacsサポート。Rustサポートも同梱されます。
* [lsp-ui](https://github.com/emacs-lsp/lsp-ui): LSPの表示レイヤー
* [cargo](https://github.com/kwrooijen/cargo.el): CargoをEmacsから呼び出せるキーバインド
* ([company](https://github.com/company-mode/company-mode))
* ([yasnippet](https://github.com/joaotavora/yasnippet))

設定の方法はいくつか流儀があるかと思いますが、ここでは[use-package](https://github.com/jwiegley/use-package)を使い以下のような設定をします。


``` emacs-lisp
(setq exec-path (cons (expand-file-name "/PATH/TO") exec-path))
(setq exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #rust

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)


(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lsp

(use-package lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook (rust-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)


```

1つづつ解説していきます。

``` emacs-lisp
(setq exec-path (cons (expand-file-name "/PATH/TO") exec-path))
```

rust-analyzerをインストールしたディレクトリを `exec-path` に加えます。


``` emacs-lisp
(setq exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))
```

`~/.cargo/bin` を `exec-path` に加えます。cargoやrustfmtなどをEmacsから使うために必要です。

``` emacs-lisp
(use-package rust-mode
  :ensure t
  ; ...
)
```

rust-modeのパッケージを使う宣言です。なければインストールします。

``` emacs-lisp
  :custom rust-format-on-save t
```

ファイルを保存する度に `rustfmt` を適用します。

``` emacs-lisp

(use-package cargo
  :ensure t
  ; ...
)
```

cargoのパッケージを使う宣言です。なければインストールします。

``` emacs-lisp
  :hook (rust-mode . cargo-minor-mode)
```

`rust-mode-hook` に `cargo-minor-mode` を追加します。
これで `rust-mode` が起動するときは `cargo-minor-mode` がonになります。


``` emacs-lisp
(use-package lsp-mode
  :ensure t
  ; ...)
```

lsp-modeのパッケージを使う宣言です。なければインストールします。

``` emacs-lisp
  :hook (rust-mode . lsp)
```

`rust-mode-hook` に `lsp` を追加します。
これで `rust-mode` が起動するときは `lsp-mode` がonになります。

``` emacs-lisp
  :bind ("C-c h" . lsp-describe-thing-at-point)
```

`lsp-mode` では `C-c h` に `lsp-describe-thing-at-point` を割り当てます。

因みに `lsp-rust` のデフォルトのLSPバックエンドはrust-analyzerです。
RLSを使う方は `:custom (lsp-rust-server 'rls)` などの設定が必要になるでしょう。


``` emacs-lisp
(use-package lsp-ui
  :ensure t)
```

lsp-uiのパッケージを使う宣言です。なければインストールします。

その他お好みで設定して下さい。

## 設定した環境の使いかた
### 基本編

基本は自動で動いてくれます。ちょっとプロジェクトを作ってテストしてみましょう。

`cargo new` でプロジェクトを作ります。

``` console
$ cargo new test-project
     Created binary (application) `test-project` package
```


プロジェクトをEmacsで開いてみましょう（`find-file`で `src/main.rs` を選択）。
lsp-modeがワークスペースをインポートするか尋いてくるので `i` と入力してインポートします。

![プロジェクトを開いたときの様子](/images/env/rust/import_project.png)

初回はrust-analyzer/rlsの初期化に少し時間がかかります。

`cargo new` で動くプロジェクトが作られているのでcargo-minor-modeのキーバインドを使って走らせてみましょう。

`C-c C-c r` です。

![C-c C-c rで走らせたところ](/images/env/rust/cargo_run.png)


下のウィンドウに "Hello world!" と表示されていますね。成功です。

それではlsp-modeの補完を試してみましょう。
ファイルの先頭に `use std::collections::HashMap;` と入力しようとしてみて下さい。
すると[company](https://github.com/company-mode/company-mode)で補完がされるはずです。

![use std::collections::HashMap;を補完しているところ](/images/env/rust/completion_hashmap.png)


次にツールでインストールしたcargo-editを使っていみましょう。cargo-minor-mode経由で使えます。
`C-c C-c a RET regex RET` と入力してみて下さい。

![cargo addを使っているところ](/images/env/rust/cargo_add.png)

cargo-editでインストールされたサブコマンド、 `cargo add` を使ってパッケージを追加してくれます。
これは `Cargo.toml` の `[dependencies]` に `regex = "最新のバージョン"` を追記する指示です。
どうやらlsp-modeが追記を読み込んでくれないようなので `M-x lsp-restart-workspace` でリロードしましょう。

今追加したregexパッケージを使ってみましょう。
`let regex = Regex::new("foo.*").unwarp();` と入力しようとしてみて下さい。


![regex::Regexを補完しているところ](/images/env/rust/completion_regex.png)

補完候補がでてきます。
このうち `regex::Regex` を選択するとファイルの先頭に `use regex::Regex;` が自動で追記されます。
なんとオートインポートまでされるんですね。
なんかドキュメントがオーバーレイ表示されて邪魔な場合は `M-x lsp-ui-doc-hide` とでもしてみて下さい。

続いて関連関数の `new` を入力するシーンでももちろん補完されます。

![Regex::newを補完しているところ](/images/env/rust/completion_regex_new.png)

それではこれの型検査（`cargo check`）をしてみましょう。
cargo-minor-modeの `C-c C-c k` を使います。

![C-c C-c kでチェックしているところ](/images/env/rust/compile.png)


未使用アイテムの警告が出て、エラーが0なのでチェックは通っているようですね。


### 発展編

普段の開発でよく使う機能を紹介します。

* LSPの補完
* LSPの定義ジャンプ（`M-.`）と元の場所に戻る（`M-,`）
* LSPのActions（`s-l a a`）
* cargo-minor-modeのcheck（`C-c C-c k`）
  + Rustの型検査だけやってくれる `cargo check` を起動する
* cargo-minor-modeのcheck（`C-c C-c K`）
  + Rustのlinterの `cargo clippy` を起動する
* cargo-minor-modeのtest（`C-c C-c t`）
  + テストを走らせる `cargo test` を起動する
* cargo-minor-modeのadd（`C-c C-c a`）
  + cargo-editプラグインの依存パッケージ追加コマンド `cargo add` を起動する
* `C-c C-c k` のあとの `M-g M-n`/`M-g M-p` （next-error/previous-error）
  + Cargoの出したエラーの起きたソースの位置に飛べる

そんなに多くないので簡単に覚えられるでしょう。
このうち、Actionについて知らないと分からないと思うので説明しておきます。

LSPにはActionというものがあるようです。
コードの特定の場所にカーソルを合わせたときにLSPサーバがActionを提示できるならそれが表示されます。
例えば変数のリネームなどです。

rust-analyzerは結構面白いActionを提示してくれます。
例えば以下は `match` の空の腕の部分にカーソルを合わせた状態です。

![Actionが表示された画面](/images/env/rust/action.png)

右上に赤字で表示されているようにパターンが足りていないのでエラーになります。
その下に "Fill match arms" とありますね。これがActionです。

![Actionはクリックできることを示す画面](/images/env/rust/action_cursor.png)

このActionを実行してみましょう。マウスを使ってクリックするか、`s-l a a` と入力すると実行できます。
上の画像はマウスカーソルを合わせたところです。Emacsでもマウス操作ができるんですね。
なお、yasnippetが必要なので `yas-minor-mode` がonになってるかは確認しましょう。

Actionを適用すると以下のように補完されます。

![Actionを適用したあとの画面](/images/env/rust/action_applied.png)

今回なmatchに必要な `Ok` と `Err` が補完されています。
型までみて賢く動作してくれるんですね。すごいですね。

![Actionの適用後、最後まで書いた画面](/images/env/rust/complete_snippets.png)

あとはこれを埋めてコードを完成させましょう。

解説は以上です。


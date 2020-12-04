---
layout: page
author: conao3
title: "2020年代のEmacs入門"
date: 2020-08-25
last_modified: 2020-09-14
---
{% include JB/setup %}

## はじめに
Emacsは1972年にMITで生まれ、今日に至るまで名だたるハッカーに愛されてきたエディタです。
Emacsがハッカーに愛されている理由は、Emacsがそれ自体、動的なEmacs Lisp環境であり、エディタの動作をEmacsの開発者ではないあなたが、ビルドなしで動的に自由にハックできることです。
OSと協調する必要のあるEmacsのコアとEmacs LispそのもののコアをC言語で提供する他は、ほとんど全ての機能がEmacs Lispによって提供されています。
あなたは細部にわたってEmacsの全てを制御できますし、Emacsの上にあなたの思い描く、あらゆるアプリケーションを構築することができます。
Emacsはあなたの相棒として、あなたの一生を支えてくれると確信しています。

本質的にエディタは「テキストを編集する」という目的を果たせば、どんなエディタでも構いません。
しかし、入学・入社に伴う教育において、半ば利用を強制されるという状況も一部では確認されています。
もちろんEmacsはGNU/Linuxにおける基本的なソフトウェアであり、その拡張性と汎用性を評価されているためだとは思われますが、歴史あるエディタであるがゆえに最初はとっつきにくい印象を受けるかもしれません。

GNUは2020/08/10に3年振りとなるメジャーアップデートとしてEmacs-27.1をリリースしました。
Emacsは古臭いエディタではなく、進化し続けています。
その状況の中で、インターネット上には古い情報がそのまま留まり続けています。
2020年という節目の年にEmacs周辺の最新情報とモダンな環境構築についてまとめるものです。

また、目標を定めないとEmacsの記事はどんどん長くなっていきます。
ここでは「Emacsの基本操作を説明する」ことと、「C++のコードを書けるようにする」ところまでを解説します。

<small>
なお、この記事はEmacs初心者向けです。
初心者向け記事においては、様々な状況を考慮して種々の可能性について言及すると逆に分かりにくくなると筆者は考えています。
Emacs上級者にとっては疑問を感じる点があるかとは思いますが、この記事のスタンスをご理解頂ければと思います。
この記事に関するフィードバックについては筆者のTwitter([@conao_3](https://twitter.com/conao_3))に頂ければと思います。
</small>

## Emacsのインストール
Emacsはメジャーなディストリビューションではパッケージマネージャを利用して簡単にインストールすることができます。

しかし、執筆時点でEmacs-27.1がリリースされてまだ2週間も経っていません。
メジャーなディストリビューションにおいてはOS標準のパッケージマネージャで "emacs" をインストールした際に、まだEmacs-26.3がインストールされると思います。

お使いのディストリビューションのパッケージマネージャで提供されていない場合にはmakeすることになりますが、
あまりそこに時間をかけるべきではありません。
makeに慣れていないようでしたら、パッケージマネージャからインストールできるEmacsをまず触ってみることを強く推奨します。

Emacsのインストールについては別記事([Emacsのインストール](/tips/install-emacs))を参照して下さい。

## Emacsの起動
Emacsの起動はOSが用意しているランチャーからクリックするだけです。
EmacsにはCUI版とGUI版がありますが、GUI版を使用して下さい。
CUI版では設定できないキーバインドの組み合わせがある他、 `posframe` のような近代的なパッケージが使用できません。

![](/images/9407a43e-a53d-4ca5-9015-c6c8991395bf.png)

CUIからEmacsを起動する必要があるのは、Emacsに起動時オプションを与えたい場合です。([Appendix C Emacs呼び出しに対するコマンドライン引数 - Emacs Manual](https://ayatakesi.github.io/emacs/27.1/emacs-ja.html#Emacs-Invocation))

知っておくと便利な起動時オプションは以下の通りです。

| オプション     | 説明                                  |
|----------------|---------------------------------------|
| `-q`           | 初期化ファイルをロードしない          |
| `--debug-init` | init.elに対して、デバッガを有効にする |
| `-l <file>`  | fileをloadする                        |


例えば以下のように利用します。
- init.elを壊してしまって、デバッグ情報が得たいとき
  ```
  emacs --debug-init
  ```

- init.elを無視して初期状態のEmacsを起動したいとき
  ```
  emacs -q
  ```

- 他の場所にあるinit.elを一時的に読み込みたいとき (対象のinit.elに作業が必要)
  ```
  emacs -q -l <file>
  ```

## Emacsの画面
![](/images/58015d69-2a71-4104-9b75-fc22013d3cce.png)

Emacsにおいて、「フレーム」と「ウィンドウ」という用語は注意する必要があります。
通常のGUIアプリケーションにおける「ウィンドウ」はEmacsにおいては「フレーム」に対応しており、
Emacsにおける「ウィンドウ」はフレームのなかで、画面分割した際に増減できる画面要素を指し、ひとつひとつのウィンドウが「バッファ」を表示します。
バッファはファイルに紐付いてファイルの内容を表示する他、ファイルに紐付かないバッファも自由に作成することができます。

「メニューバー」と「ツールバー」、「スクロールバー」は通常のGUIアプリケーションで見慣れているものだと思います。

黒い四角で表示されているものを「ポイント」と呼び、現在の挿入位置を示します。

「モードライン」はウィンドウごとに作成され、現在の「バッファ名」や「メジャーモード」、「マイナーモード」のような重要な情報を始め、
バッファの文字コード、改行コードや、バッファの未保存といった「バッファの状態」、
Gitレポジトリ内のファイルを開いている場合、現在の「Gitブランチの情報」を表示するなど、バッファに関する雑多な情報を表示する領域です。

「フリンジ」は左右に存在し、行の折り返し記号やエラーなど、行に関するちょっとした情報を表示する領域です。

「ミニバッファ」と「エコーエリア」は領域を共有しています。
そのため文脈によって呼び名を使い分けています。
特に「Emacsの対話的な機能を呼び出したときに使用する小さなバッファ」として言いたい場合には「ミニバッファ」と、
特に「Emacsからのメッセージが表示される領域」として言いたい場合には「エコーエリア」と呼びます。

## Emacsの操作
### Emacsへの心持ち
Emacsで困ったら **Ctrl+G** を押して下さい。3回くらい押しても大丈夫です。
Emacsにおける「緊急脱出」ボタンであり、内部的には結構アグレッシブなことをしているようです。
Emacsがフリーズしても大抵は帰ってきてくれます。

さて、突然ですが「Emacs」に対するマインドセットを変えて頂ければと思います。
なぜか、Emacsを触る方はショートカットを全て覚えなければならず、キーボードから手を離してはならないと思っていることが多いのですが、
Emacsに慣れている人でも、設定されているショートカットを全ては覚えてないですし、マウスも使います。

特にメニューバーやツールバーに関しては他のGUIアプリケーションと同じようにマウスポチポチで操作できるにもかかわらず、なぜか最初の一手で無効化してしまい、不必要に苦しんでいる場合があります。
まずはメニューバーやツールバーを使ってEmacsに慣れていきましょう。
そして同じ動作をするショートカットキーも逐一表示されています。何度もマウスポチポチするのに飽きてきたころにショートカットキーを意識しても遅くはないと思います。
(実はメニューバーもEmacs-lispで作られています。
そこに少なくない労力をかけてでもグラフィカルなUIが用意されているのですから、一度は使ってみてはいかがでしょうか。)

注意点として、現代人としては、アイコンにマウスホバーすると説明がツールチップが表示されることを期待しますが、
実際にはエコーエリアに表示されていることに注意して頂ければと思います。

ファイルを開くにはツールバーでは一番左のアイコン、メニューバーでは「File」→「Visit new file...」です。
これらの操作でファイルを開こうとすると、なんと普通にファイルダイアログが表示されます。
Emacsも他のGUIアプリケーションと同じように使うことができるのです。

![](/images/1d195864-dc13-4a94-9795-96d4f5255ebd.png)

### Emacsチュートリアル
マウスポチポチでEmacsを使うとしても、ポイント移動くらいはショートカットキーが欲しいです。
方法はたくさんありますが、手軽な方法はEmacsに添付されたチュートリアルを見ることです。
`M-x help-with-tutorial-spec-language Japanese` で日本語のチュートリアルを開くことができます。
(同じものを「[Emacsチュートリアル 日本語訳](/tips/tutorial-ja)」でも見ることができます)

大切なことは「全てを一度に学ぼうとしない」ことです。
先程述べたように全てのショートカットを覚える必要はありません。

### チートシート
「[Emacs教習所に行ってきた(チートシート付き) - Qiita](https://qiita.com/namn1125/items/5cd6a9cbbf17fb85c740#fn6)」より「約束」の部分と「チートシート」の部分を引用します。
(改変を含みます)

コマンド名が表記されている場合は `M-x` に続けてコマンド名を入力することで実行できます。


- 約束

  以降、

  - Ctrl を押しながらその他のキーを押す操作を C-
  - Alt を押しながらその他のキーを押す操作を M-

  と表記します。また、

  - C-x b という表記 => 「Ctrlを押しながらxを押した後、(一度キーボードから手を離して)bキーを押す」
  - C-x C-c という表記 => 「Ctrlを押しながらxを押した後、同じくCtrlを押しながらcキーを押す」
  - C-M-% という表記 => 「CtrlとAltとShiftを押しながら5キーを押す」

  という点に注意してください。

- 便利コマンド
  - `C-x`: Emacsの基本的な機能を呼び出すプレフィックス。「保存」が「C-x C-s」に割り当てられているのはこのせい。
  - `C-c`: メジャーモードが用意するコマンドのプレフィックス。「C-c」に続く単打はユーザーのために予約されているため、安全に拡張できます。
  - `M-x`: 厳密にはプレフィックスではないが、コマンド名を入力することでそのコマンドを直接起動できる。
    コマンド名は一般的にキーバインドより説明的なので、ショートカットを覚えていなくてもコマンド名(の断片)を覚えておけばコマンドを実行できます。
  - `F1`: ヘルププレフィックス。「F1 F1」でヘルプのヘルプが見れます。
    - `F1 k (describe-key)`: このコマンドに続けてキーバインドを入力すると、そのキーバインドに割り当てられているコマンドのドキュメントを見ることができる。
    - `F1 f (describe-function)`: 関数のドキュメントを見る。
    - `F1 v (describe-variable)`: 変数のドキュメントを見る。
    - `F1 w (where-is)`: 「F1 k」とは逆に、関数に割り当てられているキーバインドを調べる。

- ポイント移動

  | キーバインド | 操作             | コマンド名             |
  |--------------|------------------|------------------------|
  | C-f          | 右               | forward-char           |
  | C-b          | 左               | backward-char          |
  | C-n          | 下               | next-line              |
  | C-p          | 上               | previous-line          |
  | C-a          | 行頭へ           | move-beginning-of-line |
  | C-e          | 行末へ           | move-end-of-line       |
  | C-v          | 1画面送る       | scroll-up-command      |
  | M-v          | 1画面戻す      | scroll-down-command    |
  | M-<          | バッファの先頭へ | beginning-of-buffer    |
  | M->          | バッファの末尾へ | end-of-buffer          |

- 基本編集

  | キーバインド | 操作         | コマンド名                 |
  |--------------|--------------|----------------------------|
  | C-g          | エスケープ   | keyboard-quit              |
  | C-x C-c      | 終了         | save-buffers-kill-terminal |
  | C-x C-s      | 保存         | save-buffer                |
  | C-x C-f      | 開く         | find-file                  |
  | C-/          | アンドゥ     | undo                       |
  | C-SPC        | 選択開始 ※1  | set-mark-command           |
  | C-x h        | 全選択       | mark-whole-buffer          |
  | C-w          | 切り取り     | kill-region ※2             |
  | M-w          | コピー       | kill-ring-save             |
  | C-k          | 行切り取り   | kill-line                  |
  | C-y          | 貼り付け     | yank                       |
  | C-s          | 検索 ※3      | isearch-forward            |
  | M-%          | 置換         | query-replace              |
  | C-M-%        | 正規表現置換 | query-replace-regexp       |

  - ※1 選択開始を指示して、その後ポイントを動かすことで範囲選択します。またEmacsでもShiftキーを使った選択は可能です。さらにEmacsでは選択領域のことを `region` (リージョン)と呼びます。
  - ※2 Emacsではクリップボードのことを `kill-ring` といいます。
  - ※3 次の候補を表示するには、検索文字列入力後、さらに `C-s` を押します。

- ウィンドウ操作

  | キーバインド | 操作                           | コマンド名           |
  |--------------|--------------------------------|----------------------|
  | C-x 0        | カレントウィンドウを閉じる     | delete-window        |
  | C-x 1        | カレントウィンドウ以外を閉じる | delete-other-windows |
  | C-x 2        | 下に新しいウィンドウを開く     | split-window-below   |
  | C-x 3        | 右に新しいウィンドウを開く     | split-window-right   |
  | C-x o        | 次のウィンドウへカーソル移動   | other-window         |

- フレーム操作

  | キーバインド | 操作                           | コマンド名          |
  |--------------|--------------------------------|---------------------|
  | C-x 5 0      | カレントフレームを閉じる       | delete-frame        |
  | C-x 5 1      | カレントフレーム以外を閉じる   | delete-other-frames |
  | C-x 5 2      | 新しいフレームを開く           | make-frame-command  |
  | C-x 5 o      | 次のフレームをアクティブにする | other-frame         |

  ちなみに `C-x 4` プレフィックスは `*-other-window` が設定されていますが、使いません。

- バッファ操作

  | キーバインド | 操作                               | コマンド名        |
  |--------------|------------------------------------|-------------------|
  | C-x b        | カレントバッファ切り替え           | switch-to-buffer  |
  | C-x k        | カレントバッファ消去               | kill-buffer       |
  | C-x C-s      | カレントバッファ保存               | save-buffer       |
  | C-x C-w      | 名前を付けてカレントバッファを保存 | write-file        |
  | C-x s        | バッファ全保存                     | save-some-buffers |
  | C-x C-f      | ファイルを開く                     | find-file         |
  | C-x C-q      | 読み取り専用にする(トグル)         | read-only-mode    |

- 一般的なキーバインドを押してしまったときに何が起こるか

  | 一般的なショートカット | 期待する操作 | Emacsは何をしたか/どうすればよいか                            | Emacsでは |
  |------------------------|--------------|---------------------------------------------------------------|-----------|
  | C-s                    | 保存         | 検索しようとしています。落ち着いてC-gを押してください         | C-x C-s   |
  | C-x                    | 切り取り     | 次のキーの入力を待っています。落ち着いてC-gを押してください。 | C-w       |
  | C-c                    | コピー       | 次のキーの入力を待っています。落ち着いて(ry                   | M-w       |
  | C-a                    | 全選択       | ポイントを行頭に移動させました。逆の操作はC-e                 | C-x h     |
  | C-v                    | 貼り付け     | 1画面下に送りました。逆の操作はM-vです。            | C-y       |
  | C-z                    | アンドゥ     | タスクバーに最小化しました。(CUIコマンドのsuspendに対応)      | C-/       |
  | C-y                    | リドゥ       | kill-ringの内容を貼り付けました                               | C-g C-/   |
  | C-f                    | 検索及び置換 | ポイントを前に一つ進めました。逆の操作はC-b                   | C-s       |

## Emacs設定ファイル
### init.elの作成
Emacsの設定ファイルは `~/.emacs.d/init.el` です。
既にディレクトリがある場合は `~/.emacs.d/` を `~/.emacs.d.old` などにリネームした上で新しくディレクトリを作成してください。

2015年には[use-package](https://github.com/jwiegley/use-package)を使いましたが、2020年では[leaf](https://github.com/conao3/leaf.el)を使用します。
`~/.emacs.d/init.el` にファイルを作成し、その先頭に `leaf` のインストールコードを貼り付けます。

なお、現在では `leaf` のimenuインテグレーションのおかげでファイル分割しないinit.el管理がトレンドです。

```emacs-lisp
;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; ここにいっぱい設定を書く

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
```

### init.elのバイトコンパイル
init.elはEmacsの設定ファイルですが、Emacs lisp (Elisp)によって記述します。
Elispはバイトコンパイルすることによってバイナリを得ることができ、これはinit.elについても同様です。

バイトコンパイルによる実行速度への寄与は少ないですが、コンパイラのワーニング(未定義変数の参照、未定義関数の評価など)を受け取るためにもバイトコンパイルを行いましょう。
ただ、バイトコンパイルした `.elc` ファイルとバイトコンパイル前の `.el` ファイルは `.elc` の方が優先度が高いので、init.elを編集した後は必ずバイトコンパイルを行うようにしましょう。

初回には `leaf` や `hydra` のインストールが入るのでログが大量に流れます。

コンパイラのワーニングが見辛いようでしたらもう一度実行することをお勧めします。
多くのコンパイラと同じように、出力がなければワーニングやエラーなしで正常にコンパイルできたことを示します。

```bash
$ cd ~/.emacs.d
$ emacs --batch -f batch-byte-compile init.el
Importing package-keyring.gpg...
Importing package-keyring.gpg...done
Contacting host: elpa.gnu.org:443
Contacting host: elpa.gnu.org:443
Contacting host: melpa.org:443
Contacting host: orgmode.org:443
Package refresh done
Setting ‘package-selected-packages’ temporarily since "emacs -q" would overwrite customizations
Setting ‘package-selected-packages’ temporarily since "emacs -q" would overwrite customizations
Contacting host: melpa.org:443
  INFO     Scraping files for leaf-autoloads.el...
  INFO     Scraping files for leaf-autoloads.el...done
Checking /home/user/.emacs.d/elpa/leaf-20200817.1226...
...
Compiling /home/user/.emacs.d/elpa/blackout-20200404.1550/blackout-pkg.el...
Compiling /home/user/.emacs.d/elpa/blackout-20200404.1550/blackout.el...
Done (Total of 1 file compiled, 2 skipped)

$ emacs --batch -f batch-byte-compile init.el
```

## パッケージのインストール
[![](/images/c39c9a99-73cb-4f42-b90b-81158397c4cb.png)](https://elpa.gnu.org/)
[![](/images/4ad8a5f8-9cd0-42fc-994b-2e6e99052f9f.png)](https://melpa.org/)

Emacsは `package.el` というビルトインのパッケージマネージャが標準添付されており、ELPAと呼ばれるサーバーからパッケージをダウンロードすることができます。

GNU ELPAはGNUの管理しているELPAであり、設定なしで利用できます。
しかし、GNU ELPAに登録してもらうにはMLへの投稿が必要でハードルが高く、登録されているパッケージは多くありません。
一方、MELPAというELPAは登録時に審査はあるものの、GitHubでPRを送るだけと手続きも簡単です。
(2020/08/25現在、ELPAには256パッケージ、MELPAには4687パッケージ登録されている)
ただ、MELPAはあくまでELPAを補完するものであり、両方を設定する必要があることに注意する必要があります。

<small>
ELPAは特別なものではなく、 `package.el` が認識できるファイルをサーバーに置いておくだけです。
実際、筆者もCELPAというELPAを運用しています。([CELPA (Conao3's Emacs Lisp Package Archive) をデプロイした話 - Conao3 Note](https://conao3.com/blog/2020-d384-6424/))
</small>

leafによる設定は `;; ここにいっぱい設定を書く` の場所に書き、Emacsを再起動することで反映できます。

### Emacsの標準添付パッケージの設定
Emacsには多くの標準添付パッケージがあります。また、C言語で書かれたコア部分についても、Elispから設定ができます。
- cus-edit.c
  ```emacs-lisp
  (leaf cus-edit
    :doc "tools for customizing Emacs and Lisp packages"
    :tag "builtin" "faces" "help"
    :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))
  ```

  leafの `:custom` で設定するとinit.elにcustomが勝手に設定を追記します。
  この状況になると、変数の二重管理になってしまうので、customがinit.elに追記しないように設定します。

- cus-start.c
  ```emacs-lisp
  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :preface
    (defun c/redraw-frame nil
      (interactive)
      (redraw-frame))

    :bind (("M-ESC ESC" . c/redraw-frame))
    :custom '((user-full-name . "Naoya Yamashita")
              (user-mail-address . "conao3@gmail.com")
              (user-login-name . "conao3")
              (create-lockfiles . nil)
              (debug-on-error . t)
              (init-file-debug . t)
              (frame-resize-pixelwise . t)
              (enable-recursive-minibuffers . t)
              (history-length . 1000)
              (history-delete-duplicates . t)
              (scroll-preserve-screen-position . t)
              (scroll-conservatively . 100)
              (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
              (ring-bell-function . 'ignore)
              (text-quoting-style . 'straight)
              (truncate-lines . t)
              ;; (use-dialog-box . nil)
              ;; (use-file-dialog . nil)
              ;; (menu-bar-mode . t)
              ;; (tool-bar-mode . nil)
              (scroll-bar-mode . nil)
              (indent-tabs-mode . nil))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p)
    (keyboard-translate ?\C-h ?\C-?))
  ```
  EmacsのC言語部分で定義されている変数をcustomで扱えるようにまとめているファイルです。
  私の設定を書いておくので、取捨選択して頂ければと思います。変数の説明は `F1 v` で確認できます。
  無効にしているGUI要素についてはコメントアウトしておきました。

- autorevert
  ```emacs-lisp
  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :tag "builtin"
    :custom ((auto-revert-interval . 1))
    :global-minor-mode global-auto-revert-mode)
  ```
  Emacsの外でファイルが書き変わったときに自動的に読み直すマイナーモードです。
  もちろん、Emacsで編集している場合は外の変更で上書きされることはありません。

- cc-mode
  ```emacs-lisp
  (leaf cc-mode
    :doc "major mode for editing C and similar languages"
    :tag "builtin"
    :defvar (c-basic-offset)
    :bind (c-mode-base-map
           ("C-c c" . compile))
    :mode-hook
    (c-mode-hook . ((c-set-style "bsd")
                    (setq c-basic-offset 4)))
    (c++-mode-hook . ((c-set-style "bsd")
                      (setq c-basic-offset 4))))
  ```
  Cやそれに似た構文を持つファイルに関する設定です。
  インデントスタイルについては闇なので、詳しくは書きませんが、かなり細かな設定ができます。

- delsel
  ```emacs-lisp
  (leaf delsel
    :doc "delete selection if you insert"
    :tag "builtin"
    :global-minor-mode delete-selection-mode)
  ```
  選択している状態で入力したときに、regionを削除して挿入するマイナーモードです。
  おそらくこの挙動のほうが現代人の意図に合っていると思います。

- paren
  ```emacs-lisp
  (leaf paren
    :doc "highlight matching paren"
    :tag "builtin"
    :custom ((show-paren-delay . 0.1))
    :global-minor-mode show-paren-mode)
  ```
  対応するカッコを強調表示するマイナーモードです。

- simple
  ```emacs-lisp
  (leaf simple
    :doc "basic editing commands for Emacs"
    :tag "builtin" "internal"
    :custom ((kill-ring-max . 100)
             (kill-read-only-ok . t)
             (kill-whole-line . t)
             (eval-expression-print-length . nil)
             (eval-expression-print-level . nil)))
  ```
  kill-ringの数を制御したり、kill-lineの挙動を変更したりします。

- files
  ```emacs-lisp
  (leaf files
    :doc "file input and output commands for Emacs"
    :tag "builtin"
    :custom `((auto-save-timeout . 15)
              (auto-save-interval . 60)
              (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
              (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                          (,tramp-file-name-regexp . nil)))
              (version-control . t)
              (delete-old-versions . t)))
  ```
  Emacsで好みが分かれる設定として、バックアップファイルを開いているファイルと同じディレクトリに作成するという挙動があります。
  実際、このバックアップファイルに助けられることもあるので、 `.emacs.d` 以下にディレクトリを掘って、そこに保存するようにします。

- startup
  ```emacs-lisp
  (leaf startup
    :doc "process Emacs shell arguments"
    :tag "builtin" "internal"
    :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))
  ```
  自動保存されたファイルのリストです。 `.emacs.d/backup` 以下にまとめて保存するようにします。

### leaf
[leaf](https://github.com/conao3/leaf.el)とはパッケージ設定のためのパッケージです。
パッケージの設定はイディオムが多く、設定のほとんどはパッケージのインストール、変数とキーバインド設定だけです。

この目的で以前は[use-package](https://github.com/jwiegley/use-package)が開発されましたが、文法を継ぎ足していった結果、文法の一貫性が一部で失われてしまっていました。
また、著作権上の問題でEmacs本体への添付は事実上困難でした。そこで、筆者がスクラッチから書き直したパッケージです。

GNU ELPAには既に登録されており、Emacs本体への添付を夢見ています。

さて、パッケージの使い方を知るには、まずソースファイルの[Commentaryセクション](https://github.com/conao3/leaf.el/blob/7d2f13a103ff275f64086f4ad12308266d1fb48a/leaf.el#L25-L43)を見ることです。
しかし、leafの場合はあまり有用な情報が書かれていないので、次の情報源は[公式のREADME](https://github.com/conao3/leaf.el)です。
MELPAからインストールする場合、Elispファイルだけダウンロードされ、ドキュメントは含まれません。
日本語ブログ記事からpackage.elでインストールすると、逆にGitHubのREADMEを見る習慣がなかったということがありました。

leafの場合はさらに著者によるブログ記事があるのでそれも参考になりますが、ブログ記事はそもそも継続的なアップデートに重点は置かれていないので、
古い情報が書かれているかもしれないと思う必要があります。

- [Emacs入門から始めるleaf.el入門 - Qiita](https://qiita.com/conao3/items/347d7e472afd0c58fbd7)
- [2019年アップデート leaf.elで雑然としたEmacs設定ファイル「init.el」をクリーンにする - Qiita](https://qiita.com/conao3/items/db06dc1338aaf8e9b7b1)
- [正式リリース leaf.elで雑然としたEmacs設定ファイル「init.el」をクリーンにする - Qiita](https://qiita.com/conao3/items/dc88bdadb0523ef95878)

### ivy
```emacs-lisp
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                                      (swiper . ivy--regex-plus)))
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)
    
(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :commands (prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
            (prescient-save-file . ,(locate-user-emacs-file "prescient")))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)
```
[ivy](https://github.com/abo-abo/swiper)はミニバッファの補完を強化するパッケージです。

補完が強化され、 `M-x` はこのような表示になります。
コマンドの断片で検索できるようになるので、あえてキーバインドを与えず、 `M-x` から起動する方法も便利です。
この補完では正規表現が使えるので、 `^ivy-` をクエリーを入力すれば、 `ivy` パッケージのインタラクティブ関数が一覧できます。

![](/images/4032d545-297e-409a-b34b-fcc27cbda901.png)

`C-s` はデフォルトでは `isearch` がバインドされていますが、 `swiper` で置き換えた方が便利です。
バッファ内の検索を該当行のみ取り出して表示することができます。もちろん正規表現が使えます。

![](/images/4799cc1a-3102-4bf5-80e2-9cd65e13d266.png)

`C-S-s` には `counsel-imenu` をバインドしました。これはバッファ内の代表的な行(変数宣言、関数宣言など)を抜き出した上で、コードジャンプするものです。
leafはimenuと統合されているので、init.elで実行するとカスタマイズしたいパッケージの場所にすぐジャンプすることができます。

![](/images/c8ce6a8e-0ea7-4d67-ae39-b66d1ec7ee48.png)

### flycheck
```emacs-lisp
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)
```
[flycheck](https://www.flycheck.org)はリアルタイムにソースのエラーやワーニングを表示するマイナーモードです。
このモードにより、そもそもコンパイルが通らないソースコードに対して場所と理由を教えてくれるようになるので、とても捗ります。

エラーリストのバッファは `C-c ! l (flycheck-list-errors)` で開くことができます。

![](/images/c3cca160-9d34-41a9-bf44-9f72cf3ee8f4.png)

### company
```emacs-lisp
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))
```
[company](http://company-mode.github.io/)は入力補完のためのパッケージです。
他エディタではインテリセンスと呼ばれているものと同一の機能を提供します。

![](/images/47b4d946-d730-48f4-84e1-dab85f13a5fa.png)

## まとめ

Emacs標準添付パッケージの設定例と `leaf`, `ivy`, `flycheck`, `company` のインストール及び設定例を紹介しました。
これらのパッケージをインストールすることで、インタラクティブにエディタの支援を得ながらプログラミングできます。

また、書籍により体系的な知識として学びたいという需要に対しては、以下のような書籍があります。

- Emacs実践入門 (大竹智也著) [Amazon](http://www.amazon.co.jp/dp/4774150029)

  Emacsを日常のテキスト編集やプログラム開発で、快適に利用するためのノウハウを解説した書籍です。最初に読む本としてお勧めです。

- Emacsテクニックバイブル　〜作業効率をカイゼンする200の技〜 (るびきち著) [Amazon](http://www.amazon.co.jp/dp/4774143278)

  Emacsの改善ネタがたくさん集まっています。

- Emacs Lispテクニックバイブル (るびきち著) [Amazon](http://www.amazon.co.jp/dp/4774148970)

  Emacsを本格的に利用するために必要な Emacs Lisp の情報がまとまっています。

- やさしいEmacs-Lisp講座 (広瀬雄二著) [Amazon](http://www.amazon.co.jp/dp/4877832718)

  Emacs Lispで本格的に拡張を行う方法をコンパクトにまとめた本です。

- 入門 GNU Emacs 第3版 (オライリー) [Amazon](https://www.amazon.co.jp/dp/487311277X/)

  Emacs 21.3の時代の本ですが、標準添付パッケージについては網羅的にまとめられている本です。

Emacsについて、日本語でのコミュニケーションができる[Slackグループ](/#slack---emacs-jpslackcom)もあります。
ぜひ参加して頂ければと思います。

先述した通り、Emacsには5000近いパッケージがあります。
好みのパッケージもあれば、少し自分の意図とは動作が違うパッケージもあると思います。
Emacsという自由なエディタのもとで、自由なアイデアを実現して頂ければと思います。

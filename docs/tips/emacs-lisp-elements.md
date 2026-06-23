---
layout: page
author: Protesilaos Stavrou
title: Emacs Lisp Elements
tags:
  - Emacs
  - emacs-lisp
private: true
updated_at: '2025-08-09T22:37:13+09:00'
organization_url_name: null
slide: false
ignorePublish: false
stable-version: 1.0.0
release-date: 2025-04-12
---

<!--
#+title: Emacs Lisp Elements
#+author: Protesilaos Stavrou
#+email: info@protesilaos.com
#+language: en
#+options: ':t toc:nil author:t email:t num:t
#+startup: content
#+macro: stable-version 1.0.0
#+macro: release-date 2025-04-12
#+macro: kbd @@texinfo:@kbd{@@$1@@texinfo:}@@
#+texinfo_filename: elispelem.info
#+texinfo_dir_category: Emacs misc features
#+texinfo_dir_title: Emacs Lisp Elements: (elispelem)
#+texinfo_dir_desc: A big picture view of Emacs Lisp
#+texinfo_header: @set MAINTAINERSITE @uref{https://protesilaos.com,maintainer webpage}
#+texinfo_header: @set MAINTAINER Protesilaos Stavrou
#+texinfo_header: @set MAINTAINEREMAIL @email{info@protesilaos.com}
#+texinfo_header: @set MAINTAINERCONTACT @uref{mailto:info@protesilaos.com,contact the maintainer}

#+texinfo: @insertcopying
-->

この本は、「Prot」として知られる[Protesilaos Stavrou](https://protesilaos.com/)によって書かれ、Emacs Lispプログラミング言語の全体像を示しています。

ここで提供される情報は{{ page.release-date | date: "%Y年%m月%d日" }}にリリースされた安定バージョン{{ page.stable-version }}に対応しています。

* 公式ページ: <https://protesilaos.com/emacs/emacs-lisp-elements>
* Gitリポジトリ: <https://github.com/protesilaos/emacs-lisp-elements>

## 目次

 * 1\. [Emacs Lisp入門](#h:getting-started-with-emacs-lisp)
 * 2\. [Emacs Lispを評価する](#h:evaluate-emacs-lisp)
 * 3\. [副作用と戻り値](#h:side-effect-and-return-value)
 * 4\. [データ構造としてのバッファ](#h:buffers-as-data-structures)
 * 5\. [テキストが持つプロパティ](#h:text-has-its-own-properties)
 * 6\. [シンボル、バランスのとれた式、そしてクオート](#h:symbols-balanced-expressions-and-quoting)
 * 7\. [リスト内部の部分評価](#h:partial-evaluation-inside-of-a-list)
 * 8\. [マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)
 * 9\. [リストの要素のマッピング](#h:mapping-through-a-list-of-elements)
 * 10\. [検索結果のマッチデータ](#h:the-match-data-of-the-last-search)
 * 11\. [別のバッファ、ウィンドウ、あるいは狭められた状態に切り替える](#h:switching-to-another-buffer-window-or-narrowed-state)
 * 12\. [`if`, `cond`などによる基本的な制御フロー](#h:basic-control-flow-with-if-cond-and-others)
 * 13\. [`if-let*`の仲間を使用してフローを制御する](#h:control-flow-with-if-let-and-friends)
 * 14\. [`pcase`によるパターンマッチ](#h:pattern-match-with-pcase-and-related)
 * 15\. [コードを実行するか、他のコードにフォールバックする](#h:run-some-code-or-fall-back-to-some-other-code)
 * 16\. [名前付き関数とラムダ関数の使い分け](#h:when-to-use-a-named-function-or-a-lambda-function)
 * 17\. [インタラクティブ関数をLisp呼び出しからも動作させる](#h:make-your-interactive-function-also-work-from-lisp-calls)
 * 18\. [COPYING (著作権表示)](#h:copying)
 * 19\. GNU Free Documentation License
 * 20\. インデックス
   * 20\.1\. 関数インデックス
   * 20\.2\. 変数インデックス
   * 20\.3\. 概念索引

<!--
#+toc: headlines 8 insert TOC here, with eight headline levels
-->

## 1. Emacs Lisp入門 {#h:getting-started-with-emacs-lisp}

この本の目的は、**Emacs Lisp**（別名：Elisp）の全体像を掴んでもらうことです。Emacs Lispは、Emacsを拡張するためのプログラミング言語です。Emacsはプログラム可能なテキストエディタであり、Emacs Lispを解釈して、それに応じた動作をします。Emacsには数多くの機能が組み込まれているので、一行もコードを書かずに使うことができます。また、自分で書いたElispや、パッケージなどの形で他の人から入手したElispを「評価」（≒実行）することで、いつでも思い通りの動作をするようにプログラムできます。

独自のテキストエディタをプログラミングするのは、便利で楽しいものです。たとえば、いつも繰り返している一連の操作をひとつのコマンドにまとめ、キーバインドに割り当てることで作業を効率化できます。ポンとキーを押すだけで、途中の面倒な手順が全て一瞬で完了するのです。これによって、作業効率が向上するだけでなく、エディタが快適な作業環境になります。

おもしろいのは、コードを書いていく過程です。こうしなければならない、といった決まりごとは何もありません。全く何もないのです！ プログラミングのためにプログラミングするのです。それは視野を広げてくれる娯楽活動なのです。その上、Elispのスキルも磨かれます。将来、Emacsの動作を少し変えたいと思ったときに、そのスキルがきっと役立つはずです。

Emacsをいじくり回すことは、それ自体が体験の一部です。それによって、自分のエディタがどう動くべきかについて、臆することなく自分の意見を持つことを学べます。重要なのは、遊びに時間を浪費しすぎたり、些細なことがうまく動かない程度でイライラしたりしない程度になるために、Elispについて十分に学ぶことです。私はこれを、コンピュータサイエンスなどの専門知識がなく、ただEmacsをいじくり回す人間として書いています。エディタで遊びながら、試行錯誤を通じてEmacs Lispを学びました。当初の目的は、何度も何度も繰り返していた細かな操作を改善すること、つまり効率化でした。しかし効率だけを求めていたはずが、もっとずっと奥深いものを見つけたのです。エディタの拡張を学ぶことは、とても充実した経験であり、結果として私はもっと生産的になりました。Emacsは私が望むことを実現してくれるので、とても満足しています。

本書の各章は、基本的に短く要点をまとめています。初心者向けの内容もあれば、より高度な話題を掘り下げている章もあります。リファレンスマニュアルがそうであるように、章の間には相互にリンクが張られています。必要な情報を見つけるために、自由に行き来してください。本文は、解説文とコードで構成されています。コードには、実際の Elispもあれば、基本的な考え方を示すための疑似コードもあります。この本を読む際は、ぜひEmacsの中で、あるいはEmacsをすぐに使える状態にしておくことをお勧めします。そうすれば、紹介されている関数を実際に試してみて、その細かな動作や特性（ニュアンス）をより深く理解できるでしょう。

私がここで採用している「全体像を掴む」というアプローチは、私が普段 Emacs Lispを使っていてよく目にする基本的な概念を網羅することを目指しています。この本は公式のEmacs Lispリファレンスマニュアルの代わりになるものではありませんし、本書で解説しているElispの個々の機能について、絶対的な正しさの根拠として扱うべきではありません。

それでは、幸運を祈ります。どうぞ楽しんで！

## 2. Emacs Lispを評価する {#h:evaluate-emacs-lisp}

Emacsで行う操作はすべて、何らかの関数を呼び出しています。これは Emacs Lispコードを評価し、その戻り値を受け取ったり、副作用を引き起こしたりするということです（[副作用と戻り値](#h:side-effect-and-return-value)参照）。

<!--
#+findex: Interactive functions are commands
-->

キーボードでキーを打つと、現在のバッファに文字が入力されます。これもキーに割り当てられた関数です。実際には、これは「インタラクティブ」関数と呼ばれます。プログラムからではなく、キーバインドを通じて呼び出しているからです。インタラクティブ関数は「コマンド」として知られています。しかし、インタラクティブであるという実装上の詳細に惑わされないでください。Emacs で行う一つ一つのアクションがEmacs Lispの評価を伴っているという事実を忘れてはいけません。

<!--
#+findex: execute-extended-command
-->

もうひとつの一般的な対話方法は<kbd>M-x</kbd> キーです。これはデフォルトで`execute-extended-command`コマンドを実行します。このコマンドはミニバッファにプロンプトを表示し、コマンド名を指定して選択、実行するように促します。

<!--
#+findex: eval-last-sexp
#+findex: eval-buffer
#+findex: eval-region
-->

EmacsはどこからでもElispコードを評価できます。バッファ内にElispコードがあれば、その閉じ括弧の直後にカーソルを置いて<kbd>C-x C-e</kbd>（`eval-last-sexp`）をタイプすることで評価できます。同様に、`eval-buffer`や`eval-region`コマンドを使えば、それぞれ現在のバッファ全体やハイライトされたリージョン（選択範囲）を評価できます。

<!--
#+vindex: buffer-file-name
-->

`eval-last-sexp`はシンボル（変数名など）に対しても機能します（[シンボル、バランスの取れた式、そしてクォート](#h:symbols-balanced-expressions-and-quoting)参照）。たとえば、変数`buffer-file-name`の末尾にカーソルを置いて<kbd>C-x C-e</kbd>（`eval-last-sexp`）を使えば、その変数の値、つまり現在編集中のファイルへのパス（ファイルに関連付けられていなければ`nil`）が得られます。
<!--
#+findex: eval-expression
-->

しかし、場合によっては上記の方法が適さないこともあります。たとえば、現在のバッファのファイルパスをコピーするコマンドを書きたいとしましょう。そのためには、コード内で変数`buffer-file-name`の値をテストする必要があります（[データ構造としてのバッファ](#h:buffers-as-data-structures)参照）。しかし、そのために実際のファイルに`buffer-file-name`と打ち込み、前述の評価コマンドを実行し、その後で編集内容を元に戻す、なんてことはしたくないでしょう。それは面倒ですし、ミスも起こりやすいものです！現在のバッファの文脈でElispを手軽に実行する最善の方法は<kbd>M-:</kbd>（`eval-expression`）をタイプすることです。これによりミニバッファが開き、評価したいコードを入力するよう求められます。そこで<kbd>RET</kbd>（Enterキー）を押せば実行されます。評価は、最後にカレントだったバッファ（`eval-expression`を呼び出す直前にカレントだったバッファ）を基準に行われます。

以下のEmacs Lispコードを「⑴ファイルに対応するバッファ」と、「⑵ディスク上のどのファイルとも関連付けられていないバッファ」で、試してみるとよいでしょう。

```emacs-lisp
;; `eval-expression' を使って、さまざまなバッファで以下のコードを評価してみてください
(if buffer-file-name
    (message "The path to this file is `%s'" buffer-file-name)
  (message "Sorry mate, this buffer is not visiting a file"))
```

<!--
#+findex: ielm
#+findex: lisp-interaction-mode
#+vindex: initial-major-mode
#+findex: eval-print-last-sexp
#+findex: eval-last-sexp
-->

コードを試しているときは、それがどのように動作するかテストしたいものです。インタラクティブシェル（対話型シェル）を開くには`ielm`コマンドを使います。これによりプロンプトが表示され、任意のElispをタイプして<kbd>RET</kbd>を押せば評価できます。戻り値はそのすぐ下に表示されます。別の方法として、`*scratch*`バッファに切り替える手もあります。もし、変数`initial-major-mode`のデフォルト値であるメジャーモード`lisp-interaction-mode`を使っていれば、そのバッファ内を自由に移動し、コードの末尾で<kbd>C-j</kbd>（`eval-print-last-sexp`）をタイプして評価できます。これは`eval-last-sexp`とほぼ同じように機能しますが、評価した式のすぐ下にその戻り値を出力するという追加効果があります。

<!--
#+cindex: Introspect Emacs Lisp
#+vindex: major-mode
#+findex: describe-variable
#+findex: describe-function
#+findex: describe-keymap
#+findex: describe-key
#+findex: describe-symbol
-->

これらに加えて、Emacsの自己文書化機能を利用して、現在の状態を知ることができます。たとえば、変数`major-mode`のバッファローカルな値について知りたければ、<kbd>C-h v</kbd>（`describe-variable`）を実行し、その変数を検索します。結果として表示されるヘルプバッファには、`major-mode`の現在の値が示されます。このヘルプコマンドや、`describe-function`、`describe-keymap`、`describe-key`、`describe-symbol`のような他の多くのコマンドは、Emacsが特定の対象について何を知っているかについての情報を提供します。ヘルプバッファには、関連情報、たとえばその関数を定義しているファイルへのパスや、変数がバッファローカルとして宣言されているかどうかなどが表示されます。

<!--
#+cindex: Emacs is self-documenting
-->

Emacsが「**自己文書化されている**（self-documenting）」というのは、自身の状態を報告するからです。ヘルプバッファを明示的に更新する必要はありません。これは、関連するコードが評価されることによって自動的に行われます。つまりEmacsは、あなたが扱っている対象が何であれ、その最新の値を効果的に表示してくれるのです。

## 3. 副作用と戻り値 {#h:side-effect-and-return-value}

Emacs Lispには関数があります。関数は入力を受け取り、出力を生成します。その最も純粋な形式では、関数は値を返すだけの計算です。純粋な関数は周囲の状態（環境）を何も変更しません。ある関数の戻り値が別の関数の入力となり、処理が連鎖していくのです。この仕組みがあるからこそ、「もしこの処理が成功したら、次にこの処理を行い、失敗したら別の処理をする（あるいは何もしない）」といった条件に応じた流れを組み立てられます。

ElispはEmacsを拡張し、制御するための言語です。そのため、Elispの処理はエディタ自身の状態にも影響を及ぼします。関数を実行すると、カーソル位置へのテキスト挿入、バッファの削除、ウィンドウの新規作成といった、永続的な変化（副作用）を引き起こすことがあります。こうした変化は、後続の関数呼び出しに影響を与える可能性があります。たとえば、ある関数が特定のバッファを削除してしまえば、その後に同じバッファへ書き込もうとしていた別の関数は、対象のバッファが存在しないため、もはや処理を実行できません。

Elispのコードを書く際には、関数の「戻り値」と「副作用」の両方を考慮する必要があります。副作用への配慮が足りないと、環境への意図しない変更が原因で、予期せぬ結果を招くことになります。しかし、副作用を注意深く、意図的に活用すれば、Elispの持つ力を最大限に引き出すことができます。たとえば、「新しいバッファを作り、そのバッファに移動し、テキストを書き込み、好みの場所にファイルとして保存し、元の場所に戻ってくる。ただし、作成したバッファは開いたままにしておく」といった一連の動作を行う関数を考えてみましょう。これらはすべて副作用ですが、非常に便利なものです。さらに、この関数が意味のある戻り値（たとえば、作成したバッファそのもの）を返すようにすれば、後続の関数がその戻り値を使って、別のフレームでそのバッファを表示したり、中のテキストを大きくしたりといった、さらなる操作を行うことも可能です。

あなたがElispを書くとき、戻り値と副作用の両方を考慮に入れなければなりません。もしあなたが不注意であれば、環境への考慮不足の変更すべてによって引き起こされる意図しない結果を得るでしょう。しかし、もしあなたが副作用を細心の注意を払って使用するなら、あなたはElispをその完全な可能性へと引き出す力を与えられます。たとえば、あなたが「バッファを作成し、そこへ行き、テキストを書き、そのバッファを私の好む場所のファイルに保存し、そしてこの関数を呼び出す前にいた場所に戻り、作成したバッファは開いたままにする」という論理に従う関数を定義すると想像してください。これらはすべて副作用であり、それらはすべて有用です。あなたの関数はまた、別の関数の入力としてあなたが利用できる、何らかの意味のある戻り値を持つかもしれません。たとえば、あなたの関数はそれが生成したバッファオブジェクトを返すでしょう。そうすれば、次の関数はそこで何か、たとえばそのバッファを別のフレームに表示し、そのテキストを大きくする、といったことができます。

要するに、エディタの状態を操作し、Emacsを自分の思い描いた通りに動かすことが目標です。そのためには、コードが副作用を持つことが必要な場合もあります。一方で、副作用が全く不要であったり、むしろ意図した結果の邪魔になったりすることもあります。何が必要で何が不要か、その見極めは、経験を積み、スキル（[シンボル、バランスの取れた式、クォーティング](#h:symbols-balanced-expressions-and-quoting)参照）の幅を広げていく中で、自然と磨かれていく直感のようなものです。心配はいりません、気楽にいきましょう！

## 4. データ構造としてのバッファ {#h:buffers-as-data-structures}

<!--
#+findex: point
#+findex: point-min
#+findex: point-max
#+findex: line-beginning-position
#+findex: re-search-forward
-->

Emacsのバッファは、データを文字の連なり（シーケンス）として保持しています。ファイルを開いたときに画面に表示されるテキストなどがこれにあたります。各文字は特定の位置に存在し、その位置は数値で表されます。関数`point`は現在のポイント（通常はカーソルがある場所）の位置を数値で返します（[Emacs Lispを評価する](#h:evaluate-emacs-lisp)参照）。バッファの先頭位置では、`point`は`1`を返します（[副作用と戻り値](#h:side-effect-and-return-value)参照）。バッファ内の位置を返す関数は他にも`point-min`（先頭位置）、`point-max`（末尾位置）、`line-beginning-position`（行頭位置）、`re-search-forward`（前方検索）など多数存在します。これらの関数の中には、たとえば`re-search-forward`がカーソルを検索に一致した箇所へ移動させるように、副作用を持つものもあります。

Emacs Lisp でプログラミングを行う際、バッファはしばしば以下のような目的で利用されます：

<!--
#+findex: buffer-string
#+findex: buffer-substring
#+findex: buffer-substring-no-properties
-->

 * **ファイル内容を文字列として取り出す**： バッファを一つの大きな文字列のように扱うことができます。`buffer-string`関数を使えば、バッファの内容全体を一つの（ときには非常に大きな）文字列として取得できます。また、`buffer-substring`や、テキストプロパティを含まない`buffer-substring-no-properties`といった関数を使えば、指定した二つの位置の間にある部分文字列だけを取り出すことも可能です（[テキストが持つプロパティ](#h:text-has-its-own-properties)参照）。<br />たとえば、「(1) あるファイルを開き」「(2) 特定の位置に移動し」「(3) そこにあるテキストをコピーし」「(4) 別のバッファに切り替えて」「(5) コピーしたテキストをその新しいバッファに書き込む」といった一連の操作の一部として、これらの関数が使われる場面を想像できるでしょう。

<!--
#+findex: get-buffer-create
#+findex: get-buffer
#+findex: with-current-buffer
#+findex: erase-buffer
#+findex: delete-region
#+findex: display-buffer
#+findex: pop-to-buffer
-->

 * **何らかの処理結果を表示する**： たとえば、今後の祝日一覧を表示するような関数を考えてみましょう。コードは内部的に必要な計算を行い、最終的にその結果をテキストとして特定のバッファに書き込みます。そして、そのバッファがユーザーに表示されるわけです。この実現方法にもよりますが、まず結果を表示するためのバッファを取得（なければ作成）する`get-buffer-create`や、既存のバッファのみを取得する`get-buffer`といった関数が必要になるでしょう。もし既存のバッファの内容を一旦消去したい場合は、`with-current-buffer`マクロを使って一時的に対象バッファに切り替え、`erase-buffer`関数で全内容を削除するか、`delete-region`で指定範囲のみを削除することができます。最終的に、`display-buffer`や`pop-to-buffer`といった関数が、その結果の入ったバッファをEmacsのウィンドウに表示させます。

<!--
#+vindex: buffer-file-name
#+vindex: fill-column
#+vindex: default-directory
#+vindex: buffer-list
#+findex: setq-local
-->

 * **特定のバッファに変数を関連付ける**： Emacs Lispでは、変数はグローバルな値とは別に、バッファごとに固有の値（バッファローカル値）を持つことができます。`buffer-file-name`、`fill-column`、`default-directory`のように、常にバッファローカルであると定義されている変数もあります。たとえば、特定のディレクトリにあるファイルを開いているバッファのリストを取得したい場合を考えてみましょう。`buffer-list`関数で全バッファのリストを取得し、各バッファの`buffer-file-name`の値をチェックすることで、目的のバッファだけを絞り込む（フィルタリングする）ことができます（[`if`, `cond`などによる基本的な制御フロー](#h:basic-control-flow-with-if-cond-and-others)参照）。`buffer-file-name`のような変数はバッファごとに自動的に設定されていますが、`setq-local`マクロを使えば、任意の変数に現在のバッファ限定の値を設定することも可能です。

<!--
#+findex: seq-filter
#+findex: buffer-list
#+cindex: Hidden buffers
-->

最後に触れたバッファローカル変数は、おそらく最も応用範囲の広い考え方でしょう。バッファとは、その内容、実行中のメジャーモード、そして設定されているすべてのバッファローカル変数などを含む、いわば「変数の束」のようなものなのです。以下のコード例では、`seq-filter`関数を使って`buffer-list`関数が返すバッファのリストを処理（反復処理）しています（[シンボル、バランスのとれた式、そしてクオート](#h:symbols-balanced-expressions-and-quoting)参照）。

```emacs-lisp
(seq-filter
 (lambda (buffer)
   "Return BUFFER if it is visible and its major mode derives from `text-mode'."
   (with-current-buffer buffer
     ;; ユーザーに見られることを意図していないバッファは、バッファ名の先頭をスペースから始める慣習です。
     ;; 今回はそのようなバッファには興味がないので無視します。
     (and (not (string-prefix-p " " (buffer-name buffer)))
          (derived-mode-p 'text-mode))))
 (buffer-list))
```


このコードは「(1) ユーザーに「可視」であり（慣習的に名前がスペースで始まらない）」かつ「 (ii) メジャーモードが`text-mode`またはその派生モードである」、という条件を満たすバッファオブジェクトのリストを返します。この処理は、以下のように名前付き関数を使って書くこともできます（[名前付き関数とラムダ関数の使い分け ](#h:when-to-use-a-named-function-or-a-lambda-function)参照）：

```emacs-lisp
(defun my-buffer-visble-and-text-p (buffer)
  "Return BUFFER if it is visible and its major mode derives from `text-mode'."
  (with-current-buffer buffer
    ;; ユーザーに見られることを意図していないバッファは、バッファ名の先頭をスペースから始める慣習です。
    ;; 今回はそのようなバッファには興味がないので無視します。
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (derived-mode-p 'text-mode))))

(seq-filter #'my-buffer-visble-and-text-p (buffer-list))
```


バッファと同様に、Emacsのウィンドウやフレームもそれぞれ固有のパラメータを持っています。これらもデータ構造の一種ですが、バッファほど頻繁には扱われず、より専門的な用途になるため、本書では詳しく触れません。概念自体はバッファと同じです。重要なのは、ウィンドウやフレームもまた、プログラムで操作したり、リストとして繰り返し処理したり（[リスト要素のマッピング](#h:mapping-through-a-list-of-elements)参照）できるデータ構造である、ということを知っておくことです。

## 5. テキストが持つプロパティ {#h:text-has-its-own-properties}

<!--
#+cindex: Propertise text
#+cindex: Fontify text
#+cindex: Faces
#+findex: describe-char
-->


データ構造のように扱えるバッファ（[データ構造としてのバッファ](#h:buffers-as-data-structures)参照）と同じように、どんなテキストにもプロパティ（属性情報）を関連付けることができます。これはEmacs Lispを使って参照できるメタデータ（付加情報）です。たとえば、プログラミング用バッファで表示されるシンタックスハイライト（構文の強調表示）は、このテキストプロパティの効果によるものです。何らかの関数が、対象となるテキストに「プロパティを付与（propertise）」したり「装飾（fontify）」したりする処理を担当し、「フェイス（face）」と呼ばれるオブジェクトを適用します。フェイスとは、フォントの種類や太さ、文字色や背景色といった、文字の体裁や色に関する属性をまとめたものです。カーソル位置の文字のテキストプロパティに関する情報をヘルプバッファで確認するには、<kbd>M-x</kbd>（`execute-extended-command`）に続けて`describe-char`コマンドを実行します。すると、カーソル下の文字、表示に使われているフォント、文字コード、そしてその文字が持つテキストプロパティが表示されます。

たとえば、あなたが独自のメジャーモードを作成しているとしましょう。実験の初期段階として、`fundamental-mode`のバッファ内で、「`I have properties`」というフレーズが現れるすべての箇所に、手動でテキストプロパティを追加したいと考え、次のようなコードを書くかもしれません（[検索結果のマッチデータ](#h:the-match-data-of-the-last-search)参照）：

```emacs-lisp
(defun my-add-properties ()
  "現在のバッファ全体でテキスト \"I have properties\" にプロパティを追加する."
  (goto-char (point-min))
  (while (re-search-forward "I have properties" nil t)
    (add-text-properties (match-beginning 0) (match-end 0) '(face error))))
```


実際にこれを試してみましょう。まず<kbd>C-x b</kbd>（`switch-to-buffer`）を実行し、既存のバッファ名と重複しない適当な名前（例：`test-props`）を入力して<kbd>RET</kbd>を押します。すると、その名前で新しいバッファが開きます。このバッファは`fundamental-mode`で動作するため、自動的なテキスト装飾（fontification）は行われず、`my-add-properties`が意図通りに動作するのを確認できます。では、以下のテキストをその新しいバッファに貼り付けてください：

```
This is some sample text. Will the phrase "I have properties" use the `bold' face?

What does it even mean for I have properties to be bold?
```

続けて<kbd>M-:</kbd>（`eval-expression`）を実行し、プロンプトに対して`(my-add-properties)`と入力して<kbd>RET</kbd> を押し、先ほど定義した関数を呼び出します。どうでしょうか？ バッファ内の「`I have properties`」の部分の見た目が変わりましたか？ このコードが適用しているフェイスは`error`という名前のものです。この単語の意味（エラー）はここでは無視してください。通常、`error`フェイスは非常に目立つスタイル（たとえば赤文字など）で定義されていることが多いため、プロパティが適用されたことが分かりやすいだろうという理由だけで、このフェイスを選びました。ただし、色などのスタイル現在使用しているテーマによっては表示が異なります。


<!--
#+findex: shortdoc
#+cindex: Shortdoc for text properties
-->

Emacs Lispには、指定した位置のテキストプロパティを取得する関数や、特定のプロパティを持つ箇所を前方または後方に検索する関数なども用意されています。個々の関数の詳細は今は重要ではありません。ここで覚えておいてほしいのは、テキストは単なる文字の集まり以上の情報（プロパティ）を持ちうるということです。テキストプロパティについてさらに詳しく知りたければ、<kbd>M-x</kbd>（`execute-extended-command`）で`shortdoc`コマンドを呼び出してみてください。ドキュメントのグループ名を尋ねられるので、`text-properties`を選択すると関連情報が表示されます。`shortdoc`はそこにリストされているどの項目についても使える便利なコマンドなので、私はいつも活用しています。

## 6. シンボル、バランスのとれた式、そしてクオート {#h:symbols-balanced-expressions-and-quoting}

<!--
#+cindex: Define a simple function
-->


馴染みがない人にとって、Emacs Lispはとにかく括弧だらけの言語に見えることでしょう！ まずは、簡単な関数定義を見てみましょう：

```emacs-lisp
(defun my-greet-person (name)
  "Say hello to the person with NAME."
  ;; NAME を呼んで挨拶する.
  (message "Hello %s" name))
```

<!--
#+findex: message
#+findex: view-echo-area-messages
-->


ここで`my-greet-person`という名前の関数を定義しました。この関数はパラメータ（仮引数）のリストを持ちます（この場合は`name`という一つのパラメータだけです）。その次には、必須ではありませんがドキュメンテーション文字列（docstring）があります。これは、他の人がコードの意味を理解したり、関数の目的を把握したりするのに役立ちます。`my-greet-person`関数は、受け取った`name`を`message`関数に引数として渡し、最終的に挨拶文を表示します。`message`関数が使われると、そのテキストは`*Messages*`バッファに記録されます（このバッファは<kbd>C-h e</kbd> (`view-echo-area-messages`) ですぐに確認できます）。さて、この`my-greet-person`関数を、期待されている一つの引数を指定して呼び出すには、次のようにします：

```emacs-lisp
(my-greet-person "Protesilaos")
```

では、複数のパラメータを持つ関数で同じようにやってみましょう：

```emacs-lisp
(defun my-greet-person-from-country (name country)
  "Say hello to the person with NAME who lives in COUNTRY."
  ;; NAME と住んでいる COUNTRY を呼んで挨拶する.
  (message "Hello %s of %s" name country))
```

そしてそれをこのように呼び出します：

```emacs-lisp
(my-greet-person-from-country "Protesilaos" "Cyprus")
```

このように、最も基本的な処理であっても、たくさんの括弧が出てきます。でも、心配はいりません！ 実はこの括弧があるおかげで、コードの構造がむしろ理解しやすくなるのです。もし今そう感じられないとしても、それは単にまだ慣れていないだけです。一度この感覚に慣れてしまえば、もう括弧のない世界には戻れなくなるでしょう。

<!--
#+cindex: Lisp languages are all about lists
-->

Emacs Lispを含むあらゆるLisp方言の基本的な考え方は、括弧`()`が「リスト」の区切りを表す、という点にあります。リストは複数の「要素」から構成されます。リストは、大きく分けて二通りの扱われ方をします。一つは、何らかの計算を実行するために「評価」される場合、もう一つは、後で別の評価（計算）に使うためにデータとして「そのまま」返される場合です（[副作用と戻り値](#h:side-effect-and-return-value)参照）。

 * **関数呼び出しとしてのリスト**：リストが「評価」される場合、リストの先頭にある要素は実行すべき「関数名」とみなされ、それ以降の要素はその関数に渡される「引数」として扱われます。先ほど`my-greet-person`をその引数として`"Protesilaos"`で呼び出すことで、そのように展開されました。`my-greet-person-from-country`についても同じ原則で、`"Protesilaos"`と`"Cyprus"`がその引数です。
 * **データとしてのリスト**：リストが「評価」されない場合、その中の要素は特別な意味を持ちません。リスト全体がそのまま、変更されることなくデータとして返されます。リストを評価させずにデータとして扱いたい場合は、リストの前にシングルクォート`'`を付けます。たとえば、`'("Protesilaos" "Prot" "Cyprus")`と書くと、これは3つの文字列要素からなるリストとして、そのまま返されます。

<!--
#+findex: car
#+findex: cdr
-->

では、後者の「データとしてのリスト」のケースを考えてみましょう。まだ例を見ていませんでしたね。手元に要素のリストがあり、そこから特定のデータを取り出したいとします。最も基本的な操作として、関数`car`はリストの「最初の要素」を返し、`cdr`は「最初の要素を除いた残りの要素からなるリスト」を返します：

```emacs-lisp
(car '("Protesilaos" "Prot" "Cyprus"))
;; => "Protesilaos"

(cdr '("Protesilaos" "Prot" "Cyprus"))
;; => ("Prot" "Cyprus")
```

ここでのシングルクォート`'`は極めて重要です。これがEmacsに対して「このリストを評価しないでください」と指示しているからです。もしクォートがなければ、Emacsはこのリストを評価しようとし、最初の要素である文字列`"Protesilaos"`を関数名として扱おうとします。しかし、そんな名前の関数は定義されていないため、エラーが発生してしまうのです。

<!--
#+findex: list
#+cindex: Self-evaluating objects
-->


Emacs Lispのデータ型の中には、「自己評価（self-evaluating）」するものがあります。これは、それらを評価しても、見た目通りのそれ自身がそのまま値として返ってくる、という意味です。たとえば、文字列`"Protesilaos"`を評価すると、結果は`"Protesilaos"`です。これは文字列の他に、数値、キーワード（`:hello`のようなコロンで始まるもの）、シンボル、そして特別な値である`nil`や`t`にも当てはまります。以下は、これらのデータ型のサンプルを関数`list`を使ってリストにした例です：

```emacs-lisp
(list "Protesilaos" 1 :hello 'my-greet-person-from-country nil t)
;; => ("Protesilaos" 1 :hello 'my-greet-person-from-country nil t)
```


`list`関数は、渡された引数を（クオートされていなければ）評価します。上の例で、戻り値のリストが元の引数とほとんど変わらないように見えるのは、多くの引数（文字列、数値、キーワード、`nil`, `t`）が自己評価するからです。ここで、`my-greet-person-from-country`が、評価されたくないリストをクオートしたのと同じように、シングルクォート`'`でクオートされている点に注目してください。もしクオートがなければ、`my-greet-person-from-country`は評価されようとします。これは通常、変数としての評価を試みるため、（この名前の変数が定義されていなければ）エラーになります。

<!--
#+cindex: Quote to avoid evaluation
-->

シングルクォート`'`は、「次に続くものを評価しないでください」という明確な指示だと考えてください。もう少し正確に言うと、その文脈で通常なら評価が起こるはずの場面で、その評価を抑制する指示です（[リスト内部の部分評価](#h:partial-evaluation-inside-of-a-list)参照）。したがって、既にクオートされているリストの中で、さらに要素をクオートする必要はありません。それは二重にクオートするのと同じになってしまいます：

```emacs-lisp
;; これが正しい方法:
'(1 :hello my-greet-person-from-country)

;; そもそもリスト全体が評価されないので、`my-greet-person-from-country' をクオートするのは間違いです。
;; ここでの間違いは、既にクオートされているものを ''my-greet-person-from-country のように二重にクオートしていることです。
'(1 :hello 'my-greet-person-from-country)
```

<!--
#+cindex: Self-quoting objects
#+cindex: Unquoted symbols are evaluated
-->

では、なぜ`my-greet-person-from-country`はクオートしたのに、他の要素（数値やキーワードなど）はクオートしなかったのでしょうか？ その理由は、リスト内でクオートされなかった他の要素は、実質的に「自己クオート（self-quoting）」する、つまり評価されてもクオートされているかのように振る舞う（自己評価の裏返し）からです。一方で`my-greet-person-from-country`は「シンボル」です。シンボルとは、それ自身ではなく、何か別のものを指し示す参照のようなものです。具体的には、何らかの計算（関数）を表すか、あるいは変数の値を表します。シンボルをクオートせずに書くと、それはEmacsに対して「このシンボルが指し示している値を持ってきてください」と指示することになります。`my-greet-person-from-country`の場合、これは関数を指していますが、変数ではありません。そのため、クオートなしで評価して値を取り出そうとすると、（変数値がないため）エラーになります。

<!--
#+concept: Elisp Macros
-->

ただし、Emacs Lispには「マクロ」という概念があることに注意してください。マクロは基本的に、コードを生成するためのテンプレートのようなもので、マクロ自身が評価されるのではなく、まず別のコードに「展開」され、その展開されたコードが評価されます。マクロの定義内では、引数の評価やクオートの扱いを細かく制御できます。そのため、マクロ呼び出しの際には、これまで説明したクオートのルールがそのまま当てはまらない場合があります。たとえ展開後のコード内では通常のルールが適用されるとしてもです（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。

<!--
#+findex: quote
#+findex: function
-->

Emacs Lispのコードを読み進めると、`#'some-symbol`のように、ハッシュ記号`#`が付いたクオートを目にすることがあるでしょう。これは「シャープクオート」と呼ばれ、通常のシングルクォート`'`と同様に評価を抑制しますが、特に「関数」を参照している、という明確な意味合いが加わります。これを使うことで、プログラマは式の意図（これは関数である）をより明確に表現でき、またバイトコンパイラ（コードをより効率的な形式に変換するプログラム）が内部的なチェックや最適化を行いやすくなる可能性があります。このシャープクオートに関連して、通常のクオート`'`に対応する`quote`関数と、シャープクオート`#'`に対応する`function`関数についても調べてみると良いでしょう。

## 7. リスト内部の部分評価 {#h:partial-evaluation-inside-of-a-list}

Emacs Lispのコードがどのような見た目か、基本的な考え方はもう掴めているはずです（[シンボル、バランスのとれた式、そしてクオート](#h:symbols-balanced-expressions-and-quoting)参照）。リストは、評価されて関数呼び出しとして実行されるか、クオートされてデータとしてそのまま扱われるかのどちらかでした。しかし、もう一つ別のケースがあります。それは、リスト全体としてはデータとして扱いたいけれど、そのリストの内部にある特定の要素だけは評価したい、という場合、つまり「部分評価」を行いたい場合です。

<!--
#+cindex: Declare a variable
-->

次のコードブロックでは、`my-greeting-in-greek`という名前の変数を定義しています。これはギリシャ語で一般的な挨拶で、文字通りの意味は「あなたに健康を」、発音は「ヤー・スゥ」です。なぜギリシャ語かって？ Lispの世界をもたらした`lambda`（ラムダ）という文字がギリシャ文字なのですから、他のギリシャ由来のものが出てきてもいいでしょう、というわけです（[名前付き関数とラムダ関数の使い分け](#h:when-to-use-a-named-function-or-a-lambda-function)参照）！

```emacs-lisp
(defvar my-greeting-in-greek "Γεια σου"
  "誰かの健康を願うギリシャ語での基本的な挨拶.")
```

<!--
#+findex: message
-->

では、評価の仕組みをより深く理解するために、`message`関数を使って実験してみましょう。まずは、リスト全体をシングルクォート`'`でクオートし、データとしてそのまま扱う場合から始めます。

```emacs-lisp
(message "%S" '(one two my-greeting-in-greek four))
;;=> "(one two my-greeting-in-greek four)"
```

結果を見ると、変数`my-greeting-in-greek`が評価されていないことがわかります。表示されているのは、その変数が指す値ではなく、`my-greeting-in-greek`というシンボルそのものです。これは期待通りの動作です。なぜなら、リスト全体がシングルクォートでクオートされているため、その中のすべての要素は評価されないからです。

では次に、リスト全体はクオートしつつ、その中の`my-greeting-in-greek`という特定の要素だけは評価して、その値で置き換えたい場合にどうすればよいか、以下のコードブロックで見てみましょう：

```emacs-lisp
(message "%S" `(one two ,my-greeting-in-greek four))
;; => "(one two \"Γεια σου\" four)"
```

<!--
#+findex: concat
#+cindex: Quasi quote
#+cindex: Comma operator
-->

ここでの構文に注目してください。シングルクォート`'`の代わりに、バッククォート`` ` ``（「**準クォート**（quasi quote）」とも呼ばれます）を使っています。このバッククォートは、基本的にはシングルクォートと同じようにリスト全体の評価を抑制しますが、カンマ`,`が前に付いた要素だけは特別扱いします。カンマ`,`は、「次に続くものを評価しなさい」という指示であり、バッククォートで囲まれたリストの中でのみ有効です。カンマの後に続く「もの」は、シンボル（変数名など）でも、リスト（関数呼び出しを含む）でも構いません。では、`concat`関数を使って、特定の人への挨拶文を生成しつつ、リスト全体をデータとして返す例を見てみましょう：

```emacs-lisp
(message "%S" `(one two ,(concat my-greeting-in-greek " " "Πρωτεσίλαε") four))
;; => "(one two \"Γεια σου Πρωτεσίλαε\" four)"
```

念のため、もしこのリストをバッククォートもシングルクォートも使わずに`(one two ...)`のように書いてしまうと、エラーになることにも気をつけておきましょう。なぜなら、最初の要素`one`が関数名として扱われ、続く`two`や`my-greeting-in-greek`などが引数として評価されようとするからです。通常、`one`という名前の関数は定義されていませんし、`two`や`four`といったシンボルもクオートされていないため変数として評価されようとしますが、これらも定義されていなければエラーの原因となります。

<!--
#+cindex: Splicing in general
-->

コンマ`,`演算子の他に、`,@`（*読み方は…「コンマ・アット」？*）という表記法もあります。これは「**スプライシング**（splicing）」と呼ばれる操作を行います。これは専門用語ですが、簡単に言えば「次に続く式を評価したら、結果はリストになるはずなので、そのリストの一番外側の括弧を取り除いて、中の要素だけをここに展開してください」という意味です。つまり、通常なら`'(one two three)`というリストが返されるようなコードが、`,@`を使うとその場所に`one two three`という要素の並びが直接挿入されるような効果があります。これだけ聞いてもピンとこないかもしれませんが、リストの要素が単なるデータではなく、それ自体が意味を持つ式として扱われるべき場合に意味を持ちます。ここでは詳しい例は省略します。というのも、この機能は主にマクロを定義する際（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）に重要になるからです。

おそらく、皆さんが自分でコードを書く上で、この部分評価（`` ` ``と`,`）の知識を頻繁に使う必要はないかもしれません。主にマクロの定義でよく使われるテクニックですが、原理的にはどこでも使えます。とはいえ、このような仕組みがあることは知っておいてください。なぜなら、他人の書いたコードやライブラリを利用する際に、そこで何が行われているかを理解するために、少なくともこの知識が必要になる場面があるかもしれないからです。

## 8. マクロやスペシャルフォーム内での評価 {#h:evaluation-inside-of-a-macro-or-special-form}

Emacs Lispのコードにおける最も基本的なリストの扱いは、評価されるか、クオートされて評価されないかのどちらかでした（[シンボル、バランスのとれた式、そしてクオート](#h:symbols-balanced-expressions-and-quoting)参照）。もう少し複雑なケースとして、リストが部分的に評価される場合もありました（[リスト内部の部分評価](#h:partial-evaluation-inside-of-a-list)参照）。しかし、コードを読んでいると、時には通常のクオートや評価のルールが当てはまらないように見えることがあり、戸惑うかもしれません。そのような特殊なケースが実際にどのように動作するかを見る前に、まずは変数評価を含む典型的な関数呼び出しの動作をおさらいしてみましょう。

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")
```

<!--
#+findex: concat
#+cindex: Evaluation inside of a function call
-->

このコードは、部分評価のセクションで既に出てきましたね。これは`concat`関数を呼び出すコードで、その後に3つの引数が続いています。引数の一つ`my-greeting-in-greek`は変数です。このリストが評価される際、Emacsが実際に行っているのは、まず`my-greeting-in-greek`を含む各引数を評価してそれぞれの値を取得し、その後で、得られた値を使って`concat`関数を呼び出す、という手順です。この一連の動作は、以下のように分解して考えられます：

 * リストがあります
 * クオートされていません
 * よって、評価が必要
 * 最初の要素は関数名です
 * 残りの要素は、その関数に渡される引数です
 * 引数が何であるかを確認します
 * 各引数を評価して、それを実際の値に解決します
 * 文字列は自己評価的であり、一方`my-greeting-in-greek`は変数です
 * シンボル`my-greeting-in-greek`の値を含め、各引数の値が揃いました
 * あなたが得たすべての値で`concat`を呼び出します。

言い換えると、（`my-greeting-in-greek`が定数だと仮定すれば）以下の二つの式は全く同じ結果になります：

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")

(concat "Γεια σου" " " "Πρωτεσίλαε")
```

<!--
#+findex: setq
-->

ここまでは想像通りですね。シングルクォートの基本的なルール「クオートされていれば評価せずそのまま返し、されていなければ評価して値を返す」に従っています。しかし、この期待されるパターンが、一見すると適用されていないように見えるケースも多く存在します。その代表例として、シンボルを値に束縛するためによく使われる`setq`を見てみましょう：

```emacs-lisp
(setq my-test-symbol "Protesilaos of Cyprus")
```

上記の式は関数呼び出しのように見えます、それは「(1) リストがクオートされておらず」「(2) 最初の要素が関数の名前であり」、そして「(3) 残りの要素がその関数に渡される引数である」ことを意味します。ある意味では、これはすべて真実です。しかし、あなたはその場合`my-test-symbol`が変数として扱われ、それがその場で評価されてその結果を返し、それが次に、関数に渡される実際の引数になることを期待するでしょう。しかし、これは`setq`が機能する方法ではありません。理由は、それが内部的にこれを行う特殊なケースだからです：

```emacs-lisp
(set 'my-test-symbol "Protesilaos of Cyprus")
```

<!--
#+findex: setq
#+findex: defun
-->

このように内部動作を考えれば、動作は理解できます。舞台裏で魔法が起きているわけではありません。`setq`は、ユーザーがいちいちシンボル（変数名）をクオートしなくてもいいように用意された、便利な構文なのです。確かに、この特殊な動作は、最初は少し混乱するかもしれませんが、すぐに慣れて、最終的には自然に理解できるようになるでしょう。`setq`だけでなく、関数定義の`defun`など、多くのスペシャルフォームに慣れていくことを願っています。`defun`についても、既に出てきた例をもう一度見てみましょう：

```emacs-lisp
(defun my-greet-person-from-country (name country)
  "Say hello to the person with NAME who lives in COUNTRY."
  ;; COUNTRY に住む NAME を持つ人に挨拶する。
  (message "Hello %s of %s" name country))
```

もし通常の評価ルールが適用されるならば、パラメータのリストはクオートされなければいけませんでした。`(name country)`は、`name`が名前の関数のシンボルとしで、`country`はその引数（これもまた変数であろう）とする関数呼び出しとして解釈されてしまうように見えますが実際にはそうなりません。`defun`はパラメータのリストを、あたかもそれがクオートして記述されていたかのように扱うからです。

<!--
#+findex: let
-->

よく使われるもう一つの構文に`let`があります（[`if-let*`の仲間を使用してフローを制御する](#h:control-flow-with-if-let-and-friends)参照）。その一般的な形式は次のようになります：

```emacs-lisp
;; This is pseudo-code
(let LIST-OF-LISTS-AS-VARIABLE-BINDINGS
  BODY-OF-THE-FUNCTION)
```

`LIST-OF-LISTS-AS-VARIABLE-BINDINGS`はリストであり、その各要素は`(SYMBOL VALUE)`の形式のリストです。実際のコード例を見てみましょう：

```emacs-lisp
(let ((name "Protesilaos")
      (country "Cyprus"))
  (message "Hello %s of %s" name country))
```

スペシャルフォームについての話題を続けると、`let`も同様の特殊な評価ルールを持っています。もし`let`が通常の関数なら、`LIST-OF-LISTS-AS-VARIABLE-BINDINGS`は評価されないようにクオートする必要があったはずです。さもなければ、そのリスト自体が評価されようとし、その最初の要素が関数名とみなされてエラーになったでしょう。関数名はリストではなくシンボルでなければならず、`(name "Protesilaos")`はシンボルではないからです。`let`がうまく動作するのは、それが内部的にその`LIST-OF-LISTS-AS-VARIABLE-BINDINGS`をクオートしているからです。

<!--
#+findex: use-package
-->

ほかの多くのスペシャルフォームや、Emacsの初期設定ファイルでパッケージの設定によく使われる`use-package`のような人気のマクロも、同じように特殊な振る舞いをしています。個々のマクロがどのように動作するかは、そのマクロがどのように設計されているかによります。本書では、時間と共に変化する可能性のある個々の実装の詳細ではなく、長期的に役立つ基本原則に焦点を当てたいため、これ以上技術的な詳細には踏み込みません。

<!--
#+findex: pp-macroexpand-last-sexp
#+cindex: Pretty print or expand a macro
-->

あるマクロが、最終的にどのようなコードに展開されるのかを知るには、そのマクロ呼び出しの閉じ括弧 `)` の直後にカーソルを置き、`pp-macroexpand-last-sexp`コマンドを実行します。すると、展開された後のEmacs Lispコードが新しいバッファに表示されます。この展開されたコードが、マクロに代わって実際に評価されるものなのです。

<!--
#+findex: defmacro
#+vindex: default-directory
#+cindex: Defining macros
#+cindex: Splicing within a macro
-->

これらの知識を踏まえた上で、いよいよマクロを書いてみましょう。マクロはテンプレートのようなもので、繰り返しを避ける力を与えてくれます。構文的には、マクロ定義では多くの場合、バッククオート（準クオート／quasi-quote）、カンマ演算子、そしてスプライシングといったメカニズムが使われます（[リスト内部の部分評価](#h:partial-evaluation-inside-of-a-list)参照）。ここでは、`default-directory`をユーザーのホームディレクトリに設定した状態で、一時的なバッファ内で任意のコードを実行する、という簡単なマクロを作成するシナリオを考えます。

```emacs-lisp
(defmacro my-work-in-temp-buffer-from-home (&rest expressions)
  "Evaluate EXPRESSIONS in a temporary buffer with `default-directory' set to the user's home."
  `(let ((default-directory ,(expand-file-name "~/")))
     (with-temp-buffer
       (message "Running all expression from the `%s' directory" default-directory)
       ,@expressions)))
```

この定義では、`&rest`に続くパラメータをリストにします。マクロにはいくつでも引数を渡すことができ、`expressions`はマクロ呼び出し時に渡された引数をすべて集めたリストになります。部分評価をうまく使えば、マクロの本体は定義時ではなく、呼び出されたときに初めて評価されることを保証できます。そして、渡された引数は、あなたが指定した場所（`,@expressions`の箇所）に埋め込まれます。

では、このマクロを呼び出してみましょう：

```emacs-lisp
(progn
  (message "Now we are doing something unrelated to the macro")
  (my-work-in-temp-buffer-from-home
   (message "We do stuff inside the macro")
   (+ 1 1)
   (list "Protesilaos" "Cyprus")))
```

もし、`my-work-in-temp-buffer-from-home`の最後の閉じ括弧`)`の直後にカーソルを置いて<kbd>M-x</kbd>（`execute-extended-command`）に続けて`pp-macroexpand-last-sexp`コマンドを実行すれば、このマクロ呼び出しが実際にどのようなコードに展開されるかを確認できます。私の環境では、以下のように展開されました：

```emacs-lisp
(let ((default-directory "/home/prot/"))
  (with-temp-buffer
    (message "Running all expression from the `%s' directory" default-directory)
    (message "We do stuff inside the macro")
    (+ 1 1)
    (list "Protesilaos" "Cyprus")))
```

元のコードの文脈に、この展開されたコードを当てはめてみると、最終的に次のようになります：

```emacs-lisp
(progn
  (message "Now we are doing something unrelated to the macro")
  (let ((default-directory "/home/prot/"))
    (with-temp-buffer
      (message "Running all expression from the `%s' directory" default-directory)
      (message "We do stuff inside the macro")
      (+ 1 1)
      (list "Protesilaos" "Cyprus"))))
```

この例から、Elispのマクロは「より大きく複雑な定型処理を簡潔な短い記述で表現でき、本当に実行されるのは展開後の大きなコードの方である」と考えることができます。

さて、上で私が書いたマクロは、本体全体が準クォート（`` ` ``）で で始まっていたため、マクロ定義自体の内部で評価がどのように行われるかのニュアンスはあまり分かりませんでした。そこで代わりに、別のアプローチをお見せします。これは、ほとんど同じようなインタラクティブ関数を複数簡単に定義できるマクロ、という例です（[インタラクティブ関数をLisp呼び出しからも動作させる](#h:make-your-interactive-function-also-work-from-lisp-calls)参照）。

```emacs-lisp
(defmacro my-define-command (name &rest expressions)
  "Define command with specifier NAME that evaluates EXPRESSIONS."
  (declare (indent 1))
  (unless (symbolp name)
    (error "I want NAME to be a symbol"))
  (let ((modifined-name (format "modified-version-of-%s" name)))
    `(defun ,(intern modifined-name) ()
       (interactive)
       ,(message "The difference between `%s' and `%s'" modifined-name name)
       ,@expressions)))
```

`my-define-command` は大まかに二つの部分に分けられます： ⑴即座に評価されるもの ⑵さらなる評価のために展開されるもの。後者の部分は準クォートで始まります。この区別は私たちがマクロを呼び出すときに重要です。なぜなら前者の部分はすぐに実行されるので、もしそのときにエラーが発生すればその部分は展開されず、`EXPRESSIONS`は実行されないからです。以下の例で`pp-macroexpand-last-sexp`を試して、この意味を確認してみてください。便宜のため、各ケースの直下に展開結果も示します。

```emacs-lisp
(my-define-command first-demo
  (message "This is what my function does")
  (+ 1 10)
  (message "And this"))
;; =>
;;
;; (defun modified-version-of-first-demo nil
;;   (interactive)
;;   "The difference between ‘modified-version-of-first-demo’ and ‘first-demo’"
;;   (message "This is what my function does")
;;   (+ 1 10)
;;   (message "And this"))


(my-define-command second-demo
  (list "Protesilaos" "Cyprus")
  (+ 1 1)
  (message "Arbitrary expressions here"))
;; =>
;;
;; (defun modified-version-of-second-demo nil
;;   (interactive)
;;   "The difference between ‘modified-version-of-second-demo’ and ‘second-demo’"
;;   (list "Protesilaos" "Cyprus")
;;   (+ 1 1)
;;   (message "Arbitrary expressions here"))


(my-define-command "error scenario"
  (list "Will" "Not" "Reach" "This")
  (/ 2 0))
;; => ERROR...
```

マクロは必ずしも必要でしょうか？ 常に必要というわけではありませんが、うまく設計されたマクロを使えば、コードがより洗練される場面は確かにあります。重要なのは、コードがどのように評価されるかの仕組みを感覚的に理解し、たくさんの括弧に惑わされないようにすることです。さもないと、期待とは異なる結果に戸惑うことになるかもしれません。

## 9. リストの要素のマッピング {#h:mapping-through-a-list-of-elements}

<!--
#+findex: while
#+findex: mapcar
#+findex: mapc
#+findex: dolist
#+findex: seq-filter
#+findex: seq-remove
-->

プログラミングにおける一般的な作業の一つに、リスト（データの集まり）に含まれる項目（要素）を一つずつ順番に処理し、それぞれに対して何らかの計算を行う、というものがあります。Emacs Lispには、汎用的な`while`ループはもちろん、リストの要素を処理するために特化した様々な関数群、たとえば`mapcar`、`mapc`、`dolist`、`seq-filter`、`seq-remove`などが用意されています。目的によって、何らかの副作用（状態の変化）を起こすため、あるいは戻り値（処理結果の値）を得るために、これらの関数を使ってリストの要素を処理します（[副作用と戻り値](#h:side-effect-and-return-value)参照）。ここではいくつかの例を示し、皆さんが当面の作業に最も適したツールを選べるようにしましょう。

<!--
#+findex: mapcar
#+cindex: Accumulating results of a map
-->

まずは`mapcar`です。これはリストの各要素に関数を適用します。そして、各要素に対する関数の戻り値を集めて、新しいリストを作成します。これが`mapcar`全体としての戻り値になります。以下のコード例では、数値のリストに対して mapcar を使い、各数値を`10`ずつ増やして、その結果からなる新しいリストを返しています。

```emacs-lisp
(mapcar
 (lambda (number)
   (+ 10 number))
 '(1 2 3 4 5))
;; => (11 12 13 14 15)
```

In the code block above, I am using a ~lambda~, else an anonymous function ([When to use a named function or a lambda function]()). Here is the same code, but with an eponymous function, i.e. a named function:

上記のコードブロックでは**無名関数**（anonymous function）とも呼ばれる`lambda`を使用しています（[名前付き関数とラムダ関数の使い分け](#h:when-to-use-a-named-function-or-a-lambda-function)参照）。以下は、同じコードですが、**名前付き関数**（eponymous function）を使用しています。

```emacs-lisp
(defun my-increment-by-ten (number)
  "Add 10 to NUMBER."
  (+ 10 number))

(mapcar #'my-increment-by-ten '(1 2 3 4 5))
;; => (11 12 13 14 15)
```

ここでは名前付き関数をクオート（`#'`）している点に注意してください（[シンボル、バランスの取れた式、クォート](#h:symbols-balanced-expressions-and-quoting)参照）。

<!--
#+findex: mapcar
#+findex: mapc
#+cindex: Mapping only for side effects
-->

`mapcar`は戻り値を集めて新しいリストを作りましたが、時にはこれが無駄なこともあります。たとえば、ファイルに関連付けられていて、かつ未保存のバッファをすべて保存する関数を評価したいとしましょう。このシナリオでは、結果を集めることには関心がなく、単にバッファを直接保存するという副作用だけが欲しいのです。この目的のためには`mapc`を使うことができます。`mapc`は常に、操作対象となった元のリストを返します。

```emacs-lisp
(mapc
 (lambda (buffer)
   (when (and (buffer-file-name buffer)
              (buffer-modified-p buffer))
     (save-buffer)))
 (buffer-list))
```

<!--
#+findex: dolist
-->

上記と同様の処理を行う別の方法として`dolist`があります。これも副作用のために使われますが、常に`nil`を返します：

```emacs-lisp
(dolist (buffer (buffer-list))
  (when (and (buffer-file-name buffer)
             (buffer-modified-p buffer))
    (save-buffer)))
```

`dolist`はマクロなので、普通のリストやそれらに適用される評価ルールとは少し異なる振る舞いをする部分があることに気づくでしょう（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。これはコードがどのように表現されるかの慣れの問題です。

<!--
#+findex: dolist
#+findex: mapc
-->

`dolist`と`mapc`のどちらを使うかは、スタイルの問題です。もし名前付き関数を使うなら、私の目には`mapc`の方がすっきりして見えます。そうでなければ`dolist`の方が読みやすいでしょう。以下に私の考え方を疑似コードで示します：

```emacs-lisp
;; 私はこう書くのが好き:
(mapc #'NAMED-FUNCTION LIST)

;; `mapc' + `lambda' の組み合わせよりは `dolist' が好み:
(dolist (element LIST)
  (OPERATE-ON element))

;; これは好きではない:
(mapc
 (lambda (element)
   (OPERATE-ON element))
 LIST)
```

`dolist`や`mapc`は副作用のためのものですが、`let`や関連する形式（[`if-let*`の仲間を使用してフローを制御する](#h:control-flow-with-if-let-and-friends)参照）の助けを借りれば、結果を蓄積するためにも使えます。詳細によっては、このアプローチの方が`mapcar`に頼るよりも理にかなっている場合があります。以下に注釈付きのスケッチを示します。

```emacs-lisp
;; まず空のリスト `found-strings' を準備
(let ((found-strings nil))
  ;; リスト '("Protesilaos" 1 2 3 "Cyprus") の各要素を `dolist' でテスト
  (dolist (element '("Protesilaos" 1 2 3 "Cyprus"))
    ;; 要素が文字列なら `found-strings' に `push' し、そうでなければスキップ
    (when (stringp element)
      (push element found-strings)))
  ;; `dolist' が終わったら `found-strings' の新しい値を返す
  found-strings)
;; => ("Cyprus" "Protesilaos")

;; 上のコードと同じだが、戻り値を反転させる(こちらの方が理に適っている):
(let ((found-strings nil))
  (dolist (element '("Protesilaos" 1 2 3 "Cyprus"))
    (when (stringp element)
      (push element found-strings)))
  (nreverse found-strings))
;; => ("Protesilaos" "Cyprus")
```

完全を期すために、先の例を`mapcar`を使って行うと以下のようになります。

```emacs-lisp
(mapcar
 (lambda (element)
   (when (stringp element)
     element))
 '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" nil nil nil "Cyprus")


(delq nil
      (mapcar
       (lambda (element)
         (when (stringp element)
           element))
       '("Protesilaos" 1 2 3 "Cyprus")))
;; => ("Protesilaos" "Cyprus")
```

`mapcar`はすべての戻り値を素直に集めるため、`nil`を含むリストが返ってきます。もし`nil`が欲しかったなら、そもそも`when`節を使う必要もなかったでしょう。そのため、`mapcar`の戻り値に対して`delq`を適用し、すべての`nil`を削除しています。さて、この手間のかかる作業を`seq-filter`と比べてみてください。

```emacs-lisp
(seq-filter #'stringp '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" "Cyprus")
```

`seq-filter`は、要素が**述語関数**（条件をチェックする関数）を満たすかどうかをテストし、満たす要素だけを返す必要がある場合に最適なツールです。ただし、要素そのものを返すことしかできません。一方、`mapcar`は以下のように、どんな戻り値でも文句なく受け取ります：

```emacs-lisp
(delq nil
      (mapcar
       (lambda (element)
         (when (stringp element)
           ;; mapcar はどんな戻り値でも収集するので、要素をどんな結果に変換することもできる
           (upcase element)))
       '("Protesilaos" 1 2 3 "Cyprus")))
;; => ("PROTESILAOS" "CYPRUS")

(seq-filter
 (lambda (element)
   (when (stringp element)
     ;; `seq-filter' はここで nil ではない値が返された場合のみ値を返すが、返されるのは要素そのもの。
     ;; この `upcase' は、条件判定 (nil 以外が返るか) には使われるが、最終的な結果には反映されず無意味。
     (upcase element)))
 '("Protesilaos" 1 2 3 "Cyprus"))
;; => ("Protesilaos" "Cyprus")
```

<!--
#+findex: find-library
#+findex: seq-take
#+findex: seq-find
#+findex: seq-union
#+cindex: Visit the source code of a file
#+cindex: Shortdoc for lists and sequences
-->

リストの要素をどのように処理（マッピング）するかは、何をしようとしているかによります。すべてをこなしてくれる単一の万能関数はありません。それぞれの関数のニュアンス（細かい違いや特性）を理解すれば、うまく使いこなせるでしょう。ああ、それと、組み込みの`seq`ライブラリもぜひ調べてみてください。<kbd>M-x</kbd>（`execute-extended-command`）で`find-library`を呼び出し、`seq`を検索すると`seq.el`のソースコードを見ることができます。そこには`seq-take`、`seq-find`、`seq-union`のような多くの関数が定義されています。別の方法としては、`shortdoc`コマンドを呼び出して、ドキュメントグループ`list`や`sequence`について読むこともできます。

## 10. 検索結果のマッチデータ {#h:the-match-data-of-the-last-search}

<!--
#+findex: match-data
#+findex: match-beginning
#+findex: match-string
#+findex: re-search-forward
#+findex: looking-at
#+findex: string-match
-->

As you work with Emacs Lisp, you will encounter the concept of "match data" and the concomitant functions ~match-data~, ~match-beginning~, ~match-string~, and so on. These refer to the results of the last search, which is typically performed by the functions ~re-search-forward~, ~looking-at~, ~string-match~, and related. Each time you perform a search, the match data gets updated. Be mindful of this common side effect ([Side effect and return value](#h:side-effect-and-return-value)). If you forget about it, chances are your code will not do the right thing.

In the following code block, I define a function that performs a search in the current buffer and returns a list of match data without text properties, where relevant ([Text has its own properties](#h:text-has-its-own-properties)).

```emacs-lisp
(defun my-get-match-data (regexp)
  "Search forward for REGEXP and return its match data, else nil."
  (when (re-search-forward regexp nil t)
    (list
     :beginning (match-beginning 0)
     :end (match-end 0)
     :string (match-string-no-properties 0))))
```

You may then call it with a string argument, representing an Emacs Lisp regular expression:

```emacs-lisp
(my-get-match-data "Protesilaos.*Cyprus")
```

If the regular expression matches, then you get the match data. Here is some sample text:

```fundamental
Protesilaos lives in the mountains of Cyprus.
```

Place the cursor before that text and use {{{kbd(M-:)}}} (~eval-expression~) to evaluate ~my-get-match-data~ with the regexp I show above. You will get a return value, as intended.

<!--
#+findex: save-excursion
#+findex: point
-->

The way ~my-get-match-data~ is written, it does two things: (i) it has the side effect of moving the cursor to the end of the text it found and (ii) it returns a list with the match data I specified. There are many scenaria where you do not want the aforementioned side effect: the cursor should stay where it is. As such, you can wrap your code in a ~save-excursion~ ([Run some code or fall back to some other code](#h:switching-to-another-buffer-window-or-narrowed-state][Switching to another buffer, window, or narrowed state]]): it will do what it must and finally restore the ~point~ ([[#h:run-some-code-or-fall-back-to-some-other-code)):

```emacs-lisp
(defun my-get-match-data (regexp)
  "Search forward for REGEXP and return its match data, else nil."
  (save-excursion ; we wrap our code in a `save-excursion' to inhibit the side effect
    (when (re-search-forward regexp nil t)
      (list
       :beginning (match-beginning 0)
       :end (match-end 0)
       :string (match-string-no-properties 0)))))
```

<!--
#+findex: save-match-data
#+cindex: Preserve the last match data
-->

If you evaluate this version of ~my-get-match-data~ and then retry the function call I had above, you will notice how you get the expected return value without the side effect of the cursor moving to the end of the matching text. In practice, this is a useful tool that may be combined with ~save-match-data~. Imagine you want to do a search forward inside of another search you are performing, such as to merely test if there is a match for a regular expression in the context, but need to inhibit the modification of the match data you planned to operate on. As such:

```emacs-lisp
(defun my-get-match-data-with-extra-check (regexp)
  "Search forward for REGEXP followed by no spaces and return its match data, else nil."
  (save-excursion
    (when (and (re-search-forward regexp nil t)
               (save-match-data (not (looking-at "[\s\t]+"))))
      ;; Return the match data of the first search.  The second one
      ;; which tests for spaces or tabs is just an extra check, but we
      ;; do not want to use its match data, hence the `save-match-data'
      ;; around it.
      (list
       :beginning (match-beginning 0)
       :end (match-end 0)
       :string (match-string-no-properties 0)))))
```

Evaluate the function ~my-get-match-data-with-extra-check~ and then call with {{{kbd(M-:)}}} (~eval-expression~) to test that it returns a non-~nil~ value with the second example below, but not the first one. This is the expected outcome.

```emacs-lisp
(my-get-match-data-with-extra-check "Protesilaos.*Cyprus")
;; => nil


;; Protesilaos, also known as "Prot", lives in the mountains of Cyprus   .

(my-get-match-data-with-extra-check "Protesilaos.*Cyprus")
;; => (:beginning 41988 :end 42032 :string "Protesilaos lives in the mountains of Cyprus")


;; Protesilaos lives in the mountains of Cyprus.
```

## 11. 別のバッファ、ウィンドウ、あるいは狭められた状態に切り替える {#h:switching-to-another-buffer-window-or-narrowed-state}

Emacs Lispを使ってプログラム的に何かを処理しようとすると、現在の場所から一時的に離れる必要のある場面に出くわします。たとえば、別のバッファに切り替えたり、特定のバッファが表示されているウィンドウに移動したり、あるいは編集中のバッファの見えている範囲を変更（スクロールなど）したり、といったことです。これらの操作はすべて、一つ以上の「副作用」を伴います。そして多くの場合、これらの副作用は、関数の処理が終わった後には元に戻されるべきものです ([副作用と戻り値](#h:side-effect-and-return-value)参照)。

<!--
#+findex: point
#+findex: save-excursion
#+cindex: Restore the point
-->

副作用を元に戻す最も一般的なケースは、おそらく`point`（カーソル位置）の復元でしょう。たとえば、特定のテキストを検索するために、バッファ内を前方や後方に移動するコードを書いたとします。しかし処理が終わった後、カーソルは元の位置に戻っていなければなりません。さもなければ、ユーザーは自分のカーソルがどこへ行ったか分からなくなり、混乱して迷子になってしまいます。この問題は、対象のコードを`save-excursion`で囲むだけで解決します。これについては、他の箇所でも説明している通りです（[検索結果のマッチデータ](#h:the-match-data-of-the-last-search)参照）:

```emacs-lisp
(save-excursion ; restore the `point' after you are done
  MOVE-AROUND-IN-THIS-BUFFER)
```

<!--
#+findex: save-window-excursion
#+findex: select-window
#+cindex: Restore the selected window
-->

`save-window-excursion`も同じ原則で動作します。これを使えば、`select-window`などで別のウィンドウを選択したり、そのバッファ内を移動したりといった操作を行った後で、ウィンドウ構成を元通りに復元することができます：

```emacs-lisp
(save-window-excursion
  (select-window SOME-WINDOW)
  MOVE-AROUND-IN-THIS-BUFFER)
```

<!--
#+findex: save-restriction
#+findex: widen
#+findex: narrow-to-region
#+findex: org-narrow-to-subtree
#+cindex: Restore the narrowing state
-->

`save-restriction`は、バッファの現在のナローイング状態を保存し、処理後に復元します。これを使えば、ブロック内で`widen`や`narrow-to-region`、あるいは`org-narrow-to-subtree`のような関連コマンドを使って表示範囲を自由に変更し、必要な処理を行った後、バッファの状態を自動的に元に戻すことができます。

```emacs-lisp
;; Here we assume that we start in a widened state.  Then we narrow to
;; the current Org heading to get all of its contents as one massive
;; string.  Then we widen again, courtesy of `save-restriction'.
(save-restriction
  (org-narrow-to-subtree)
  (buffer-string))
```

場合によっては、ここで説明してきたものを組み合わせたくなることもあるでしょう。ただし、`save-restriction`のドキュメントでは、「`save-excursion`は一番外側で呼び出すように」と書かれているので気をつけてください。これらとは別に、条件に応じた処理を行うために、また別のアプローチが必要になるケースもあります（[コードを実行するか、他のコードにフォールバックする](#h:run-some-code-or-fall-back-to-some-other-code)参照）。

## 12. `if`, `cond`などによる基本的な制御フロー {#h:basic-control-flow-with-if-cond-and-others}

<!--
#+findex: defun
#+findex: forward-line
-->

基本的な操作を行うのに、必ずしも条件分岐のロジックが必要というわけではありません。たとえば、15行下に移動するコマンドを書いても、バッファの終端に達して指定した行数だけ移動できない場合、Emacsは自動的にそこで停止してくれます。`defun`を使えば、内部で`forward-line`を呼び出して無条件に15行下に移動するインタラクティブ関数（つまり「コマンド」）を、次のように定義できます（ちなみに`forward-line`は負の数を渡せば逆方向に移動します）：

```emacs-lisp
(defun my-15-lines-down ()
  "Move at most 15 lines down."
  (interactive)
  (forward-line 15))
```

<!--
#+findex: if
#+findex: when
#+findex: unless
#+findex: cond
#+findex: and
#+findex: or
#+cindex: Control flow
-->

`my-15-lines-down`は、これ以上ないほど単純なコマンドです。やっていることは、基本的な関数を呼び出し（ラップし）、それに常に同じ引数（この場合は数値の`15`）を渡しているだけです。<kbd>M-x</kbd>（`execute-extended-command`）を実行し、このコマンドを名前で呼び出してみてください。ちゃんと動くはずです！ しかし、特定の条件が満たされた場合にのみ、ある処理を実行したいと考えた途端に話は少し複雑になります。このような論理的な流れの中で処理を分岐させるための「制御フロー」は、主に `if`、`when`、`unless`、`cond`といった構文で表現されます。場合によっては、`and`や`or`といった単純な論理演算子だけで事足りることもあります。

<!--
#+findex: eobp
#+findex: string-match-p
#+findex: stringp
#+cindex: Predicate functions
-->

では、あなたの`my-15-lines-down`をもう少し賢くしてみせましょうか！ もしカーソルがバッファの一番最後にいるときは、下に移動する代わりに15行上に移動するようにしてみます。なぜそんなことをするかって？ これはデモンストレーションですから、面白くしてみましょう！ カーソルがバッファの終端 (end of buffer) にあるかをテストするための述語関数は`eobp`です。「述語（predicate）」とは、特定の条件が満たされたときに真（技術的には`nil`以外）、そうでなければ偽（`nil`）を返す関数のことです（[副作用と戻り値](#h:side-effect-and-return-value)参照）。`eobp`のような少し変わった名前についてですが、Emacs Lispでは、述語関数の名前を`p`という接尾辞で終わらせる慣習があります。関数名が`string-match`のように複数の単語からなる場合は`string-match-p`のように`NAME-p`となり、stringのように単一の単語であれば`stringp`のように`NAMEp`となります。

```emacs-lisp
(defun my-15-lines-down-or-up ()
  "Move at most 15 lines down or go back if `eobp' is non-nil."
  (interactive)
  (if (eobp)
      (forward-line -15)
    (forward-line 15)))
```

この関数を評価してから、<kbd>M-x</kbd>（`execute-extended-command`）で`my-15-lines-down-or-up`を呼び出し、その動作を体感してみてください。次に示すのは似たような考え方ですが、もし`eobp`が`nil`以外を返した場合、つまりバッファの最後にいる場合に、エラーを発生させて処理を中断する例です：

```emacs-lisp
(defun my-15-lines-down-or-error ()
  "Throw an error if `eobp' returns non-nil, else move 15 lines down."
  (interactive)
  (if (eobp)
      (error "Already at the end; will not move further")
    (forward-line 15)))
```

<!--
#+cindex: Indentation in Emacs Lisp
-->

Emacs Lispの奇妙な点、あるいは長所と言うこともできるのかもしれませんが、インデントの仕組みを挙げられます。書いたコードを選択して<kbd>TAB</kbd>キーを押すだけで、Emacs があるべき形にインデントを整えてくれます。`if`文を例に取ると、「then」節（真の場合の処理）は、「else」節（偽の場合の処理）よりも一段深くインデントされます。このインデント自体に特別な構文上の意味はありません。やろうと思えば `(if 条件 then else)`のように、すべてを一行で書くことも可能です、これは通常のリストと同じ見た目ですね（[シンボル、バランスのとれた式、そしてクオート](#h:symbols-balanced-expressions-and-quoting)参照）。では、インデントが何のためにあるかというと、括弧の対応が崩れている箇所を見つけやすくするためです。もし式の並びがおかしく見えたら、それはおそらく括弧が足りないか、多すぎるかのどちらかです。一般的に、同じ階層にある式は同じように字下げされ、より深い階層の式はさらに深く字下げされます。経験を積めば、括弧の不一致をすぐに見つけられるようになります。でも、もし見つけられなくても、最終的にはエラーとして表示されるので安心してください！

`if`の書き方は、2つ以上の引数を取る関数に似ています。この「以上」の部分、つまり3つ目以降の引数は、すべて「else」節の一部として扱われます。したがって、`(if COND THIS)`には「else」節がありませんが、`(if COND THIS ELSE1 ELSE2 ELSE3)`と書いた場合、「else」節として`ELSE1`、`ELSE2`そして`ELSE3`が順番に実行されます。これを適切なインデントで書くと、次のようになります：

```emacs-lisp
(if COND
    THIS
  ELSE1
  ELSE2
  ELSE3)
```

<!--
#+findex: progn
-->

では、`THIS`の部分が複数の複数の式を実行したい場合はどうすればよいでしょうか？ そのために、Elispには`progn`という構文が用意されています。これを使えば、複数の関数呼び出しを一つの式にまとめる（ラップする）ことができます。これらをすべて組み合わせると、コードは次のようになります：

```emacs-lisp
(if COND
    (progn
      THIS1
      THIS2
      THIS3)
  ELSE1
  ELSE2
  ELSE3)
```

<!--
#+findex: when
-->

もし「else」節が必要ない場合は、`when`を使うと意図がより明確になります。`when`は内部的にはマクロであり、実際には`(if COND (progn EXPRESSIONS...))`という形に展開されます。そのため、`EXPRESSIONS`には、一つ以上の式を書くことができます。`when`を使うと、次のようになります：

```emacs-lisp
(when COND
  THIS1
  THIS2
  THIS3)
```

<!--
#+findex: unless
-->

同様に、`unless`は`(when (not COND) EXPRESSIONS)`の意味を持ち、最終的には`if`に展開されるマクロです：

```emacs-lisp
(unless COND
  THIS1
  THIS2
  THIS3)
```

<!--
#+findex: and
#+findex: or
-->

テストしたい条件が複数の要素からなる場合は、`and`や`or`を使うことができます。

```emacs-lisp
(when (or THIS THAT)
  EXPRESSIONS)

(when (and THIS THAT)
  EXPRESSIONS)

(when (or (and THIS THAT) OTHER)
  EXPRESSIONS)
```

<!--
#+findex: if
#+findex: when
#+findex: or
#+findex: and
#+findex: cond
-->

場合によっては、`if`や`when`、`or`、`and`をいくつも組み合わせると、コードが不格好で読みにくくなってしまうことがあります。そのようなときには`cond`を使って、ロジックを個別の条件に分解し、上から順にテストしていくことができます。`cond`は、リストの中にさらにリストが入った形で記述します。この内側のリストはクオートする必要はありません（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。

抽象化すると、次のような形になります：

```emacs-lisp
(cond
 (CONDITION1
  CONSEQUENCES1)
 (CONDITION2
  CONSEQUENCES2)
 (CONDITION3
  CONSEQUENCES3)
 (t
  CONSEQUENCES-FALLBACK))
```

結果のそれぞれは、先ほどの`when`で見たように、式をいくつでも書くことができます。`cond`がどのように振る舞うかを示すための、おもちゃ関数を示します：

```emacs-lisp
(defun my-toy-cond (argument)
  "引数 (ARGUMENT) の型に応じて、異なるメッセージを返します."
  (cond
   ((and (stringp argument)
         (string-blank-p argument))
    (message "空白の文字列ですね。もうちょっと頑張ってみましょう！"))
   ((stringp argument)
    (message "空白じゃない文字列もいけますね。これは進歩です"))
   ((null argument)
    (message "はい、nil は () のような空リストのことでもありますが、今は気にしなくて大丈夫です"))
   ((listp argument)
    (message "お、リストを使う流れに乗ってきましたね！"))
   ((symbolp argument)
    (message "おっと、シンボルですか？"))
   ((natnump argument)
    (message "自然数はいいですね！"))
   ((numberp argument)
    (message "もしかして数学の天才ですか！"))
   (t
    (message "あなたの引数 `%s` が、一体どんな種類のデータなのか、私にはさっぱり分かりません" argument))))
```

この関数を評価して、いろいろな引数を渡して何が起こるか試してみてください（[Emacs Lispを評価する](#h:evaluate-emacs-lisp)参照）。ここに2つの例を示します：

```emacs-lisp
(my-toy-cond "")
;; => "空白の文字列ですね。もうちょっと頑張ってみましょう！"

(my-toy-cond '(1 2 3))
;; => "お、リストを使う流れに乗ってきましたね！"
```

ここまで見てきた構文はすべて、Emacs Lispでよく使われるものです。これら以外にも、`pcase`という強力なマクロがありますが、これは少し特殊な機能を持つため、別のセクションで詳しく見ていきます（[`pcase`によるパターンマッチ](#h:pattern-match-with-pcase-and-related)参照）。

## 13. `if-let*`の仲間を使用してフローを制御する {#h:control-flow-with-if-let-and-friends}

<!--
#+findex: let
#+findex: let*
#+cindex: Let bind variables in the current scope
-->

`let`と`let*`は、その構文の本体（`BODY`）の中だけで有効なローカル変数を宣言します。以下のように：

```emacs-lisp
(let BINDINGS
  BODY)

(let ((variable1 value1)
      (variable2 value2))
  BODY)
```

`BINDINGS`部分はリストのリストで、クオートの必要はありません（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。一方、`BODY`部分は、実行される一つ以上の式からなります（本書の他の箇所では`EXPRESSIONS`とも呼んでいます）。`let`と`let*`（レット・スターと読みます）の違いは、`let*`の方が、先に束縛した変数を、同じ束縛リストの後に出てくる変数の定義に使えるという点です。次の例を見てみましょう：

```emacs-lisp
;; `let*'は先に束縛した変数を後から使えるので、`greeting'の定義で`name'と`country'にアクセスできます
(let* ((name "Protesilaos")
       (country "Cyprus")
       (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))

;; しかし、`let'を使うと失敗します...
(let ((name "Protesilaos")
      (country "Cyprus")
      (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))
```

ローカル変数を束縛したいけれど、その初期値がすべて`nil`でない場合にのみ束縛したい、という状況もあります。もし初期値が`nil`であれば、その変数は役に立たないので、代わりに別の処理を行いたいわけです（[`if`, `cond`などによる基本的な制御フロー](#h:basic-control-flow-with-if-cond-and-others)参照）。このような状況は、関数の戻り値や他の変数の値を元に変数を束縛しようとするときによく起こります。それらの値が`nil`になる可能性があるからです。このような処理は、いつでも次のように書くことができます：

```emacs-lisp
(let ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
      (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
  (if (and variable1 variable2) ; 両方 nil ではないことを簡単にテスト
      THIS
    ELSE))
```

<!--
#+findex: if-let*
-->

しかし、同じことは`if-let*`でさらに簡潔に書くことができます。`if-let*`を使うと、指定したすべての変数が`nil`でない値に束縛できた場合にのみ、`THIS`部分が実行されます。

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

`ELSE`部分では、束縛された変数`variable1`や`variable2`は存在しません。これらの変数が有効なのは、`THIS`部分のスコープ内だけです。

<!--
#+findex: when-let*
-->

`when-let*`は`when`と同じように、"else"がありません。束縛しようとした値のいずれかが`nil`だった場合、`when-let*`全体が`nil`を返すだけです。この点について長々と説明する必要はないでしょう。

Emacs Lispの世界をさらに深く探求していくと、`if-let*`の少し変わった使い方に出会うことがあります。それは、⑴`let`や`let*`のように複数の変数を束縛しつつ、⑵さらに述語関数を呼び出して、`THIS`部分に進むべきかどうかの追加テストを行う、というものです。`if-let*`は、束縛する値のいずれかが`nil`であれば、即座に`ELSE`部分に進む、という基本動作を思い出してください。次の例を考えてみましょう：

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          ;; _ は、「この値は変数に束縛しないが、戻り値が nil でないことだけは確認したい」という意図を示します。
          ;; ここで行っているのは、`variable1' が文字列（のパターンにマッチする）かどうかをテストすることです。
          ;; もし文字列であれば束縛を続け、そうでなければコードの ELSE 部分に処理が移ります。
          (_ (string-match-p variable1))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

物事のやり方に、本質的に優劣があるわけではありません。要はタスクに適したツールを使う、ということに尽きます。たとえば、値が`nil`であっても変数を束縛したい場合もあるでしょう。ケースごとに理にかなった方法を選択してください。

## 14. `pcase`によるパターンマッチ {#h:pattern-match-with-pcase-and-related}

<!--
#+findex: pcase
#+vindex: major-mode
-->

Emacs Lispで自分の考えを表現するのに慣れてくると、`if`や`cond`といった構文を自在に使いこなせるようになるでしょう（[`if`, `cond`などによる基本的な制御フロー](#h:basic-control-flow-with-if-cond-and-others)参照）。`if-let*`などを使えば（[`if-let*`の仲間を使用してフローを制御する](#h:control-flow-with-if-let-and-friends)参照）、さらに凝った処理も書けるかもしれません。しかし、どのような方法を使うにせよ、もっと簡潔な表現を使った方が良いと思われるケースも出てきます。そこで登場するのが`pcase`です。最も基本的な使い方では、`pcase`は`cond`に似ています。ある一つの式の値を、一連の条件リストと照らし合わせてテストするのです。以下は、変数`major-mode`のバッファローカルな値を、いくつかの既知のシンボルと比較する例です：

```emacs-lisp
(pcase major-mode
  ('org-mode (message "You are in Org"))
  ('emacs-lisp-mode (message "You are in Emacs Lisp"))
  (_ (message "You are somewhere else")))
```


これは、以下の`cond`とまったく同じ考え方です：

```emacs-lisp
(cond
 ((eq major-mode 'org-mode)
  (message "You are in Org"))
 ((eq major-mode 'emacs-lisp-mode)
  (message "You are in Emacs Lisp"))
 (t
  (message "You are somewhere else")))
```

<!--
#+findex: pcase
#+findex: message
-->

プログラマーには`pcase`の方がよりエレガントだと主張する人もいるでしょう。この特定の例では確かにそうだと思いますが、私自身は柔軟かつ実用的な考え方をしています：つまり、自分が書いているコードにとって最も理にかなったものであれば、何でも使うということです。

エレガントさという話題のついでに言えば、実は、これまで見てきた条件分岐のロジックはほとんどすべて、少し意外に思える方法で実現できる、ということをお伝えしておくべきでしょう。この本で私が示してきた例が、`message`関数を何度も繰り返し使っていたことを思い出してください。実際には、毎回変化していたのは`message`関数に渡される文字列（引数）の部分だけでした。

それなら、次のように書いても全く同じように動作します：

```emacs-lisp
(message
 (pcase major-mode
   ('org-mode "You are in Org")
   ('emacs-lisp-mode "You are in Emacs Lisp")
   (_ "You are somewhere else")))
```


同じ考え方は、`if`や`when`やそのほかのものにも当てはまります。

<!--
#+cindex: Domain-Specific Language (DSL)
-->

さて、`pcase`が他とどう違うのかという話題に戻りましょう。`pcase`のドキュメントを読むと、それが独自のミニ言語、つまり「ドメイン固有言語（DSL）」を持っていることがわかります。これはマクロではよくあることです（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。それらは、どのように評価されるか、どの式を特別に扱うかを独自に定義します。では、ここで検討しているDSLの主な特徴のいくつかを説明するために、このおもちゃ関数を贈ります：

```emacs-lisp
(defun my-toy-pcase (argument)
  "Use `pcase' to return an appropriate response for ARGUMENT."
  (pcase argument
    (`(,one ,_ ,three)
     (message "List where first element is `%s', second is ignored, third is `%s'" one three))
    (`(,one . ,two)
     (message "Cons cell where first element is `%s' and second is `%s'" one two))
    ((pred stringp)
     (message "The argument is a string of some sort"))
    ('hello
     (message "The argument is equal to the symbol `hello'"))
    (_ (message "This is the fallback"))))
```

どうぞ関数を評価して、実際にお試しください（[Emacs Lispを評価する](#h:evaluate-emacs-lisp)参照）。以下にいくつか例を挙げます：

```emacs-lisp
(my-toy-pcase '("Protesilaos" "of" "Cyprus"))
;; => "List where first element is ‘Protesilaos’, second is ignored, third is ‘Cyprus’"

(my-toy-pcase '("Protesilaos" . "Cyprus"))
;; => "Cons cell where first element is ‘Protesilaos’ and second is ‘Cyprus’"
```

<!--
#+findex: pcase-let
#+findex: pcase-let*
#+findex: pcase-lambda
#+findex: pcase-dolist
#+findex: let
#+findex: let*
#+findex: lambda
#+findex: dolist
#+cindex: Destructuring
-->

先ほどの節のいくつかは、実のところ`cond`を別の方法で表現しているだけです。こちらのほうが優れて見えるかもしれませんが、私の意見では決定的な勝者というわけではありません。真に印象的でパラダイムシフトと呼べるのは「デストラクチャリング（destructuring/分解束縛）」という概念です。これは、リストやコンスセルのようなデータ構造に対してパターンマッチを行い、その要素をあたかも`let`のようにローカル変数に束縛する機能です。このデストラクチャリングの構文は、一見すると難解かもしれません。しかし、部分評価で使われる準クォート（`` ` ``）とカンマ（`,`）に関連付けて考えると理解しやすくなります（[リスト内部の部分評価](#h:partial-evaluation-inside-of-a-list)参照）。

これを念頭に、`pcase-let`、`pcase-let*`、`pcase-lambda`、`pcase-dolist`といった構文を考えてみましょう。これらは、通常の`let`、`let*`、`lambda`、`dolist`に、デストラクチャリング機能を追加した変種と見なせます。ただし、これらは`pcase`のような条件分岐機能は持たず、使い慣れた元の構文の動作にデストラクチャリング機能だけを追加したものです。この機能は、関数がリストを返し、そのリストの要素をすぐに変数に束縛したい、といった場合に特に便利です。これは高度な使い方なので、ここではこれ以上詳しく説明しません。もしあなたが既にそのレベルに達しているなら、私から何をどう書くべきか言われる必要はないでしょう。私のように、通常はよりシンプルなコードを扱う私たちにとっては、`pcase-let`がこの原理を説明するのに十分な例となります。

```emacs-lisp
(defun my-split-string-at-space (string)
  "Split STRING at the space, returning a list of strings."
  (split-string string "\s"))

(pcase-let ((`(,one ,_ ,three) (my-split-string-at-space "Protesilaos of Cyprus")))
  (message "This is like `let', but we got `%s' and `%s' via destructuring" one three))
;; => "This is like ‘let’, but we got ‘Protesilaos’ and ‘Cyprus’ via destructuring"
```

`pcase`やデストラクチャリングを実際に使うかどうかは、あなた次第です。高品質なコードを書くために、これらの機能が必須というわけではありません。しかし「本質的によりエレガントだ」と考える人々の意見に共感し、まさにその理由から、つまり「簡潔でありながら高い表現力を持つ」コードを書くために、これらの機能を選択することもありえるでしょう。

## 15. コードを実行するか、ほかのコードにフォールバックする {#h:run-some-code-or-fall-back-to-some-other-code}

<!--
#+findex: unwind-protect
#+cindex: Unwinding
-->

Your typical code will rely on ~if~, ~cond~, and the like for control flow ([Control flow with ~if-let*~ and friends](#h:basic-control-flow-with-if-cond-and-others][Basic control flow with ~if~, ~cond~, and others]]). Depending on your specific needs or stylistic considerations, it may even include ~pcase~ ([Pattern match with ~pcase~ and related](#h:pattern-match-with-pcase-and-related)) as well as ~if-let*~ ([[#h:control-flow-with-if-let-and-friends)). There are some cases, nonetheless, that make it imperative you run additional code after your primary operation concludes or exits. The idea is to clean up whatever intermediate state you created. The logic is "do this with all the necessary side effects, then whatever happens to it do that now to, inter alia, undo the side effects." This is the concept of "unwinding", which is implemented via ~unwind-protect~.

<!--
#+findex: y-or-n-p
-->

In the following code block, I define a function which produces a minibuffer prompt asking you to provide a =y= or =n= answer, which is shorthand notation for "yes" or "no". It tests the return value of ~y-or-n-p~ to determine what it needs to do. While the prompt is open, the function highlights all instances of the regular expression =(defun= in the current buffer. Those highlights must go away after you are done with the minibuffer and its consequences.

```emacs-lisp
(defun my-prompt-with-temporary-highlight ()
  "Ask for confirmation and highlight all instances of a regexp while waiting."
  (let ((regexp "(defun"))
    (unwind-protect
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (message "You have decided to proceed")
            (message "You prefer not to continue")))
      (unhighlight-regexp regexp))))
```

Try the above in your Emacs to get a feel for it. While the "yes or no" prompt is active, also do {{{kbd(C-g)}}} (~keyboard-quit~) or {{{kbd(C-])}}} (~abort-recursive-edit~) to confirm that the highlights are removed even though the code never gets past the prompting phase. You may even modify the function to produce an error: it will create a backtrace, which will still have the effect of unwinding after you do {{{kbd(q)}}} (~debugger-quit~) from the =*Backtrace*= window.

```emacs-lisp
(defun my-prompt-with-temporary-highlight-try-with-error ()
  "Ask for confirmation and highlight all instances of a regexp while waiting."
  (let ((regexp "(defun"))
    (unwind-protect
        (progn
          (highlight-regexp regexp)
          (error "This error makes no sense here; close the backtrace to test the unwinding")
          (if (y-or-n-p "Should we proceed or not? ")
              (message "You have decided to proceed")
            (message "You prefer not to continue")))
      (unhighlight-regexp regexp))))
```

<!--
#+findex: unwind-protect
#+findex: save-excursion
#+findex: save-restriction
#+findex: save-match-data
#+findex: with-temp-buffer
#+findex: save-window-excursion
#+findex: error
-->

Taking a step back, you will figure out how ~unwind-protect~ is a more general form of specialists like ~save-excursion~ and ~save-restriction~ ([The match data of the last search](#h:switching-to-another-buffer-window-or-narrowed-state][Switching to another buffer, window, or narrowed state]]), while it underpins the ~save-match-data~ ([[#h:the-match-data-of-the-last-search)) among many other functions/macros, such as ~with-temp-buffer~ and ~save-window-excursion~. What ~unwind-protect~ does not do is respond specially to signals, such as those coming from the ~error~ function: it will allow the error to happen, meaning that a backtrace will be displayed and your code will exit right there (but the unwinding will still work, as I already explained, once you dismiss the backtrace). To make your code treat signals in a more controlled fashion, you must rely on ~condition-case~.

<!--
#+findex: condition-case
#+cindex: Catching errors and other signals
#+cindex: Non-local exits
#+findex: signal
-->

With ~condition-case~ you assume full control over the behaviour of your code, including how it should deal with errors. Put differently, your Elisp will express the intent of "I want to do this, but if I get an error I want to do that instead." There are many signals to consider, all of which come from the ~signal~ function. These include the symbols ~error~, ~user-error~, ~args-out-of-range~, ~wrong-type-argument~, ~wrong-length-argument~, and ~quit~, in addition to anything else the programmer may consider necessary. In the following code blocks, I show you how ~condition-case~ looks like. Remember that sometimes you do not do quoting the usual way because of how the underlying form is implemented ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)). The example I am using is the same I had for ~unwind-protect~.

```emacs-lisp
(defun my-prompt-with-temporary-highlight-and-signal-checks ()
  "Ask for confirmation and highlight all instances of a regexp while waiting."
  (let ((regexp "(defun"))
    (condition-case nil
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (user-error "You have decided to proceed; but we need to return a `user-error'")
            (error "You prefer not to continue; but we need to return an `error'")))
      (:success
       (unhighlight-regexp regexp)
       (message "No errors, but still need to unwind what we did, plus whatever else we want here"))
      (quit
       (unhighlight-regexp regexp)
       (message "This is our response to the user aborting the prompt"))
      (user-error
       (unhighlight-regexp regexp)
       (message "This is our response to the `user-error' signal"))
      (error
       (unhighlight-regexp regexp)
       (message "This is our response to the `error' signal")))))
```

<!--
#+findex: condition-case
#+findex: let
#+findex: unwind-protect
#+findex: cond
#+findex: message
#+findex: user-error
-->

The above function illustrates both the aforementioned concept of unwinding and the mechanics of handling signals. The abstract structure of ~condition-case~ looks to me like an amalgamation of ~let~, ~unwind-protect~, and ~cond~. These conditions may include the special handler of =:success=, as I show there. Granted, the code I wrote will never lead to that specific success case, though you can modify what happens after the prompt to, say, call ~message~ instead of the ~user-error~ function, which will then count as a successful conclusion. Otherwise, I think the expressions I wrote tell you exactly how this program responds to the signals it receives.

What I have not covered yet, is the aspect of ~condition-case~ that is like the ~let~, namely, how it binds the error data to a variable within this scope. In my implementation above, it is the ~nil~ you see there, meaning that I choose not to perform such a binding, as I have no use for its data. Below I decide to use it, just for the sake of demonstration.

```emacs-lisp
(defun my-prompt-with-temporary-highlight-and-signal-checks-with-error-report ()
  "Ask for confirmation and highlight all instances of a regexp while waiting."
  (let ((regexp "(defun"))
    (condition-case error-data-i-got
        (progn
          (highlight-regexp regexp)
          (if (y-or-n-p "Should we proceed or not? ")
              (user-error "You have decided to proceed; but we need to return a `user-error'")
            (error "You prefer not to continue; but we need to return an `error'")))
      (:success
       (unhighlight-regexp regexp)
       (message "No errors, but still need to unwind what we did, plus whatever else we want here")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (quit
       (unhighlight-regexp regexp)
       (message "This is our response to the user aborting the prompt")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (user-error
       (unhighlight-regexp regexp)
       (message "This is our response to the `user-error' signal")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got)))
      (error
       (unhighlight-regexp regexp)
       (message "This is our response to the `error' signal")
       (message "The error is `%s' and its data is `%S'" (car error-data-i-got) (cdr error-data-i-got))))))
```

There will be times when ~unwind-protect~ and ~condition-case~ are the right tools for the job. My hope is that these examples have given you the big picture view and you are now ready to write your own programs in Emacs Lisp.

## 16. 名前付き関数とラムダ関数の使い分け {#h:when-to-use-a-named-function-or-a-lambda-function}

<!--
#+findex: lambda
#+cindex: Anonymous and eponymous functions
-->

`lambda`は名前を持たない「無名関数」です。これは、`defun`で定義される「名前付き関数」と対になるものです。どちらを使うかは、多くの場合スタイルの問題といえます。ただし、状況によってはどちらか一方がより適している場合もあります。経験則としては、その関数を複数箇所で使う必要があるなら、`defun`で定義して名前で呼び出すべきです。そうしないと、実質的に使うたびに関数を再定義することになり、後でプログラムを修正するのが難しくなります。逆に、関数がその場限りで必要になるだけなら、`lambda`で十分でしょう。

場合によっては、名前付き関数の内部で`lambda`が使われることもあります。本書の他の例（[リスト要素へのマッピング](#h:mapping-through-a-list-of-elements)参照）を少し変更してみましょう。

```emacs-lisp
(defun my-increment-numbers-by-ten (numbers)
  "Add 10 to each number in NUMBERS and return the new list."
  (mapcar
   (lambda (number)
     (+ 10 number))
   numbers))

(my-increment-numbers-by-ten '(1 2 3))
;; => (11 12 13)
```

名前付き関数の中で`lambda`を定義し、`let`を使ってそれを繰り返し利用する、という使い方も考えられます。たとえば、`mapc`を使って副作用としてリスト内の人々に挨拶する関数が必要で、その挨拶処理自体を何度も`defun`で定義したくない、といった場合です：

```emacs-lisp
(defun my-greet-teams (&rest teams)
  "TEAMS 内の各人に挨拶し、チームごとの全人物を含むリストを返す.
TEAMS の各メンバーは文字列のリストである."
  (let* ((greet-name (lambda (name)
                       (message "Hello %s" name)))
         (greet-team-and-names (lambda (team)
                                 (message "Greeting the team of `%s'..." team)
                                 (mapc greet-name team))))
    (mapcar greet-team-and-names teams)))

(my-greet-teams
 '("Pelé" "Ronaldo")
 '("Maradona" "Messi")
 '("Beckenbauer" "Neuer")
 '("Platini" "Zidane")
 '("Baresi" "Maldini")
 '("Eusebio" "Cristiano Ronaldo")
 '("Xavi" "Iniesta")
 '("Charlton" "Shearer")
 '("Puskas" "Kubala")
 '("All of the Greece Euro 2004 squad ;)"))
;; => (("Pelé" "Ronaldo") ("Maradona" "Messi") ...)
```

<!--
#+cindex: View the echo area messages
-->

この例での挨拶メッセージは副作用であり、`*Messages*`バッファに出力されます。このバッファは<kbd>C-h e</kbd> （`view-echo-area-messages`）で素早く確認できます。`my-greet-teams`が具体的に何をしているかは、ここでは本質ではありません。注目すべきは、**名前付き関数の中で無名関数（`lambda`）を組み合わせて利用している点**です。

## 17. インタラクティブ関数をLisp呼び出しからも動作させる {#h:make-your-interactive-function-also-work-from-lisp-calls}

<!--
#+findex: interactive
#+findex: read-string
#+cindex: Interactive functions are commands
#+cinfex: The interactive specification
-->

Functions can be used interactively when they are declared with the ~interactive~ specification. This turns them into "commands". They can be called via their name by first doing {{{kbd(M-x)}}} (~execute-extended-command~) and then finding the command. They may also be assigned to a key and invoked directly by pressing that key. In its simplest form, the ~interactive~ specification is an unquoted list like ~(interactive)~. Here is a trivial example that calls ~read-string~ to produce a minibuffer prompt which accepts user input and returns it as a string:

```emacs-lisp
(defun my-greet-person ()
  (interactive)
  (message "Hello %s" (read-string "Whom to greet? ")))
```

The problem with the above implementation is that it is only useful in interactive use. If you want to issue such a greeting non-interactively through a program, you need to write another function that does practically the same thing except that it takes a =NAME= argument. Like this:

```emacs-lisp
(defun my-greet-person-with-name (name)
  "Greet person with NAME."
  (message "Hello %s" name))
```

You do not need to write two separate functions which practically do the same thing. Instead, you can have one function, with its parameters, which decides how to get the values of the arguments passed to it depending on if it is called interactively or programmatically. Consider this scenario:

```emacs-lisp
(defun my-greet-interactive-and-non-interactive (name)
  "Greet person with NAME.
When called interactively, produce a minibuffer prompt asking for NAME.

When called from Lisp, NAME is a string."
  (interactive (list (read-string "Whom to greet? ")))
  (message "Hello %s" name))
```

<!--
#+findex: defun
-->

The documentation I wrote there tells you exactly what is happening. Though let me explain ~interactive~ in further detail: it takes an argument, which is a list that corresponds to the argument list of the current ~defun~. In this case, the ~defun~ has a list of arguments that includes a single element, the =NAME=. Thus, ~interactive~ also has a list with one element, whose value corresponds to =NAME=. If the parameters were more than one, then the ~interactive~ would have to be written accordingly: each of its elements would correspond to the parameter at the same index on the list.

This list of expressions you pass to ~interactive~ essentially is the preparatory work that binds values to the parameters. When you call the above function interactively, you practically tell Emacs that in this case =NAME= is the return value of the call to ~read-string~. For more parameters, you get the same principle but I write it down just to be clear:

```emacs-lisp
(defun my-greet-with-two-parameters (name country)
  "Greet person with NAME from COUNTRY.
When called interactively, produce a minibuffer prompt asking for NAME
and then another prompt for COUNTRY.

When called from Lisp, NAME and COUNTRY are strings."
  (interactive
   (list
    (read-string "Whom to greet? ")
    (read-string "Where from? ")))
  (message "Hello %s of %s" name country))

(my-greet-with-two-parameters "Protesilaos" "Cyprus")
;; => "Hello Protesilaos of Cyprus"
```

Write ~interactive~ specifications with care and you will end up with a rich corpus of code that is economical and flexible.

## COPYING (著作権表示) {#h:copying}

> Copyright (C) 2025 Protesilaos Stavrou
>
> Permission is granted to copy, distribute and/or modify this document
> under the terms of the GNU Free Documentation License, Version 1.3 or
> any later version published by the Free Software Foundation; with no
> Invariant Sections, with the Front-Cover Texts being “A GNU Manual,” and
> with the Back-Cover Texts as in (a) below.  A copy of the license is
> included in the section entitled “GNU Free Documentation License.”
>
> (a) The FSF’s Back-Cover Text is: “You have the freedom to copy and
> modify this GNU manual.”

{% include licenses/gfdl-1.3.md %}

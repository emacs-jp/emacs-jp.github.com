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

`eval-last-sexp`はシンボル（変数名など）に対しても機能します（[シンボル、バランスの取れた式、そしてクォート](#h:symbols-balanced-expressions-and-quoting)参照）。例えば、変数`buffer-file-name`の末尾にカーソルを置いて<kbd>C-x C-e</kbd>（`eval-last-sexp`）を使えば、その変数の値、つまり現在編集中のファイルへのパス（ファイルに関連付けられていなければ`nil`）が得られます。
<!--
#+findex: eval-expression
-->

しかし、場合によっては上記の方法が適さないこともあります。例えば、現在のバッファのファイルパスをコピーするコマンドを書きたいとしましょう。そのためには、コード内で変数`buffer-file-name`の値をテストする必要があります（[データ構造としてのバッファ](#h:buffers-as-data-structures)参照）。しかし、そのために実際のファイルに`buffer-file-name`と打ち込み、前述の評価コマンドを実行し、その後で編集内容を元に戻す、なんてことはしたくないでしょう。それは面倒ですし、ミスも起こりやすいものです！現在のバッファの文脈でElispを手軽に実行する最善の方法は<kbd>M-:</kbd>（`eval-expression`）をタイプすることです。これによりミニバッファが開き、評価したいコードを入力するよう求められます。そこで<kbd>RET</kbd>（Enterキー）を押せば実行されます。評価は、最後にカレントだったバッファ（`eval-expression`を呼び出す直前にカレントだったバッファ）を基準に行われます。

以下のEmacs Lispコードを「(i) ファイルに対応するバッファ」と、「(ii) ディスク上のどのファイルとも関連付けられていないバッファ」で、試してみるとよいでしょう。

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

これらに加えて、Emacsの自己文書化機能を利用して、現在の状態を知ることができます。例えば、変数`major-mode`のバッファローカルな値について知りたければ、<kbd>C-h v</kbd>（`describe-variable`）を実行し、その変数を検索します。結果として表示されるヘルプバッファには、`major-mode`の現在の値が示されます。このヘルプコマンドや、`describe-function`、`describe-keymap`、`describe-key`、`describe-symbol`のような他の多くのコマンドは、Emacsが特定の対象について何を知っているかについての情報を提供します。ヘルプバッファには、関連情報、例えばその関数を定義しているファイルへのパスや、変数がバッファローカルとして宣言されているかどうかなどが表示されます。

<!--
#+cindex: Emacs is self-documenting
-->

Emacsが「**自己文書化されている**（self-documenting）」というのは、自身の状態を報告するからです。ヘルプバッファを明示的に更新する必要はありません。これは、関連するコードが評価されることによって自動的に行われます。つまりEmacsは、あなたが扱っている対象が何であれ、その最新の値を効果的に表示してくれるのです。

## 3. 副作用と戻り値 {#h:side-effect-and-return-value}

Emacs Lispには関数があります。関数は入力を受け取り、出力を生成します。その最も純粋な形式では、関数は値を返すだけの計算です。純粋な関数は周囲の状態（環境）を何も変更しません。ある関数の戻り値が別の関数の入力となり、処理が連鎖していくのです。この仕組みがあるからこそ、「もしこの処理が成功したら、次にこの処理を行い、失敗したら別の処理をする（あるいは何もしない）」といった条件に応じた流れを組み立てられます。

ElispはEmacsを拡張し、制御するための言語です。そのため、Elispの処理はエディタ自身の状態にも影響を及ぼします。関数を実行すると、カーソル位置へのテキスト挿入、バッファの削除、ウィンドウの新規作成といった、永続的な変化（副作用）を引き起こすことがあります。こうした変化は、後続の関数呼び出しに影響を与える可能性があります。例えば、ある関数が特定のバッファを削除してしまえば、その後に同じバッファへ書き込もうとしていた別の関数は、対象のバッファが存在しないため、もはや処理を実行できません。

Elispのコードを書く際には、関数の「戻り値」と「副作用」の両方を考慮する必要があります。副作用への配慮が足りないと、環境への意図しない変更が原因で、予期せぬ結果を招くことになります。しかし、副作用を注意深く、意図的に活用すれば、Elispの持つ力を最大限に引き出すことができます。たとえば、「新しいバッファを作り、そのバッファに移動し、テキストを書き込み、好みの場所にファイルとして保存し、元の場所に戻ってくる。ただし、作成したバッファは開いたままにしておく」といった一連の動作を行う関数を考えてみましょう。これらはすべて副作用ですが、非常に便利なものです。さらに、この関数が意味のある戻り値（例えば、作成したバッファそのもの）を返すようにすれば、後続の関数がその戻り値を使って、別のフレームでそのバッファを表示したり、中のテキストを大きくしたりといった、さらなる操作を行うことも可能です。

あなたがElispを書くとき、戻り値と副作用の両方を考慮に入れなければなりません。もしあなたが不注意であれば、環境への考慮不足の変更すべてによって引き起こされる意図しない結果を得るでしょう。しかし、もしあなたが副作用を細心の注意を払って使用するなら、あなたはElispをその完全な可能性へと引き出す力を与えられます。例えば、あなたが「バッファを作成し、そこへ行き、テキストを書き、そのバッファを私の好む場所のファイルに保存し、そしてこの関数を呼び出す前にいた場所に戻り、作成したバッファは開いたままにする」という論理に従う関数を定義すると想像してください。これらはすべて副作用であり、それらはすべて有用です。あなたの関数はまた、別の関数の入力としてあなたが利用できる、何らかの意味のある戻り値を持つかもしれません。例えば、あなたの関数はそれが生成したバッファオブジェクトを返すでしょう。そうすれば、次の関数はそこで何か、例えばそのバッファを別のフレームに表示し、そのテキストを大きくする、といったことができます。

要するに、エディタの状態を操作し、Emacsを自分の思い描いた通りに動かすことが目標です。そのためには、コードが副作用を持つことが必要な場合もあります。一方で、副作用が全く不要であったり、むしろ意図した結果の邪魔になったりすることもあります。何が必要で何が不要か、その見極めは、経験を積み、スキル（[シンボル、バランスの取れた式、クォーティング](#h:symbols-balanced-expressions-and-quoting)参照）の幅を広げていく中で、自然と磨かれていく直感のようなものです。心配はいりません、気楽にいきましょう！

## 4. データ構造としてのバッファ {#h:buffers-as-data-structures}

<!--
#+findex: point
#+findex: point-min
#+findex: point-max
#+findex: line-beginning-position
#+findex: re-search-forward
-->

Emacsのバッファは、データを文字の連なり（シーケンス）として保持しています。ファイルを開いたときに画面に表示されるテキストなどがこれにあたります。各文字は特定の位置に存在し、その位置は数値で表されます。関数`point`は現在のポイント（通常はカーソルがある場所）の位置を数値で返します（[Emacs Lispを評価する](#h:evaluate-emacs-lisp)参照）。バッファの先頭位置では、`point`は`1`を返します（[副作用と戻り値](#h:side-effect-and-return-value)参照）。バッファ内の位置を返す関数は他にも`point-min`（先頭位置）、`point-max`（末尾位置）、`line-beginning-position`（行頭位置）、`re-search-forward`（前方検索）など多数存在します。これらの関数の中には、例えば`re-search-forward`がカーソルを検索に一致した箇所へ移動させるように、副作用を持つものもあります。

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


データ構造のように扱えるバッファ（[データ構造としてのバッファ](#h:buffers-as-data-structures)参照）と同じように、どんなテキストにもプロパティ（属性情報）を関連付けることができます。これはEmacs Lispを使って参照できるメタデータ（付加情報）です。例えば、プログラミング用バッファで表示されるシンタックスハイライト（構文の強調表示）は、このテキストプロパティの効果によるものです。何らかの関数が、対象となるテキストに「プロパティを付与（propertise）」したり「装飾（fontify）」したりする処理を担当し、「フェイス（face）」と呼ばれるオブジェクトを適用します。フェイスとは、フォントの種類や太さ、文字色や背景色といった、文字の体裁や色に関する属性をまとめたものです。カーソル位置の文字のテキストプロパティに関する情報をヘルプバッファで確認するには、<kbd>M-x</kbd>（`execute-extended-command`）に続けて`describe-char`コマンドを実行します。すると、カーソル下の文字、表示に使われているフォント、文字コード、そしてその文字が持つテキストプロパティが表示されます。

例えば、あなたが独自のメジャーモードを作成しているとしましょう。実験の初期段階として、`fundamental-mode`のバッファ内で、「`I have properties`」というフレーズが現れるすべての箇所に、手動でテキストプロパティを追加したいと考え、次のようなコードを書くかもしれません（[検索結果のマッチデータ](#h:the-match-data-of-the-last-search)参照）：

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

続けて<kbd>M-:</kbd>（`eval-expression`）を実行し、プロンプトに対して`(my-add-properties)`と入力して<kbd>RET</kbd> を押し、先ほど定義した関数を呼び出します。どうでしょうか？ バッファ内の「`I have properties`」の部分の見た目が変わりましたか？ このコードが適用しているフェイスは`error`という名前のものです。この単語の意味（エラー）はここでは無視してください。通常、`error`フェイスは非常に目立つスタイル（例えば赤文字など）で定義されていることが多いため、プロパティが適用されたことが分かりやすいだろうという理由だけで、このフェイスを選びました。ただし、色などのスタイル現在使用しているテーマによっては表示が異なります。


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
 * **データとしてのリスト**：リストが「評価」されない場合、その中の要素は特別な意味を持ちません。リスト全体がそのまま、変更されることなくデータとして返されます。リストを評価させずにデータとして扱いたい場合は、リストの前にシングルクォート`'`を付けます。例えば、`'("Protesilaos" "Prot" "Cyprus")`と書くと、これは3つの文字列要素からなるリストとして、そのまま返されます。

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


Emacs Lispのデータ型の中には、「自己評価（self-evaluating）」するものがあります。これは、それらを評価しても、見た目通りのそれ自身がそのまま値として返ってくる、という意味です。例えば、文字列`"Protesilaos"`を評価すると、結果は`"Protesilaos"`です。これは文字列の他に、数値、キーワード（`:hello`のようなコロンで始まるもの）、シンボル、そして特別な値である`nil`や`t`にも当てはまります。以下は、これらのデータ型のサンプルを関数`list`を使ってリストにした例です：

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

Keep in mind that Emacs Lisp has a concept of "macro", which basically is a templating system to write code that actually expands into some other code which is then evaluated. Inside of a macro, you control how quoting is done, meaning that the aforementioned may not apply to calls that involve the macro, even if they are still used inside of the macro's expanded form ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)).


ただし、Emacs Lispには「マクロ」という概念があることに注意してください。マクロは基本的に、コードを生成するためのテンプレートのようなもので、マクロ自身が評価されるのではなく、まず別のコードに「展開」され、その展開されたコードが評価されます。マクロの定義内では、引数の評価やクオートの扱いを細かく制御できます。そのため、マクロ呼び出しの際には、これまで説明したクオートのルールがそのまま当てはまらない場合があります。たとえ展開後のコード内では通常のルールが適用されるとしてもです（[マクロやスペシャルフォーム内での評価](#h:evaluation-inside-of-a-macro-or-special-form)参照）。

<!--
#+findex: quote
#+findex: function
-->

Emacs Lispのコードを読み進めると、`#'some-symbol`のように、ハッシュ記号`#`が付いたクオートを目にすることがあるでしょう。これは「シャープクオート」と呼ばれ、通常のシングルクォート`'`と同様に評価を抑制しますが、特に「関数」を参照している、という明確な意味合いが加わります。これを使うことで、プログラマは式の意図（これは関数である）をより明確に表現でき、またバイトコンパイラ（コードをより効率的な形式に変換するプログラム）が内部的なチェックや最適化を行いやすくなる可能性があります。このシャープクオートに関連して、通常のクオート`'`に対応する`quote`関数と、シャープクオート`#'`に対応する`function`関数についても調べてみると良いでしょう。

## 7. リスト内部の部分評価 {#h:partial-evaluation-inside-of-a-list}

You already have an idea of how Emacs Lisp code looks like ([Symbols, balanced expressions, and quoting](#h:symbols-balanced-expressions-and-quoting)). You have a list that is either evaluated or taken as-is. There is another case where a list should be partially evaluated or, more specifically, where it should be treated as data instead of a function call with some elements inside of it still subject to evaluation.

<!--
#+cindex: Declare a variable
-->

In the following code block, I am defining a variable called ~my-greeting-in-greek~, which is a common phrase in Greek that literally means "health to you" and is pronounced as "yah sou". Why Greek? Well, you got the ~lambda~ that engendered this whole business with Lisp, so you might as well get all the rest ([When to use a named function or a lambda function](#h:when-to-use-a-named-function-or-a-lambda-function))!

```emacs-lisp
(defvar my-greeting-in-greek "Γεια σου"
  "Basic greeting in Greek to wish health to somebody.")
```

<!--
#+findex: message
-->

Now I want to experiment with the ~message~ function to better understand how evaluation works. Let me start with the scenario of quoting the list, thus taking it as-is:

```emacs-lisp
(message "%S" '(one two my-greeting-in-greek four))
;;=> "(one two my-greeting-in-greek four)"
```

You will notice that the variable ~my-greeting-in-greek~ is not evaluated. I get the symbol, the actual ~my-greeting-in-greek~, but not the value it represents. This is the expected result, because the entire list is quoted and, ipso facto, everything inside of it is not evaluated.

Now check the next code block to understand how I can tell Emacs that I want the entire list to still be quoted but for ~my-greeting-in-greek~ in particular to be evaluated, so it is replaced by its value:

```emacs-lisp
(message "%S" `(one two ,my-greeting-in-greek four))
;; => "(one two \"Γεια σου\" four)"
```

<!--
#+findex: concat
#+cindex: Quasi quote
#+cindex: Comma operator
-->

Pay close attention to the syntax here. Instead of a single quote, I am using the backtick or back quote, which is also known as a "quasi quote" in our case. This behaves like the single quote except for anything that is preceded by a comma. The comma is an instruction to "evaluate the thing that follows" and only works inside of a quasi-quoted list. The "thing" that follows is either a symbol or a list. The list can, of course, be a function call. Let me then use ~concat~ to greet a certain person all while returning everything as a list:

```emacs-lisp
(message "%S" `(one two ,(concat my-greeting-in-greek " " "Πρωτεσίλαε") four))
;; => "(one two \"Γεια σου Πρωτεσίλαε\" four)"
```

Bear in mind that you would get an error if you were not quoting this list at all, because the first element ~one~ would be treated as the symbol a function, which would be called with all other elements as its arguments. Chances are that ~one~ is not defined as a function in your current Emacs session or those arguments are not meaningful to it, anyway. Plus, ~two~ and ~four~ would then be treated as variables, since they are not quoted, in which case those would have to be defined as well, else more errors would ensue.

<!--
#+cindex: Splicing in general
-->

Other than the comma operator, there is the =,@= (how is this even pronounced? "comma at", perhaps?), which is notation for "splicing". This is jargon in lieu of saying "the return value is a list and I want you to remove the outermost parentheses of it." In effect, the code that would normally return ='(one two three)= now returns =one two three=. This difference may not make much sense in a vacuum, though it does once you consider those elements as expressions that should work in their own right, rather than simply be elements of a quoted list. I will not elaborate on an example here, as I think this is best covered in the context of defining macros ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)).

Chances are you will not need to use the knowledge of partial evaluation. It is more common in macros, though can be applied anywhere. Be aware of it regardless, as there are scenaria where you will, at the very least, want to understand what some code you depend on is doing.

Lastly, since I introduced you to some Greek words, I am now considering you my friend. Here is a joke from when I was a kid. I was trying to explain some event to my English instructor. As I lacked the vocabulary to express myself, I started using Greek words. My instructor had a strict policy of only responding to English, so she said "It is all Greek to me." Not knowing that her answer is an idiom for "I do not understand you", I blithely replied, "Yes, Greek madame; me no speak England very best." I was not actually a beginner at the time, though I would not pass on the opportunity to make fun of the situation. Just how you should remember to enjoy the time spent tinkering with Emacs. But enough of that! Back to reading this book.

## 8. マクロやスペシャルフォーム内での評価 {#h:evaluation-inside-of-a-macro-or-special-form}

In the most basic case of Emacs Lisp code, you have lists that are either evaluated or not ([Partial evaluation inside of a list](#h:symbols-balanced-expressions-and-quoting][Symbols, balanced expressions, and quoting]]). If you get a little more fancy, you have lists that are only partially evaluated ([[#h:partial-evaluation-inside-of-a-list)). Sometimes though, you look at a piece of code and cannot understand why the normal rules of quoting and evaluation do not apply. Before you see this in action, inspect a typical function call that also involves the evaluation of a variable:

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")
```

<!--
#+findex: concat
#+cindex: Evaluation inside of a function call
-->

You encountered this code in the section about partial evaluation. What you have here is a call to the function ~concat~, followed by three arguments. One of these arguments is a variable, the ~my-greeting-in-greek~. When this list is evaluated, what Emacs actually does is to first evaluate the arguments, including ~my-greeting-in-greek~, in order to get their respective values and only then to call ~concat~ with those values. You can think of the entire operation as follows:

- Here is a list.
- It is not quoted.
- So you should evaluate it.
- The first element is the name of the function.
- The remaining elements are arguments passed to that function.
- Check what the arguments are.
- Evaluate each of the arguments to resolve it to its actual value.
- Strings are self-evaluating, while the ~my-greeting-in-greek~ is a variable.
- You now have the value of each of the arguments, including the value of the symbol ~my-greeting-in-greek~.
- Call ~concat~ with all the values you got.

In other words, the following two yield the same results (assuming a constant ~my-greeting-in-greek~):

```emacs-lisp
(concat my-greeting-in-greek " " "Πρωτεσίλαε")

(concat "Γεια σου" " " "Πρωτεσίλαε")
```

<!--
#+findex: setq
-->

This is predictable. It follows the basic logic of the single quote: if it is quoted, do not evaluate it and return it as-is, otherwise evaluate it and return its value. But you will find plenty of cases where this expected pattern is seemingly not followed. Consider this common case of using ~setq~ to bind a symbol to the given value:

```emacs-lisp
(setq my-test-symbol "Protesilaos of Cyprus")
```

The above expression looks like a function call, meaning that (i) the list is not quoted, (ii) the first element is the name of a function, and (iii) the remaining elements are arguments passed to that function. In a way, this is all true. Though you would then expect the ~my-test-symbol~ to be treated as a variable, which would be evaluated in place to return its result which would, in turn, be the actual argument passed to the function. However, this is not how ~setq~ works. The reason is that it is a special case that internally does this:

```emacs-lisp
(set 'my-test-symbol "Protesilaos of Cyprus")
```

<!--
#+findex: setq
#+findex: defun
-->

This is where things are as expected. There is no magic happening behind the scenes. The ~setq~, then, is a convenience for the user to not quote the symbol each time. Yes, this makes it a bit more difficult to reason about it, though you get used to it and eventually it all makes sense. Hopefully, you will get used to such special forms, as you find them with ~setq~ but also with ~defun~, among many others. Here is a function you have already seen:

```emacs-lisp
(defun my-greet-person-from-country (name country)
  "Say hello to the person with NAME who lives in COUNTRY."
  (message "Hello %s of %s" name country))
```

If the normal rules of evaluation applied, then the list of parametes should be quoted. Otherwise, you would expect =(name country)= to be interpreted as a function call with ~name~ as the symbol of the function and ~country~ as its argument which would also be a variable. But this is not what is happening because ~defun~ will internally treat that list of parameters as if it was quoted.

<!--
#+findex: let
-->

Another common scenario is with ~let~ ([Control flow with ~if-let*~ and friends](#h:control-flow-with-if-let-and-friends)). Its general form is as follows:

```emacs-lisp
;; This is pseudo-code
(let LIST-OF-LISTS-AS-VARIABLE-BINDINGS
  BODY-OF-THE-FUNCTION)
```

The =LIST-OF-LISTS-AS-VARIABLE-BINDINGS= is a list in which each element is a list of the form =(SYMBOL VALUE)=. Here is some actual code:

```emacs-lisp
(let ((name "Protesilaos")
      (country "Cyprus"))
  (message "Hello %s of %s" name country))
```

Continuing with the theme of special forms, if ~let~ was a typical function call, the =LIST-OF-LISTS-AS-VARIABLE-BINDINGS= would have to be quoted. Otherwise, it would be evaluated, in which case the first element would be the name of the function. But that would return an error, as the name of the function would correspond to another list, the =(name "Protesilaos")=, rather than a symbol. Things work fine with ~let~ because it internally does the quoting of its =LIST-OF-LISTS-AS-VARIABLE-BINDINGS=.

<!--
#+findex: use-package
-->

Expect similar behaviour with many special forms as well as with macros such as the popular ~use-package~, which is used to configure packages inside of your Emacs initialisation file. How each of those macros works depends on the way it is designed. I will not delve into the technicalities here, as I want the book to be useful long-term, focusing on the principles rather than the implementation details that might change over time.

<!--
#+findex: pp-macroexpand-last-sexp
#+cindex: Pretty print or expand a macro
-->

To learn what a given macro actually expands to, place the cursor at the end of its closing parenthesis and call the command ~pp-macroexpand-last-sexp~. It will produce a new buffer showing the expanded Emacs Lisp code. This is what is actually evaluated in the macro's stead.

<!--
#+findex: defmacro
#+vindex: default-directory
#+cindex: Defining macros
#+cindex: Splicing within a macro
-->

With those granted, it is time to write a macro. This is like a template, which empowers you to not repeat yourself. Syntactically, a macro will most probably depend on the use of the quasi-quote, the comma operator, and the mechanics of splicing ([Partial evaluation inside of a list](#h:partial-evaluation-inside-of-a-list)). Here is a simple scenario where we want to run some code in a temporary buffer while setting the ~default-directory~ to the user's home directory.

```emacs-lisp
(defmacro my-work-in-temp-buffer-from-home (&rest expressions)
  "Evaluate EXPRESSIONS in a temporary buffer with `default-directory' set to the user's home."
  `(let ((default-directory ,(expand-file-name "~/")))
     (with-temp-buffer
       (message "Running all expression from the `%s' directory" default-directory)
       ,@expressions)))
```

In this definition, the =&rest= makes the following parameter a list. So you can pass an arbitrary number of arguments to it, all of which will be collected into a single list called =EXPRESSIONS=. The judicious use of partial evaluation ensures that the macro will not be evaluated right now but only when it is called. The arguments passed to it will be placed where you have specified. Here is a call that uses this macro:

```emacs-lisp
(progn
  (message "Now we are doing something unrelated to the macro")
  (my-work-in-temp-buffer-from-home
   (message "We do stuff inside the macro")
   (+ 1 1)
   (list "Protesilaos" "Cyprus")))
```

If you place the cursor at the closing parenthesis of ~my-work-in-temp-buffer-from-home~, you will be able to confirm what it expands to by typing {{{kbd(M-x)}}} (~execute-extended-command~) and then invoking the command ~pp-macroexpand-last-sexp~. This is what I get:

```emacs-lisp
(let ((default-directory "/home/prot/"))
  (with-temp-buffer
    (message "Running all expression from the `%s' directory" default-directory)
    (message "We do stuff inside the macro")
    (+ 1 1)
    (list "Protesilaos" "Cyprus")))
```

Piecing it together with the rest of the code in its context, I arrive at this:

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

With this example in mind, consider Elisp macros to be a way of saying "this little thing here helps me express this larger procedure more succinctly, while the actual code that runs is still that of the latter."

The above macro I wrote has its body start with a quasi-quote, so you do not get to appreciate the nuances of evaluation within it. Let me show you this other approach, instead, where I write a macro that lets me define several almost identical interactive functions ([Make your interactive function also work from Lisp calls](#h:make-your-interactive-function-also-work-from-lisp-calls)).

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

The ~my-define-command~ can be broadly divided into two parts: (i) what gets evaluated outright and (ii) what gets expanded for further evaluation. The latter part starts with the quasi-quote. This distinction is important when we call the macro, because the former part will be executed right away so if we hit the error, it will never expand and then run the =EXPRESSIONS=. Try ~pp-macroexpand-last-sexp~ with the following to see what I mean. For your convenience, I include the macro expansions right below each case.

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

Do you need macros? Not always, though there will be cases where a well-defined macro makes your code more elegant. What matters is that you have a sense of how evaluation works so that you do not get confused by all those parentheses. Otherwise you might expect something different to happen than what you actually get.

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

`mapcar`は戻り値を集めて新しいリストを作りましたが、時にはこれが無駄なこともあります。例えば、ファイルに関連付けられていて、かつ未保存のバッファをすべて保存する関数を評価したいとしましょう。このシナリオでは、結果を集めることには関心がなく、単にバッファを直接保存するという副作用だけが欲しいのです。この目的のためには`mapc`を使うことができます。`mapc`は常に、操作対象となった元のリストを返します。

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

As you use Emacs Lisp to do things programmatically, you encounter cases where you need to move away from where you are. You may have to switch to another buffer, change to the window of a given buffer, or even modify what is visible in the buffer you are editing. At all times, this involves one or more side effects which, most probably, should be undone when your function finishes its job ([Side effect and return value](#h:side-effect-and-return-value)).

<!--
#+findex: point
#+findex: save-excursion
#+cindex: Restore the point
-->

Perhaps the most common case is to restore the ~point~. You have some code that moves back or forth in the buffer to perform a match for a given piece of text. But then, you need to leave the cursor where it originally was, otherwise the user will lose their orientation. Wrap your code in a ~save-excursion~ and you are good to go, as I show elsewhere ([The match data of the last search](#h:the-match-data-of-the-last-search)):

```emacs-lisp
(save-excursion ; restore the `point' after you are done
  MOVE-AROUND-IN-THIS-BUFFER)
```

<!--
#+findex: save-window-excursion
#+findex: select-window
#+cindex: Restore the selected window
-->

Same principle for ~save-window-excursion~, which allows you to select another window, such as with ~select-window~, move around in its buffer, and then restore the windows as they were:

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

The ~save-restriction~ allows you to restore the current narrowing state of the buffer. You may then choose to either ~widen~ or ~narrow-to-region~ (and related commands like ~org-narrow-to-subtree~), do what you must, and then restore the buffer to its original state.

```emacs-lisp
;; Here we assume that we start in a widened state.  Then we narrow to
;; the current Org heading to get all of its contents as one massive
;; string.  Then we widen again, courtesy of `save-restriction'.
(save-restriction
  (org-narrow-to-subtree)
  (buffer-string))
```

Depending on the specifics, you will want to combine the aforementioned. Beware that the documentation of ~save-restriction~ tells you to use ~save-excursion~ as the outermost call. Other than that, you will also find cases that require a different approach to perform some conditional behaviour ([Run some code or fall back to some other code](#h:run-some-code-or-fall-back-to-some-other-code)).

## 12. `if`, `cond`などによる基本的な制御フロー {#h:basic-control-flow-with-if-cond-and-others}

<!--
#+findex: defun
#+findex: forward-line
-->

You do not need any conditional logic to perform basic operations. For example, if you write a command that moves 15 lines down, it will naturally stop at the end of the buffer when it cannot move past the number you specified. Using ~defun~, you write an interactive function (i.e. a "command") to unconditionally move down 15 lines using ~forward-line~ internally (call it with a negative number to move in the opposite direction):

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

The ~my-15-lines-down~ is about as simple as it gets: it wraps around a basic function and passes to it a fixed argument, in this case the number =15=. Use {{{kbd(M-x)}}} (~execute-extended-command~) and then call this command by its name. It works! Things get more involved as soon as you decide to perform certain actions only once a given condition is met. This "control flow" between different branches of a logical sequence is expressed with ~if~, ~when~, ~unless~, and ~cond~, among others. Depending on the specifics of the case, ~and~ as well as ~or~ may suffice.

<!--
#+findex: eobp
#+findex: string-match-p
#+findex: stringp
#+cindex: Predicate functions
-->

How about you make your ~my-15-lines-down~ a bit smarter? When it is at the absolute end of the buffer, have it move 15 lines up. Why? Because this is a demonstration, so why not? The predicate function that tests if the point is at the end of the buffer is ~eobp~. A "predicate" is a function that returns true, technically non-~nil~, when its condition is met, else it returns ~nil~ ([Side effect and return value](#h:side-effect-and-return-value)). As for the weird name, the convention in Emacs Lisp is to end predicate functions with the =p= suffix: if the name of the function consists of multiple words, typically separated by dashes, then the predicate function is named =NAME-p=, like ~string-match-p~, otherwise it is =NAMEp=, like ~stringp~.

```emacs-lisp
(defun my-15-lines-down-or-up ()
  "Move at most 15 lines down or go back if `eobp' is non-nil."
  (interactive)
  (if (eobp)
      (forward-line -15)
    (forward-line 15)))
```

Evaluate this function, then type {{{kbd(M-x)}}} (~execute-extended-command~) and invoke ~my-15-lines-down-or-up~ to get a feel for it. Below is a similar idea, which throws and error and exits what it was doing if ~eobp~ returns non-~nil~:

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

A quirk of Emacs Lisp, which may be a feature all along, is how indentation is done. Just mark the code you have written and type {{{kbd(TAB)}}}: Emacs will take care to indent it the way it should be done. In the case of the ~if~ statement, the "then" part is further in than the "else" part of the logic. There is no special meaning to this indentation: you could write everything on a single line like =(if COND THIS ELSE)=, which looks like your typical list, by the way ([Symbols, balanced expressions, and quoting](#h:symbols-balanced-expressions-and-quoting)). What the indentation does is help you identify imbalances in your parentheses. If the different expressions all line up in a way that looks odd, then you are most probably missing a parentheses or have too many of them. Generally, expressions at the same level will all line up the same way. Those deeper in will have more indentation, and so on. Experience will allow you to spot mistakes with mismatching parentheses. But even if you do not identify them, you will get an error eventually. Rest assured!

The way ~if~ is written is like a function that takes two or more arguments. The "or more" all counts as part of the "else" logic. As such, =(if COND THIS)= has no "else" consequence, while =(if COND THIS ELSE1 ELSE2 ELSE3)= will run =ELSE1=, =ELSE2=, and =ELSE3= in order as part of the "else" branch. Here is how this looks once you factor in proper indentation:

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

Now what if the =THIS= part needs to be more than one function call? Elisp has the ~progn~ form, which you can use to wrap function calls and pass them as a single argument. Putting it all together, your code will now look this like:

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

If you do not need the "else" part, use ~when~ to express your intention. Internally, this is a macro which actually stands for =(if COND (progn EXPRESSIONS))=, where =EXPRESSIONS= is one or more expressions. A ~when~ looks like this:

```emacs-lisp
(when COND
  THIS1
  THIS2
  THIS3)
```

<!--
#+findex: unless
-->

Similarly, the ~unless~ has the meaning of =(when (not COND) EXPRESSIONS)=. It, too, is a macro that expands to an ~if~ statement:

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

When the condition you are testing for has multiple parts, you can rely on ~and~ as well as ~or~:

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

Depending on the specifics of the case, the combination of multiple ~if~, ~when~, ~or~, ~and~ will look awkward. You can break down the logic to distinct conditions, which are tested in order from top to bottom, using ~cond~. The way ~cond~ is written is as a list of lists, which do not need quoting ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)). In abstract, it looks like this:

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

Each of the consequences can be any number of expressions, like you saw above with ~when~. This is a toy function to show how ~cond~ behaves:

```emacs-lisp
(defun my-toy-cond (argument)
  "Return a response depending on the type of ARGUMENT."
  (cond
   ((and (stringp argument)
         (string-blank-p argument))
    (message "You just gave me a blank string; try harder!"))
   ((stringp argument)
    (message "I see you can do non-blanks string; I call that progress."))
   ((null argument)
    (message "Yes, the nil is an empty list like (), but do not worry about it"))
   ((listp argument)
    (message "Oh, I see you are in the flow of using lists!"))
   ((symbolp argument)
    (message "What's up with the symbols, mate?"))
   ((natnump argument)
    (message "I fancy those natural numbers!"))
   ((numberp argument)
    (message "You might as well be a math prodigy!"))
   (t
    (message "I have no idea what type of thing your argument `%s' is" argument))))
```

I want you to evaluate it and pass it different arguments to test what it does ([Evaluate Emacs Lisp](#h:evaluate-emacs-lisp)). Here are two examples:

```emacs-lisp
(my-toy-cond "")
;; => "You just gave me a blank string; try harder!"

(my-toy-cond '(1 2 3))
;; => "Oh, I see you are in the flow of using lists!"
```

All of the above are common in Emacs Lisp. Another powerful macro is ~pcase~, which we will consider separately due to its particularities ([Pattern match with ~pcase~ and related](#h:pattern-match-with-pcase-and-related)).

## 13. `if-let*`の仲間を使用してフローを制御する {#h:control-flow-with-if-let-and-friends}

<!--
#+findex: let
#+findex: let*
#+cindex: Let bind variables in the current scope
-->

The ~let~ and ~let*~ declare variables that are available only within the current scope, else the =BODY= of the ~let~. As such:

```emacs-lisp
(let BINDINGS
  BODY)

(let ((variable1 value1)
      (variable2 value2))
  BODY)
```

The =BINDINGS= is a list of lists, which does not need to be quoted ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)). While =BODY= consists of one or more expressions, which I have also named =EXPRESSIONS= elsewhere in this book. The difference between ~let~ and ~let*~ (pronounced "let star") is that the latter makes earlier bindings available to later bindings. Like this:

```emacs-lisp
;; This works because `greeting' can access `name' and `country',
;; courtesy of `let*':
(let* ((name "Protesilaos")
       (country "Cyprus")
       (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))

;; But this fails...
(let ((name "Protesilaos")
      (country "Cyprus")
      (greeting (format "Hello %s of %s" name country)))
  (DO-STUFF-WITH greeting))
```

Sometimes what you want to do is create those bindings if---and only if---they are all non-~nil~. If their value is ~nil~, then they are useless to you, in which case you do something else ([Basic control flow with ~if~, ~cond~, and others](#h:basic-control-flow-with-if-cond-and-others)). Values may or may not be ~nil~ when you are creating a binding with the return value of a function call or some other variable. You could always write code like this:

```emacs-lisp
(let ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
      (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
  (if (and variable1 variable2) ; simply test both for non-nil
      THIS
    ELSE))
```

<!--
#+findex: if-let*
-->

But you can do the same with ~if-let*~, where the =THIS= part runs only if all the bindings are non-~nil~:

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

In the =ELSE= part, the bindings ~variable1~ and ~variable2~ do not exist: they only exist for the =THIS= part of the code.

<!--
#+findex: when-let*
-->

The ~when-let*~ is the same as ~when~, meaning that it has no "else" logic. If one of its bindings is ~nil~, then the whole ~when-let*~ returns ~nil~. No need to belabour that point.

As you dig dipper into the Emacs Lisp ecosystem, you will come across uses of ~if-let*~ that (i) create multiple bindings like ~let~ or ~let*~ but (ii) also call a predicate function to test if they should continue with the =THIS= part of their logic. Remember that ~if-let*~ goes straight to =ELSE= if one of its bindings returns ~nil~. Consider this example:

```emacs-lisp
(if-let* ((variable1 (SOME-FUNCTION SOME-ARGUMENT))
          ;; The _ signifies intent: "do not bind this; I only care
          ;; about the return value being non-nil".  What we are doing
          ;; here is test if `variable1' is a string: if it is, we
          ;; continue with the bindings, otherwise we move to the ELSE
          ;; part of the code.
          (_ (string-match-p variable1))
          (variable2 (OTHER-FUNCTION OTHER-ARGUMENT)))
    THIS
  ELSE)
```

There is no inherently superior way of doing things. It is a matter of using the right tool for the task at hand. Sometimes you want the bindings to be created, even if their value is ~nil~. Choose what makes sense.

## 14. `pcase`によるパターンマッチ {#h:pattern-match-with-pcase-and-related}

<!--
#+findex: pcase
#+vindex: major-mode
-->

Once you get in the flow of expressing your thoughts with Emacs Lisp, you will be fluent in the use of ~if~, ~cond~, and the like ([Control flow with ~if-let*~ and friends](#h:basic-control-flow-with-if-cond-and-others][Basic control flow with ~if~, ~cond~, and others]]). You might even get more fancy if ~if-let*~ ([[#h:control-flow-with-if-let-and-friends)). However you go about it, there are some cases that arguably benefit from more succinct expressions. This is where ~pcase~ comes in. At its more basic formulation, it is like ~cond~, in that it tests the return value of a given expression against a list of conditions. Here is an example that compared the buffer-local value of the variable ~major-mode~ for equality against a couple of known symbols:

```emacs-lisp
(pcase major-mode
  ('org-mode (message "You are in Org"))
  ('emacs-lisp-mode (message "You are in Emacs Lisp"))
  (_ (message "You are somewhere else")))
```

The above is the same idea as this ~cond~:

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

Some programmers may argue that ~pcase~ is more elegant. I think it is true in this specific example, though I remain flexible and practical: I will use whatever makes more sense for the code I am writing. While on the topic of elegance, I should inform you that practically all of the conditional logic can be done in a way that may seem unexpected. Consider how my examples in this book make repetitive use of ~message~, when in reality the only part that changes is the actual string/argument passed to that function. This will work just as well:

```emacs-lisp
(message
 (pcase major-mode
   ('org-mode "You are in Org")
   ('emacs-lisp-mode "You are in Emacs Lisp")
   (_ "You are somewhere else")))
```

Same idea for ~if~, ~when~, and the rest.

<!--
#+cindex: Domain-Specific Language (DSL)
-->

Back to the topic of what ~pcase~ does differently. If you read its documentation, you will realise that it has its own mini language, or "domain-specific language" (DSL). This is common for macros ([Evaluation inside of a macro or special form](#h:evaluation-inside-of-a-macro-or-special-form)). They define how evaluation is done and what sort of expressions are treated specially. Let me then gift you this toy function that illustrates some of the main features of the DSL now under consideration:

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

Go ahead and evaluate that function and then try it out ([Evaluate Emacs Lisp](#h:evaluate-emacs-lisp)). Below are a couple of examples:

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

Some of those clauses are a different way to express ~cond~. Arguably better, but not a clear winner in my opinion. What is impressive and a true paradigm shift is the concept of "destructuring", else the pattern matching done to the expression that effectively ~let~ binds elements of a list or cons cell to their corresponding index. The syntax used for this destructuring is arcane, until you relate it to the quasi-quote and the comma which are used for partial evaluation ([Partial evaluation inside of a list](#h:partial-evaluation-inside-of-a-list)). With this in mind, consider ~pcase-let~, ~pcase-let*~, ~pcase-lambda~, and ~pcase-dolist~, as variations of the plain ~let~, ~let*~, ~lambda~, and ~dolist~ with the added feature of supporting destructuring. They are not doing any of the extras of ~pcase~ though---just destructuring on top of their familiar behaviour! This is especially useful when you are working with the return value of a function which comes as a list. I will not elaborate at length, as this is an advanced use-case. If you are already at that level, you do not need me to tell you what to write. For the rest of us who, like me, typically work with simpler code, the ~pcase-let~ serves as a sufficient illustration of the principle:

```emacs-lisp
(defun my-split-string-at-space (string)
  "Split STRING at the space, returning a list of strings."
  (split-string string "\s"))

(pcase-let ((`(,one ,_ ,three) (my-split-string-at-space "Protesilaos of Cyprus")))
  (message "This is like `let', but we got `%s' and `%s' via destructuring" one three))
;; => "This is like ‘let’, but we got ‘Protesilaos’ and ‘Cyprus’ via destructuring"
```

Whether you use ~pcase~ and destructuring in general is up to you. You do not require them to write high quality code. Though you might agree with those who consider them inherently more elegant and opt to use them for this very reason to have code that is succinct yet highly expressive.

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

名前付き関数の中で`lambda`を定義し、`let`を使ってそれを繰り返し利用する、という使い方も考えられます。例えば、`mapc`を使って副作用としてリスト内の人々に挨拶する関数が必要で、その挨拶処理自体を何度も`defun`で定義したくない、といった場合です：

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

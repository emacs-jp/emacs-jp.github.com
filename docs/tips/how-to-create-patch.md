---
layout: page
author: [conao3]
title: "Emacsにパッチを送るには"
tags: [patch]
date: 2020-09-13
last_modified: 2020-09-13
org: t
---
{% include JB/setup %}

# 概要

筆者は最近初めてEmacsにパッチを送り、[無事マージ](https://emba.gnu.org/emacs/emacs/-/commit/66509f2ead423b814378a44a55c9f63dcb1e149b)されました。 日本人の方でコミット権を持つ方すらおられるのに、この情報については散逸しておりパッチを送ることは戸惑うことがたくさんありました。

この記事はその経験をまとめると共に、Emacs本体にパッチを送る際のtipsについてまとめるものです。


### 実際のパッチ

```diff
From 66509f2ead423b814378a44a55c9f63dcb1e149b Mon Sep 17 00:00:00 2001
From: Naoya Yamashita <conao3@gmail.com>
Date: Wed, 9 Sep 2020 09:52:39 +0900
Subject: [PATCH] Add gv-define-expander for plist-get

It is necessary to make plist-get as a generalized variable, and this
definition allows user to use setf and other useful functions on
plist-get.

* lisp/emacs-lisp/gv.el: Add gv-define-expander for plist-get

* lisp/emacs-lisp/gv-tests.el: Add new tests for plist-get
---
 lisp/emacs-lisp/gv.el            | 11 +++++++++
 test/lisp/emacs-lisp/gv-tests.el | 40 ++++++++++++++++++++++++++++++++
 2 files changed, 51 insertions(+)

diff --git a/lisp/emacs-lisp/gv.el b/lisp/emacs-lisp/gv.el
index 78d86b9fc3..5470b8532f 100644
--- a/lisp/emacs-lisp/gv.el
+++ b/lisp/emacs-lisp/gv.el
@@ -417,6 +417,17 @@ The return value is the last VAL in the list.
                                               `(delq ,p ,getter))))))
                             ,v))))))))))

+(gv-define-expander plist-get
+  (lambda (do plist prop)
+    (macroexp-let2 macroexp-copyable-p key prop
+      (gv-letplace (getter setter) plist
+        (macroexp-let2 nil p `(cdr (plist-member ,getter ,key))
+          (funcall do
+                   `(car ,p)
+                   (lambda (val)
+                     `(if ,p
+                          (setcar ,p ,val)
+                        ,(funcall setter `(cons ,key (cons ,val ,getter)))))))))))

 ;;; Some occasionally handy extensions.

diff --git a/test/lisp/emacs-lisp/gv-tests.el b/test/lisp/emacs-lisp/gv-tests.el
index 7a8402be07..10e3b531f3 100644
--- a/test/lisp/emacs-lisp/gv-tests.el
+++ b/test/lisp/emacs-lisp/gv-tests.el
@@ -156,6 +156,46 @@ its getter (Bug#41853)."
       (eval-buffer)))
   (should (equal (get 'gv-setter-edebug 'gv-setter-edebug-prop) '(123))))

+(ert-deftest gv-plist-get ()
+  (require 'cl-lib)
+
+  ;; Simple setf usage for plist-get.
+  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
+                   (setf (plist-get target :b) "modify")
+                   target)
+                 '(:a "a" :b "modify" :c "c")))
+
+  ;; Other function (cl-rotatef) usage for plist-get.
+  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
+                   (cl-rotatef (plist-get target :b) (plist-get target :c))
+                   target)
+                 '(:a "a" :b "c" :c "b")))
+
+  ;; Add new key value pair at top of list if setf for missing key.
+  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
+                   (setf (plist-get target :d) "modify")
+                   target)
+                 '(:d "modify" :a "a" :b "b" :c "c")))
+
+  ;; Rotate with missing value.
+  ;; The value corresponding to the missing key is assumed to be nil.
+  (should (equal (let ((target '(:a "a" :b "b" :c "c")))
+                   (cl-rotatef (plist-get target :b) (plist-get target :d))
+                   target)
+                 '(:d "b" :a "a" :b nil :c "c")))
+
+  ;; Simple setf usage for plist-get. (symbol plist)
+  (should (equal (let ((target '(a "a" b "b" c "c")))
+                   (setf (plist-get target 'b) "modify")
+                   target)
+                 '(a "a" b "modify" c "c")))
+
+  ;; Other function (cl-rotatef) usage for plist-get. (symbol plist)
+  (should (equal (let ((target '(a "a" b "b" c "c")))
+                   (cl-rotatef (plist-get target 'b) (plist-get target 'c))
+                   target)
+                 '(a "a" b "c" c "b"))))
+
 ;; `ert-deftest' messes up macroexpansion when the test file itself is
 ;; compiled (see Bug #24402).
```


# パッチを送る前に

Emacsマニュアルに記載があることについて、パッチを送る前に処理しておくとMLでのやりとりがスムーズになります。 メンテナさんの負担を減らすためにもこれらのことについては事前に準備しておきましょう。


## 著作権譲渡の手続き

-   参考
    -   en
        -   [Emacs manual - Copyright Assgnment](https://www.gnu.org/software/emacs/manual/html_node/emacs/Copyright-Assignment.html#Copyright-Assignment)
        -   [worg - How to contribute to Org?](https://orgmode.org/worg/org-contribute.html)
    -   ja
        -   [Emacs manual - 著作権の割り当て](https://ayatakesi.github.io/emacs/27.1/html/Copyright-Assignment.html)

FSFのプロジェクトにパッチを送る際には、FSFに著作権を譲渡する必要があります。 例外として累積パッチ行数が15行以下なら省略してよいという記載がありますが、パッチを送るのは一回きりにするという選択はあまりないと思いますので、やっておくと良いと思います。

Emacsマニュアルには具体的なプロセスについて記載がないのですが、親切な他のFSFプロジェクトを参照すると記載があります。例えば[org-mode](https://orgmode.org/worg/org-contribute.html)です。

org-modeはさらに親切にも[テンプレート](https://orgmode.org/request-assign-future.txt)を用意してくれています。 テキストファイル直リンクでふとしたときに消えそうなので、以下に全文を記載しておきます。

> Please email the following information to assign@gnu.org, and we will send you the assignment form for your past and future changes.
> 
> Please use your full legal name (in ASCII characters) as the subject line of the message.
> 
> ---
> 
> REQUEST: SEND FORM FOR PAST AND FUTURE CHANGES
> 
> [What is the name of the program or package you're contributing to?] Org-mode, which is part of Emacs
> 
> [Did you copy any files or text written by someone else in these changes? Even if that material is free software, we need to know about it.]
> 
> [Do you have an employer who might have a basis to claim to own your changes? Do you attend a school which might make such a claim?]
> 
> [For the copyright registration, what country are you a citizen of?]
> 
> [What year were you born?]
> 
> [Please write your email address here.]
> 
> [Please write your postal address here.]
> 
> [Which files have you changed so far, and which new files have you written so far?]

ちなみにこの著作権譲渡のプロセスは一回やっておけばFSFのどのプロジェクトにもパッチを送れるようになります。 このテンプレートを見つけたのがorg-modeのページなので、話を合わせるためにもorg-modeにパッチを送りたいんだということにしておくことで、やりとりがスムーズになる可能性があります。

このテンプレートの前に序文として以下のような内容を入れました。

> Hi! I'm Naoya Yamashita, aka, conao3.
> 
> I read I need to sign some paper from FSF to contribute GNU code. As a finding a template and mentioned Email address in org-mode which is part of Emacs, I now sent this Email.
> 
> If I need other work, please comment.

そして「REQUEST: Copyright Assignment - Naoya Yamashita」という題で4/19に assign@gnu.org に送りました。

先方から返信があったのは4/28でした。そこからサインを書いてスキャンして送りかえしたりと数回やりとりがあった後、5/13に手続きが完了しました。


### 「Do you have an employer&#x2026;」について

「Do you have an employer&#x2026;」に対する返答は重要です。 ここは「No」と答えておくことが無難です。実際私はこの手続きに一回挫折しており、一回目は「No. I'm just bachelor in Hiroshima University in Japan.」と書いたところ、大学の著作権管理部にサインをもらってこいと言われ難儀しました。(そもそも大学にこのような個人の活動を管理する部署はない。。)

このサインをもらうことはほぼ不可能で、FSFに大学はサインしたくないと言っているんだがと相談しても、いやそれは説明の仕方が悪いんだと栓ないやりとりを続けることになります。。

「No」と答えておけば自分のサインだけで十分ということになるので、スムーズに手続きを進めることができます。


### 「Which files have you changed so far&#x2026;」について

当時はパッチを書いていなかったので、「No. But I plan to contribute to GNU code.」と答えておきました。


### 生年について

手続きの最後に

> One more thing, may I get your year of birth for our records? The information is missing from the questionnaire.

と聞かれました。 こちらとしてはorg-modeの記載に従いましたよというスタンスなので記載してなくても問題ないのですが、よりスムーズに手続きしたいなら初回のメールに記載しておくと良いかもしれません。

私の場合はこのように聞かれたので、その返信で生年を答えたところ、「Thanks」と返答を受け、手続きは完了しました。


## upstreamのレポジトリ

-   参考
    -   en
        -   [Emacs manual - Contributing to Emacs Development](https://www.gnu.org/software/emacs/manual/html_node/emacs/Contributing.html#Contributing)
        -   [git.savannah.gnu.org - Emacs](https://git.savannah.gnu.org/cgit/emacs.git)
    -   ja
        -   [Emacs manual - Emacs開発への貢献](https://ayatakesi.github.io/emacs/27.1/html/Contributing.html)

Emacsのupstreamは[savannnah](https://git.savannah.gnu.org/cgit/emacs.git)にあります。 下記コマンドでcloneできます。

```sh
git clone https://git.savannah.gnu.org/git/emacs.git
```

patchがconflictしていてmasterに当てられないと、まずマージはしてもらえないので先端の変更に追随することは重要です。


## パッチを送るML

-   参考
    -   en
        -   [Emacs manual - Contributing to Emacs Development](https://www.gnu.org/software/emacs/manual/html_node/emacs/Contributing.html#Contributing)
        -   [Emacs manual - Checklist for Bug Reports](https://www.gnu.org/software/emacs/manual/html_node/emacs/Checklist.html#Checklist)
        -   [Emacs manual - Sending Patches for GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/emacs/Sending-Patches.html#Sending-Patches)
        -   [savannah.gnu.org - Emacs - Mailing Lists]()
        -   [bug-gnu-emacs](https://lists.gnu.org/mailman/listinfo/bug-gnu-emacs)
        -   [emacs-devel](https://lists.gnu.org/mailman/listinfo/emacs-devel)
    -   ja
        -   [Emacs manual - Emacs開発への貢献](https://ayatakesi.github.io/emacs/27.1/html/Contributing.html)
        -   [Emacs manual - バグレポートのためのチェックリスト](https://ayatakesi.github.io/emacs/27.1/html/Checklist.html)
        -   [Emacs manual - GNU Emacsへのパッチの送付]()

[bug-gnu-emacs](https://lists.gnu.org/mailman/listinfo/bug-gnu-emacs)か[emacs-devel](https://lists.gnu.org/mailman/listinfo/emacs-devel)のどちらかにパッチを送ることになります。

どちらが良いのか。。というのは状況によります。 マニュアルの記載を要約すると以下のようになります。

-   bug-gnu-emacs
    -   Emacsのバグを見つけたとき (`M-x report-emacs-bug`)
-   emacs-devel
    -   Emacsを改善する作業(相談)をしたいとき
    -   既にパッチを書き上げたとき

ということでパッチを送るのは普通はemacs-develになります。 アーカイブを見るとバグ報告と同時にパッチを送付する方もおり、その場合はbug-gnu-emacsに送ることになります。

-   Note
    
    [Emacs manual - Checklist for Bug Reports](https://www.gnu.org/software/emacs/manual/html_node/emacs/Checklist.html#Checklist) には、バグレポートに不要な要素としてパッチが挙げられています。
    
    しかし[Emacs manual - Sending Patches for GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/emacs/Sending-Patches.html#Sending-Patches)にはbug-gnu-emacsはパッチのトラッキングシステムがあるので、bug-gnu-emacsに送ってくれと書いてあります。。
    
    完成度の高いパッチならバグレポートに加えても良いということでしょうか。。


# コードの変更

cloneしたemacsディレクトリで作業します。 とりあえずmasterの状態でテストが通るか確認します。

まず普通にビルドします。installしないので `--prefix` 指定は必要ありません。

```sh
./autogen.sh
./configure
make
```

Emacsのテストはtestディレクトリ以下にあります。 testディレクトリのREADMEに説明がありますが、結局以下のコマンドを実行すれば、gvのテストを実行できます。

```sh
cd test
make lisp/emacs-lisp/gv-tests
```

ソースを変更した後はルートディレクトリに戻って `make` した後に `make lisp/emacs-lisp/gv-tests` ができます。面倒ですが仕方ない。。

`make` が必要ですが、よほど変なことが起こらない限り `make clean` は必要ありません。 `gv.el` に関連するファイルだけが再ビルドされるので再ビルドは高速です。

テストは[ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/index.html)で記述します。前後を見て追加すれば雰囲気で動くと思います。


# パッチの作成

-   参考
    -   en
        -   [Emacs manual - Coding Standards](https://www.gnu.org/software/emacs/manual/html_node/emacs/Coding-Standards.html#Coding-Standards)
        -   [Emacs manual - Checklist for Bug Reports](https://www.gnu.org/software/emacs/manual/html_node/emacs/Checklist.html#Checklist)
        -   [Emacs manual - Sending Patches for GNU Emacs](https://www.gnu.org/software/emacs/manual/html_node/emacs/Sending-Patches.html#Sending-Patches)
    -   ja
        -   [Emacs manual - コーディング規約](https://ayatakesi.github.io/emacs/27.1/html/Coding-Standards.html)
        -   [Emacs manual - バグレポートのためのチェックリスト](https://ayatakesi.github.io/emacs/27.1/html/Checklist.html)
        -   [Emacs manual - GNU Emacsへのパッチの送付]()

さてようやく、パッチを作成します。 コミットするまでは普通の作業と同じです。 コミットメッセージについては空気を読んでいい感じに記載します。

ちなみに[magit-patch-changelog](https://github.com/dickmao/magit-patch-changelog/tree/876c780bdb676b6ece64861704e199b94f33cf71)はEmacsの規約に沿ったテンプレートを作成してくれるパッケージです。これを使えば少しコミットメッセージを書くのが楽になるかもしれません。

コミットしたらMagitで `W c c` を押すことでHEADから1コミットのパッチが作成されます。特にメッセージ等は出ないので不安になりますが、プロジェクトルートに保存されています。

後はそのパッチを添付して適切なMLにメールを送るだけです。

なお、複数コミットをパッチにしたいときは 「[git 複数のコミットを反映する git format-patch apply](https://blog.bbtune.com/archives/2617/git-patch-format-apply)」などを参照してください。Magitでの操作は分かりません。。


# タイムテーブル

一例として今回のパッチがマージされるまでのやりとりをまとめておきます。

-   2020/08/17 emacs-develにパッチを投げた ([from conao3](https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00503.html))
-   2020/08/27 誰も返事をくれないので、本当にここで合ってるのか？とメール ([from conao3](https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00899.html))
    -   2020/08/28 君のメールは見えてるよと私にToでメール(MLには投稿されてないので他の人には見えない) (from drew)
    -   2020/08/28 ありがとうと返事 (from conao3)
-   2020/09/05 パッチのレビュー ([from stefan](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00080.html))
-   2020/09/09 レビューに対する返事、パッチアップデート ([from conao3](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00363.html))
-   2020/09/09 アップデートしたパッチ、少し間違えてるよと指摘 ([from stefan](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00368.html))
-   2020/09/09 指摘の箇所を修正、パッチアップデート ([from conao3](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00398.html))
-   2020/09/09 マージ!!! ([from stefan](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00450.html))
-   2020/09/10 ありがとうメール ([from conao3](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00463.html))
-   2020/09/10 これはとても良い変更なので、NEWSに入れたらどうだという提案 ([from adam](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00521.html))
-   To be continued&#x2026;

という感じでした。

別件のメールでやってしまった失敗ですが、Gmailでやり取りしていると「返信」ボタンで返信するとfromだけにtoで返信するので、ccで指定されているMLに投稿されないんですよね。。

今回のやりとりでもdrewさんと私が失敗していて、このメールはMLのアーカイブから見えません。。Gmailの設定で「全員に返信」をデフォルトにしておいた方が良いと思います。


# まとめ

人生初の大型OSSへのコントリビュートでした。

FSFプロジェクトへの貢献はMLでのコミュニケーションが中心なので、GitHubやGitLabでのコミュニケーションに慣れているとハードルは高いですが、一度乗り越えるとまたパッチを送ろうと思えるようになります。

この記事によってEmacsへのコントリビュートがどのように行なわれているのかが分かり、自分でもやってみようかなと思っていただければ嬉しいです。

<!--
This file is generated from org file.
Please edit that org source instead of this file.

;; Local Variables:
;; buffer-read-only: t
;; End:
-->

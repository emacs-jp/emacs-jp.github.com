---
title: Avyならなんでもできる
tags:
  - Emacs
  - emacs-lisp
private: true
updated_at: '2024-08-09T22:37:13+09:00'
id: 8379ec7322051bb40d67
organization_url_name: null
slide: false
ignorePublish: false
---

これはKarthik Chikmagalurさんによって記述された記事を日本語に翻訳した記事であり、記事の所有権と著作権はKarthik Chikmagalurさんに帰属します。

元の記事: [Avy can do anything | Karthinks](https://karthinks.com/software/avy-can-do-anything/)


<!-- # You're using Avy wrong. -->

# Avyの使い方が間違っている

<!--
Too harsh? Let me rephrase: you’re barely using Avy. Still too broad? Okay, the noninflammatory version: Avy, the Emacs package to jump around the screen, lends itself to efficient composable usage that’s obscured by default.
-->

厳しすぎるだろうか? 言い直そう。あなたはAvyのほとんどを使っていない。まだ言い過ぎだろうか? よろしい、では扇動的ではないバージョンで言い直そう: 画面上を飛び回るEmacsパッケージであるAvyは、効率的に組み立て構成可能な使い方(デフォルトでは隠されている)に適しているパッケージなのだ。

<!--
Without burying the lede any further, here's a demo that uses a single Avy command (avy-goto-char-timer) to do various things in multiple buffers and windows, all without manually moving the cursor:
-->

説明はこれ位にしておこう。以下のデモでは複数のバッファーやウィンドウにたいして、手作業でカーソルを移動することなく、Avyのコマンド1つ(avy-goto-char-timer)で様々な物事すべてを行う様子をお見せする:

[avy-all-demo.mp4](https://karthinks.com/img/avy-all-demo.mp4)

<!-- 
Copy text, kill lines or regions, move text around, mark text, bring up help buffers, look up definitions, search google, check my spelling, the list goes on. I emphasize this again: Avy defines dozens of jump commands, but I'm only using one. This post breaks this down in detail so you can create your own version of this, but more importantly tries to explain why this is a neat idea.
-->

テキストのコピー、行やリージョンのkill、テキストの移動、マーク、ヘルプバッファーの表示、定義の検索、Google検索、スペルチェックなど、まだまだリストは続く。もう一度強調しておこう: Avyによって定義されるジャンプコマンドは数十個あるが、わたしが使うコマンドは1つだけだ。この記事ではあなたが自分用のバージョンを作れるようにこのコマンドを詳細まで掘り下げていくが、もっと重要なのはこれがなぜいかしたアイデアなのかという理由を説明することにある。


<!--
This is the first of two parts in a series on , an Emacs package for jumping around efficiently with the keyboard. Part 1 is about about supercharging built-in customization to do anything with Avy, or some approximation thereof. Part 2 will be a more technical (elisp-y) dive into writing more complex features for your individual needs. If you are interested in the short elisp snippets in this document, they are collated into a single file here. 
-->

この記事はキーボードによって効果的にジャンプ移動するEmacsパッケージであるAvyに関するシリーズ2部作のうちのパート1となる。このパート1では **なんでもAvyで行う** ためにカスタマイズによるビルトイン機能の強化、あるいはそれに近いことに関して説明する。パート2では読者の個々のニーズに合わせてさらに複雑な機能を記述するために、より技術的(elispっぽい)な話題へと掘り下げていく予定だ。今回の記事に記載した短いスニペットに興味を感じたら、[ここ](https://gist.github.com/karthink/af013ffd77fe09e67360f040b57b4c7b#start-of-content)に1つのファイルとしてまとめてあるので参考にして欲しい。

<!-- # Filter → Select → Act -->

# フィルター -> 選択 -> アクション

<!--
We see the same pattern repeated in most interactions with Emacs whose primary purpose isn't typing text. To perform a task, we Filter, Select and Act in sequence:
-->

主要目的がテキストをタイプすることではないEmacsとのやり取りのほとんどにおいて、同じパターンが繰り返されるのを目にする。あるタスクを実行するためにフィルター(filter: 抽出)して、選択(select)して、それからアクション(act: 実行)を順繰りに行うパターンのことだ。

<!--
Filter: Winnow a large pile of candidates to a smaller number, usually by typing in
some text. These candidates can be anything, see below for examples.

Select: Specify a candidate as the one you need, usually with visual confirmation.
If you filter a list down to one candidate, it's automatically selected.

Act: Run the task with this candidate as an argument. 
-->

フィルター(Filter): 通常は何らかのテキストをタイプすることによって、大量に積まれたウィンドウ候補をより少ない候補へと絞り込む。以下に示す例のように候補は何でもよい。

選択(Select): 必要は候補いずれかを指定する(通常は視覚的な同意つき)。フィルターによって候補リストが1つの候補に絞り込まれた場合には自動的にそれを選択する。

アクション(Act): 引数としてその候補を渡してタスクを実行する。

<!-- 
* Want to open a file? Invoke a command, then type in text to filter a list of completions, select a completion, find-file.
* Switch buffers? Type to filter, select a buffer, switch. 
* Autocomplete a symbol in code? Type a few characters to narrow a pop-up list, choose a candidate, confirm. 
-->

* ファイルをオープンしたい? ならコマンドを呼び出してから補完リストをフィルターするテキストをタイプして補完結果から選択してからfind-fileだ。

* バッファーの切り替え? フィルターに何かをタイプ、バッファーを選択して切り替えるだけ。

* コード内のシンボルの自動補完はどうだろう? ポップアップリストを絞り込むための文字を数文字タイプして、候補を選択したら確認に同意すればいい。

<!-- 
This observation leads to several interesting ideas. For one, the Filter → Select → Act process is often thought of as a single operation. Decoupling the filtering, selection and action phases (in code and in our heads) frees us to consider a flock of possibilities. Here's minibuffer interaction: 
-->

例のごとく、これは単純化されたモデルだ。たとえばHelm、Ivy、Diredの仲間たちは処理する複数の候補をユーザーが選択できるようにしている。このアイデアを検討する間は、この知識は脇に避けておくことにしよう。この観察によっていくつかの興味深いアイデアが導かれる。たとえばフィルター->選択->アクションというプロセスが、1つの操作と考えられることが珍しくない。フィルタリング、選択、アクションのフェーズを(コードや脳内で)分離してみれば、多くの可能性を考慮する自由が得られるだろう。以下はミニバッファーにおけるやり取り:

![minibuffer-interaction-paradigm.png](https://karthinks.com/img/minibuffer-interaction-paradigm.png)

<!-- 
The possibilities are, respectively: different completion styles (matching by regexps, flex, orderless), different selection interfaces (Icomplete, Ivy, Helm, Vertico, Selectrum and more by the day) and different action dispatchers (Embark, Helm/Ivy actions)1. A similar mini-Cambrian explosion has happened in the space of shell utilities since fzf arrived on the scene. In Emacs the minibuffer is where we're used to selecting things, so it's natural to think of these examples.

1. Technically there’s also sorting the candidates, which I lump in with filtering in this breakdown.
-->

異なる補完スタイル(regexp、flex、あるいはorderlessによるマッチング)、異なる選択インターフェイス(Icomplete、Ivy、Helm、Vertico、Selectrumと日々増加中)、異なるアクションディスパッチャー(Embark、HelmやIvyのアクション)といった可能性が考えられる[^1]。[シェルユーティリティ分野](https://github.com/junegunn/fzf/wiki/)においても[fzf](https://github.com/junegunn/fzf)の登場以来、新たなユーティリティが次々と爆誕する様はさながらミニカンブリア爆発とでも形容したくなる勢いだ。Emacsにおけるミニバッファーとは、物事を選択する上でわたしたちが慣れ親しんだ場所なので、これらの例を考えるのは自然だろう。

<!-- 
But this usage pattern extends to more than just minibuffer interaction. We often do things in regular buffers that involve the Filter → Select → Act pattern. In fact, you can look at most interactions with Emacs (whose focus isn't typing text) this way: when you call a goto-definition command with point–the text cursor–in some text, the filtering stage is skipped, the selection is made for you by a function from the thing-at-pt library, and a preset action is called. How about a mouse interaction? scroll through a window full of icons, move the mouse cursor to an icon and click on it. These completely redundant examples prime us for the more interesting cases. Consider Isearch: 
-->

この使用パターンはミニバッファーとの対話にかぎらず拡張できる。通常のバッファー操作において、フィルター->選択->アクションというパターンで物事を行うことは珍しいことではない。Emacsとのほとんどの(テキストのタイプに重点を置かない)対話において、この使い方を実際に目にできるだろう。ポイント(テキストカーソルのこと)が何らかのテキストにあるときにgoto-definitionコマンドを呼び出せば、フィルタリングのステップは飛ばしてthing-at-ptライブラリーの関数が選択を行い事前にセットされたアクションを行うのだ。マウスによる対話ならどうなるだろう? アイコンだらけのウィンドウをスクロールしてかいくぐり、お目当てのアイコンにマウスカーソルを置いてクリックといった具合である。この完全に冗長な例から、より興味深いケースにたいする気づきが得られる。Isearchで考えてみよう:

![isearch-interaction-paradigm.png](https://karthinks.com/img/isearch-interaction-paradigm.png)

<!-- 
When you type (after C-s), you automatically filter buffer text and select the nearest match. You can select each subsequent match in turn, or jump to the first or last match. The Act here is the process of moving the cursor to the match location, but it can be one of a few things, like running occur or query-replace on the search string. Many Isearch commands simultaneously filter, select and act, so we're fitting a square peg in a round hole here2.
-->

<!-- 
Isearch combines filter/search/act into a single command for efficiency. Even so,  it's a very cleverly designed library for Emacs with support for many actions.  Folks who replace Isearch entirely with Swiper or similar commands are missing  out.
-->

(C-sの後に)何かタイプすると、バッファーのテキストが自動的にフィルターされてもっとも近くにあるマッチが選択される。その後に続くマッチを順に選択したり、最初や最後のマッチにジャンプすることも可能だ。ここではマッチした位置へのカーソル移動というプロセスがアクションに相当するが、これは検索文字列にたいするoccurやquery-replaceの実行のような処理のうちの1つとみなすことができる。Isearchには多くはフィルターと選択、アクションを同時に行うコマンドが多く、そのためにユーザーは丸い穴に無理して四角い釘を打つような破目となる[^2]。

<!-- 
If you've spent any time using Isearch, you can appreciate the tradeoff involved in dividing a task into these three independently configurable phases. Lumping two or all three into a single operation makes Isearch a wicked fast keyboard interaction. When I use Isearch my brain is usually trying to catch up to my fingers. On the other hand, the advantage of modularity is expressive power. The three phase process is slower on the whole, but we can do a whole lot more by plugging in different pieces into each of the Filter , Select and Act slots. To see this, you have only to consider how many disparate tasks the minibuffer handles, and in how many different ways!
-->

Isearchの使用に少しでも時間を費やした経験があればIsearchの単一のタスクを、独自に構成可能な3つのフェーズに分割することで生じるトレードオフが理解できるはずだ。2つ、あるいは3つすべてを単一操作にまとめることが、Isearchにおけるキーボード操作を *邪悪なまでに高速化* する。わたしがIsearchを使用する際には、頭が指を追いかける場面が珍しくない。他方モジュール化がもつ優位な点は、その表現力にある。3つのフェーズからなるプロセスは全体として遅くはなるもののフィルター、選択、アクションそれぞれのスロットに異なるパーツを繋げることによって、さらに多くのことを行うことが可能になる。まったく異なるタスクにたいして、ミニバッファーがさまざま方法を用いていかにして処理をこなすか思い浮かべてみればわかるだろう!

<!-- 
But back to Isearch: what can we do to decouple the three stages here? Not much without modifying its guts. It's all elisp, so that's not a tall order. For example, Protesilaos Stavrou adds many intuitive actions (such as marking candidates) to Isearch in this video. But it turns out we don't need to modify Isearch, because Avy exists, has a killer Filter feature, and it separates the three stages like a champ. This makes for some very intriguing possibilities. 
-->

話しをIsearchに戻す。どうやれば3つのステージに分割できるのか? 根性が持続できれば大したことはないだろう。すべてがelispである以上、それは難しい注文ではないのだ。たとえばProtesilaos Stavrouは[ビデオ](https://www.youtube.com/watch?v=y6_bmcd3nis)において、候補のマーキングといった直観的な多くのアクションをIsearchに追加している。しかし **Avy** の存在によってIsearchの修整が不要だということが明らかになった。フィルターというキラー機能を有しつつも、王者同様3つのステージを分離しているのがAvyである。Avyによって何とも興味がそそられる可能性が生みだされた。

![avy-interaction-paradigm.png](https://karthinks.com/img/avy-interaction-paradigm.png)

<!--
Wait, what's Avy
? -->

# 待った、Avyとは?

<!--
Avy is authored by the prolific abo-abo (Oleh Krehel), who also wrote Ivy, Counsel, Ace-Window, Hydra, Swiper and many other mainstays of the Emacs package ecosystem that you've probably used. If you're reading this, chances are you already know (and probably use) Avy. So here's a very short version from the documentation:
-->

[Avy](https://github.com/abo-abo/avy)は[abo-abo](https://oremacs.com/)の作者でもあるOleh Krehelによって記述された。彼は実に多産な作者でありIvy、Counsel、Ace-Window、Hydra、Swiperを記述したのは彼であり、他にもおそらく読者も使ったことがあるEmacsのパッケージエコシステムの主要コンポーネントの多くも彼が記述した。この記事を読んでいる読者なら、すでにAvyを知っている(そして多分使ったことがある)かもしれない。したがってAvyのドキュメントから非常に短いバージョンを紹介しよう:

<!-- 
*avy is a GNU Emacs package for jumping to visible text using a char-based decision tree.* 
-->

> *Avyとは文字ベースのデシジョンツリーを用いて可視なテキストにジャンプするためのGNU Emacs向けパッケージです。*

<!-- 
You can call an Avy command and type in some text. Any match for this text on the frame (or all Emacs frames if desired) becomes a selection candidate, with some hint characters overlaid on top. Here I type in  "an" and all strings in the frame that match it are highlighted:
-->

Avyコマンドを呼び出して何かテキストをタイプしてみよう。フレーム(必要に応じてすべてのEmacsフレーム)上にあるテキストへのマッチすべてが選択のための候補となり、ヒント文字が上に重ねて表示される。ここ(訳注: このパラグラフの原文)で"an"と入力すると、これにマッチするフレーム上の文字列すべてがハイライト表示される:

![avy-basic-demo.png](https://karthinks.com/img/avy-basic-demo.png)

<!-- 
Typing in one of the hints then jumps the cursor to that location. Here I jump to this sentence from another window:
-->

ヒントのいずれかをタイプすれば、カーソルがその位置にジャンプする。以下のビデオでは別のウィンドウからこのセンテンスにジャンプしている:

[avy-basic-demo.mp4](https://karthinks.com/img/avy-basic-demo.mp4)

<!--
Typical  Avy usage looks something like this: Jump to a location that matches your text (across all windows), then jump back with pop-global-mark (C-x C-SPC). In a later section I go into more detail on jumping back and forth with Avy. Here is a demo of this process where I jump twice with Avy and then jump back in sequence:
-->

テキストにマッチする位置に(ウィンドウ間を横断して)ジャンプ、その後はpop-global-mark (C-x C-SPC)で元の位置にジャンプして戻る、というのがAvyの典型的な使い方だろう。後半のセクションではじAvyを使った前方あるいは後方へのジャンプについて詳解する。以下のデモではAvyで2回ジャンプしてから順番にジャンプして戻るプロセスを行っている:

[avy-jump-back-demo-2.mp4](https://karthinks.com/img/avy-jump-back-demo-2.mp4)

<!-- 
At  least that's the official description. You can peruse the README for more information, but what I find mystifying is that…
-->

少なくとも、これが公式の説明だ。詳細については[README](https://github.com/abo-abo/avy/blob/master/README.md)を参照してほしいが、わたしが疑問に思うのは...

<!-- 
# …Avy's documentation leaves out the best part
-->

# ... Avyのドキュメントにはもっとも肝心な部分が記載されていない

<!-- 
Avy handles filtering automatically and the selection is made through a char-based decision tree. Here's how it fits into our three part interaction model.
-->

Avy自動的にフィルタリングを行い、文字ベースのデシジョンツリーを通じて選択を行う。これを3つのパートからなるわたしたちの対話モデルにどのように適合させるかを示そう。

## フィルター

<!-- 
Before you call Avy every text character on your screen is a potential candidate for selection. The possibilites are all laid out for you, and there are too many of them!
-->

あなたがAvyを呼び出すまでは、 *スクリーン上にあるすべてのテキスト文字列* が選択のための候補である。どれを選ぶかはすべてあなたに委ねられており、選べる選択肢が多すぎるくらいだ!

<!-- 
You filter the candidate pool with Avy similar to how you would in the minibuffer, by typing text. This reduces the size of the pool to those that match your input. Avy provides dozens of filtering styles. It can: only consider candidates above/below point, only beginnings of words, only the current window (or frame), only whitespace, only beginnings of lines, only headings in Org files, the list goes on.
-->

プールされた大量の候補からフィルタリングを行うのは、ミニバッファーでテキストをタイプすることによってフィルタリングを行う方法と似ている。これによりプールのサイズは、入力にマッチするサイズへと絞り込まれる。Avyは数十個のフィルタリングスタイルを提供している。使用できるスタイルはポイントより上または下の候補だけを考慮するスタイル、単語の先頭、カレントのウィンドウ(またはフレーム)、空白、行頭、あるいはOrgファイルのヘッダーだけを考慮するスタイルといった具合にリストはまだまだ続いていく。

<!-- + Filtering commands in Avy -->
<!-- | Unit: Char            | Unit: Word or Symbol          | Unit: Line or other           | -->
<!-- |-----------------------+-------------------------------+-------------------------------+ -->
<!-- | avy-goto-char-timer   | avy-goto-word-0               | avy-goto-line                 | -->
<!-- | avy-goto-char         | avy-goto-subword-0            | avy-goto-end-of-line          | -->
<!-- | avy-goto-char-2       | avy-goto-symbol-1-above       | avy-goto-whitespace-end       | -->
<!-- | avy-goto-char-in-line | avy-goto-word-0-below         | avy-org-goto-heading-timer    | -->
<!-- | avy-goto-char-2-below | avy-goto-word-1-below         | avy-goto-whitespace-end-above | -->
<!-- | avy-goto-char-2-above | avy-goto-symbol-1-below       | avy-goto-line-above           | -->
<!-- | avy-goto-word-1-above | avy-goto-whitespace-end-below |                               | -->
<!-- |                       | avy-goto-symbol-1             | avy-goto-line-below           | -->
<!-- |                       | avy-goto-word-or-subword-1    |                               | -->
<!-- |                       | avy-goto-subword-1            |                               | -->
<!-- |                       | avy-goto-word-1               |                               | -->
<!-- |                       | avy-goto-word-0-above         |                               | -->

+ Avyのフィルタリングコマンド

| 単位: 文字            | 単位: 単語/シンボル           | 単位: 行/その他               |
|-----------------------|-------------------------------|-------------------------------|
| avy-goto-char-timer   | avy-goto-word-0               | avy-goto-line                 |
| avy-goto-char         | avy-goto-subword-0            | avy-goto-end-of-line          |
| avy-goto-char-2       | avy-goto-symbol-1-above       | avy-goto-whitespace-end       |
| avy-goto-char-in-line | avy-goto-word-0-below         | avy-org-goto-heading-timer    |
| avy-goto-char-2-below | avy-goto-word-1-below         | avy-goto-whitespace-end-above |
| avy-goto-char-2-above | avy-goto-symbol-1-below       | avy-goto-line-above           |
| avy-goto-word-1-above | avy-goto-whitespace-end-below |                               |
|                       | avy-goto-symbol-1             | avy-goto-line-below           |
|                       | avy-goto-word-or-subword-1    |                               |
|                       | avy-goto-subword-1            |                               |
|                       | avy-goto-word-1               |                               |
|                       | avy-goto-word-0-above         |                               |

<!-- This is a crazy list to keep track of. -->

これは追い続けるのが厳しい *気狂いじみた* リスト だ。

<!--
Filtering in Avy is independent of the selection method (as it should be), but there is a dizzying collection of filtering methods. I assume the idea is that the user will pick a couple of commands that they need most often and commit only those to memory. 
-->

(当然ではあるが)Avyのフィルタリングは選択メソッドからは独立しているが、フィルタリングには軽い目眩を覚える程の量のコレクションが存在する。これはユーザーはもっとも頻繁に使うコマンドをいくつか選んで、選んだコマンドだけを記憶に刻むという仮定がもたらしたアイデアではないかとわたしは考えている。

<!-- 
Here's the problem: **We want to use our mental bandwidth for the problem we're trying to solve with the text editor, not the editor itself.** Conscious decision-making is expensive and distracting. As of now we need to decide on the fly between Isearch and Avy to find and act on things. If you use a fancy search tool like Swiper, Helm-swoop or Consult-line, you now have three options. Having a bunch of Avy commands on top is a recipe for mental gridlock. To that end, we just pick the most adaptable, flexible and general-purpose Avy command (avy-goto-char-timer) for everything.
-->

ここで問題が生じる: **わたしたちはエディター自体にではなく、テキストエディターで解決しようとしている問題にたいして精神的な帯域幅を使用したい** のだ。無意識に行えない意思決定にはコストがかかり、集中力も乱されてしまう。現段階では物事を見つけて処理を行うためには、IsearchとAvyの中間点の何かを使ってその場で決定する必要がある。Swiper、Helm-swoop、Consult-lineといった多機能な検索ツールを使っている場合には、選べるオプションは3つだ。大量のAvyコマンドをトップに据えると、それが精神的な行き詰まりへと導くための処方箋になりかねない。すべての面においてもっとも適応力があり、柔軟かつ汎用的なAvyコマンド(avy-goto-char-timer)を選択するだけにしておけば、これを回避できるだろう。

```elisp
(global-set-key (kbd "M-j") 'avy-goto-char-timer)
```

<!-- 
Further below I make the case that you don't need to make even this decision, you can always use Isearch and switch to Avy as needed. 
-->

さらに以下では、常にIsearchを使用して必要に応じてAvyに切り替えることにより、この決定さえ不要だというわたしの考えを述べる。

<!-- 
To be clear, this decision cost has to be balanced against the cost of frequent busywork and chronic context switching that Avy helps avoid. There is a case to be made for adapting Avy's flexible filtering options to our needs, and the number of packages that offer Avy-based candidate filtering (for everything from linting errors to buffer selection) attests to this. We will examine this in depth in Part II. 
-->

明確にしておこう。それに忙殺されているように見えるもののAvyを使えば回避できる、慢性的かつ頻繁なコンテキスト切り替えによるコストが存在する。そのコストに見合ったコストであることが決定コストには要求されるものとする。ニーズにたいしてAvyの柔軟なフィルタリングオプションの調整を要するケースは存在する。Avyベースの候補(lintエラーからバッファー選択に至るすべて)にたいしてフィルタリングを提供する[数々のパッケージ](https://melpa.org/#/?q=avy)の存在がその証拠だ。これについてはパート2で詳しく検討しよう。

<!-- 
But in this piece we are interested in a different, much less explored aspect of Avy.
-->

とは言ったものの興味があるのは、まだ探求が済んでいないAvyの別の側面だ。以下の主張ではそれについても目を向けていこう。

## 選択

<!-- 
Every selection method that Avy offers involves typing characters that map to screen locations. This is quite different from Isearch, where you call isearch-repeat-forward (C-s, possibly with a numerical prefix argument) or the minibuffer, where you navigate a  completions buffer or list with C-n and C-p. Avy's selection method is generally faster because it minimizes, by design, the length of the character sequences it uses, and we have ten fingers that can access ∼40 keys in O(1) time. The fact that we're often looking directly where we mean to jump means we don't need to parse an entire screen of gibberish. Unfortunately for this article, this means using Avy is much more intuitive than looking at screenshots or watching demos.
-->

Avyが提供する選択メソッドには、すべてスクリーン上の位置にマッピングされた文字のタイプが含まれている。これはIsearchとは大きく異なる。isearch-repeat-forward (C-s、数プレフィックス引数が指定されているかもしれない)を呼び出すIsearch、補完バッファー(completions buffer)のナビゲーションにC-nやC-pを用いるミニバッファーとはまったく異なるのだ。Avyの選択メソッドは使用する文字シーケンスが最小となるようにデザインされており、わたしたちには40のキーに計算量オーダーO(1)でアクセス可能な10本の指が備わっているので、より高速であることが一般的だ。ジャンプしようとしている位置を直接見ていることが多いという事実は、ちんぷんかんぷんなスクリーン全体の解析が不要なことを意味している。この記事にとっては不幸ではあるが、スクリーンショットやデモを見るよりも、Avyを使ってみるほうが遥かに直観的であることをも意味している。

<!-- 
This  excellent design leaves us with little reason to tinker with the selection phase: it's sufficiently modular and accommodating of different filter and act stages. You can customize avy-style if you want to change the set or positions of characters used for selection. Here is an example of using simple words to select candidates:
-->

この洗練されたデザインによって、選択フェーズに手を加える理由はほとんどなくなった。選択フェーズは必要十分にモジュール化されているので、他のフィルターやアクションのステージに融通できる。選択に用いる文字集合や文字位置を変更したければ、Avyのスタイルをカスタマイズできる。候補選択に簡単な単語を用いる例を示す。

![avy-words-style-demo.png](https://karthinks.com/img/avy-words-style-demo.png)

<!-- 
We will pay more attention to the selection operation in part II as well.
-->

選択操作については第2部でも注目していく予定。

## アクション!

<!-- 
This brings us to the focus of this article. The stated purpose of Avy, jumping to things, makes it sound like a (contextually) faster Isearch. But jumping is only one of many possibilities. Avy provides a "dispatch list", a collection of actions you can perform on a candidate, and they are all treated on equal footing with the jump action. You can show these commands any time you use Avy with ?:
-->

という訳で、この記事の焦点をアクションに定めよう。Avyがゴールとして掲げているのは何かにジャンプすることである。これは(文脈的には)より高速化されたIsearchのように聞こえる。しかしジャンプすることは、Avyのもつ可能性のうちの1つに過ぎない。Avyは"ディスパッチリスト(dispatch list)"を提供する。これは候補にたいして実行可能なアクションのコレクションであり、すべてのアクションはジャンプアクションと同等に扱われる。これらのコマンドは、Avy使用中なら何時でも?:で表示できる。

![avy-dispatch-demo.png](https://karthinks.com/img/avy-dispatch-demo.png)

<!-- 
This means we are free to leverage Avy’s unique filtering and selection method to whatever action we want to carry out at any location on the screen. Our interaction model now ends in a block that looks something like this: 
-->

これはスクリーン上の任意の場所で *実行したいアクションが何であれ* 、すべてのアクションがAvy独自のフィルタリングおよび選択のメソッドを自由に使用できることを意味する。わたしたちの対話モデルの終わり方は以下のようなブロックとなるだろう。

![avy-interaction-paradigm-detailed.png](https://karthinks.com/img/avy-interaction-paradigm-detailed.png)

<!-- 
Additionally, Avy also defines a few commands that run different actions, like copying text from anywhere on screen:
-->

さらにAvyには、スクリーン上の何処かからテキストをコピーするのように、他のアクションを実行するいくつかのコマンドも定義されている:

| Kill                          | Copy            | Move                          |
|-------------------------------|-----------------|-------------------------------|
| avy-kill-ring-save-whole-line | avy-copy-line   | avy-move-line                 |
| avy-kill-ring-save-region     | avy-copy-region | avy-move-region               |
| avy-kill-region               |                 | avy-transpose-lines-in-region |
| avy-kill-whole-line           |                 | avy-org-refile-as-child       |


<!-- 
The problem with this approach is that it doesn't scale. Each of these commands defines a full Filter → Select → Act process, and we quickly run out of headspace and keyboard room if we want any kind of versatility or coverage. They're also not dynamic enough: you're locked into the pipeline and cannot change your mind once you start the command.
-->

このアプローチは拡張性において問題がある。
これらはそれぞれフィルター -> 選択 -> アクションという完全なプロセスを定義するコマンドなので、多様性や対応範囲で何らかの必要が生じたとしても、拡張の余地やキーボードの空きはすぐに枯渇してしまうだろう。動的な面においても不十分だ。一旦コマンドを開始してから気が変わっても、コマンド間をつなぐパイプラインが固定されているので変更できないのだ。

<!--
Folks love Vim's editing model for a reason: it's a mini-language where knowing M actions (verbs) and N cursor motions gives you M × N composite operations. This (M + N) → (M × N) ratio pays off quadratically with the effort you put into learning verbs and motions in Vim. Easymotion, which is Vim's version of Avy3[^3], has part of this composability built-in as a result. We seek to bring some of this to Avy, and (because this is Emacs) do a lot more than combining motions with verbs. We won't need to step into Avy's source code for this, it has all the hooks we need already.

[^3]: Or perhaps Avy is Emacs’ version of Easymotion
-->

Vimの編集モデルが愛されるのには理由がある。M個のアクション(動詞)とN個のカーソル移動を知ることによって、M×N個の合成操作を得られるミニ言語であることがその理由だ。(M+N)の知識から(M×N)の結果が得られるという比率関係により、Vimで動詞や移動の習得に努力を費やせば、その見返りとして得られる報酬が二次関数的に増加するからだ。この理由によりAvyのVimバージョンに相当するEasymotion[^3]には、合成機能の一部が組み込まれている。この機能を部分的にAvyに導入する方法を見つけられれば、動詞と移動の組み合わせよりも多くのことを行える筈なのだ(こっちはEmacs なのだから)。これを達成するためにAvyのソースコードに分け入る必要はない。すでに必要なフックはすべて揃っている。

# Avyのアクション

<!--
The basic usage for running an arbitrary action with Avy is as follows:

Call an Avy command. Any command will do, I stick to avy-goto-char-timer.
Filter: Type in some text to shrink the candidate pool from the entire screen to a few locations.
Act: Specify the action you want to run. You can pull up the dispatch help with ?, although you won't have to if you set it up right, see Remembering to Avy.
Select: Pick one of the candidates to run the action on.
-->

Avyにおいて任意のアクションを実行する際の基本的な使い方は以下のとおり:

1. Avyのコマンドを呼び出す。どのコマンドでもよいが、わたしはavy-goto-char-timerの一択。

2. フィルター: スクリーン全体からいくつかの位置に候補を絞り込むためのテキストを何かタイプする。

3. 選択: 実行したいアクションを指定する。?でディスパッチのヘルプを表示できる。正しくセットアップしてあれば不要だが、詳細が知りたければ"Avyにたいする心構え"を参照のこと。

4. アクション: アクションを実行したい候補を1つ選ぶ。

<!-- 
Here are some things I frequently do with Avy. Note that you can do this on any text in your frame, not just the active window! 
-->

以下はわたしがAvyで頻繁に行うことの一部。アクティブなウィンドウにかぎらず、フレーム内のすべてのテキスト行えることに注目!

<!-- 
First, taking the annoyance out of some common editing actions with Avy. If you use Vim and Easymotion, you get the first few actions below for free:
-->

まずはAvyを用いて一般的な編集アクションを行う際の悩みの種を解消する。VimとEasymotionを使っていれば、以下のアクションのうち最初のいくつかは無料で入手できる:

<details><summary>以下のデモに関する注意</summary>
<div>

<!--
For clarity, I set Avy to desaturate the screen and to "pulse" the line during a few of these actions. These are not enabled by default. I also slowed down the operations by adding a delay to make them easier to follow. In actual usage these are instantaneous.

The keys Avy uses to dispatch actions on candidates are specified in avy-dispatch-alist.

We will also need to ensure that these keys don't coincide with the ones Avy uses as selection hints on screen. Consider customizing avy-keys for this.
-->

- わかりやすくするために、いくつのアクションの間はスクリーン彩度を下げて、行が"点滅"するようにAvyをセットアップしてある。これらはデフォルトでは有効になっていない。追いやすくするために、遅延を追加して操作の低速化も行った。実際の使用では瞬時に実行される。

- Avyで候補にアクションをディスパッチするために使用するキーは、avy-dispatch-alistで指定する。

- 使用するキーがAvyがスクリーン上の選択ヒントにAvyが用いるキーと重複しないことも保証する必要があるだろう。これを行うためにはavy-keysのカスタマイズも検討してほしい。

</div>
</details>

## 単語、sexp、行の候補のkill

<!-- 
Killing words or s-expressions is built-in. I added an action to kill a line. In this demo I quickly squash some typos and delete a comment, then remove some code in a different window:
-->

単語とsexpのkillについてはビルトイン。行のkillはわたしが追加した。このデモでは手っ取り早くtypo修整とコメント削除を行い、その後に別ウィンドウのコードを削除している:

[avy-kill-demo.mp4](https://karthinks.com/img/avy-kill-demo.mp4)

<details><summary>ビデオ実況</summary><div>

1. avy-goto-char-timerを呼び出す。

2. "is"をタイプする。"is"にマッチするすべての候補にヒントが表示される。

3. kでavy-action-killを呼び出す。

4. 重複した"is"のいずれかを選択して削除する。

5. 余分な"and"にたいして1-4のステップを繰り返す。

6. avy-goto-char-timerを呼び出す。

7. "keyへのマッチに絞り込むために"key"とタイプする。

8. kでavy-action-kill-whole-lineを呼び出す。

9. コメント行を選択してその行を削除する。

10. 今度は別ウィンドウにある"("を選択して1-4のステップを繰り返して、関数の定義をkillする。

11. 今度は別ウィンドウにある"add"を選択して7-9のステップを繰り返して"(advice-add …)"がある行をkillする。

</div></details>

```elisp
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
```

## 単語、sexp、行の候補のyank

<!-- 
Copy to the kill-ring or copy to point in your buffer. In this demo I copy some text from a man page into my file:
-->

killリングまたはバッファーのポイント位置にコピーする。このデモではmanページからファイルにテキストをコピーしている。

[avy-copy-demo.mp4](https://karthinks.com/img/avy-copy-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer.
Type in "[". This filters to all matches for "[" in the frame.
Call avy-action-yank, bound to y.
Select the match corresponding to "[big-cache]". This text is copied to the buffer from the other window.
Call avy-goto-char-timer.
Type in "de". This filters to matches that include "demuxer".
Call avy-action-yank-whole-line, bound to Y.
Select one of the matches. The line is copied to the buffer.
Fix indentation with just-one-space, bound to M-SPC by default.
-->

1. avy-goto-char-timerを呼び出す。

2. "["をタイプしてフレーム内の"["へのマッチすべてをフィルタリングする。

3. yにバインドされているavy-action-yankを呼び出す。

4. "[big-cache]"にたいするマッチを選択する。これは別ウィンドウからこのバッファーにコピーされたテキスト。

5. avy-goto-char-timerを呼び出す。

6. "de"をタイプして"demuxer"を含んだマッチを選択する。

7. Yにバインドされているavy-action-yank-whole-lineを呼び出す。

8. マッチからいずれか1つ選択する。その行がバッファーにコピーされる。

9. デフォルトでM-SPCにバインドされているjust-one-spaceでインデントを修整する。

</div></details>

```elisp
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
```

<!-- 
Note that Avy actually defines separate commands for this: avy-copy-line and avy-copy-region to copy lines and regions from anywhere in the frame. These are a little faster since they have the action stage baked into the function call. You might be better served by these. But we want to avoid the mental burden of remembering too many top level commands, so we work in two simpler stages: call avy-goto-char-timer (to filter and select) and then dispatch on our selected candidate as we see fit.
-->

フレームのどこかから行やリージョンをコピーするために、実際にはavy-copy-lineおよびavy-copy-regionというコマンドをAvyが個別に定義している。これらのコマンドの方が関数呼び出しにアクションステージが織り込み済みなので僅かに速いので、そちらを使うほうがよいかもしれない。しかし多すぎるトップレベルコマンドを記憶することによる精神的な燃え尽き症候群をわたしたちは避けたいので、avy-goto-char-timerを呼び出して(フィルターと選択)、その後にお目当ての候補を選択してディスパッチを行うという、よりシンプルな2つのステージで作業を行っている。

## 単語、sexp、行の候補の移動

これはAvyでは"teleport"、わたしは"transpose"と呼んでいるが、いずれにせよtにバインドされることになる。このデモではバッファーのあちこちをあまり移動せずに、バッファーのあちこちにテキストを移動する。

[avy-transpose-demo.mp4](https://karthinks.com/img/avy-transpose-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Make some room, type in a space.
Call avy-goto-char-timer.
Filter to candidates that begin with "(".
Press t to run avy-action-teleport
Select the candidate that says "(parametric forcing)". It is moved over (transposed) to where the point is.
Jump to where it says "DOWNLOADED" in the window with avy-goto-char-timer. This is the only match for the input "down", so Avy jumps there automatically. You could also just isearch-backwards here.
Call avy-goto-char-timer.
Filter to candidates matching "the".
Press T to run =avy-action-teleport-line~.
Select a candidate line (the one just below the image). It is moved over (transposed) to where the point is.
-->

1. 場所を空けるためにスペースを何個かタイプする。

2. avy-goto-char-timerを呼び出す。

3. "("で始まる候補にフィルタリングする。

4. tを押下してavy-action-teleportを実行する。

5. "(parametric forcing)"という候補を選択する。

6. avy-goto-char-timerでそのウィンドウ内にある"DOWNLOADED"という候補にジャンプする。ポイントがある位置に移動(またはtranspoae: 転地)するはず。

7. avy-goto-char-timerを呼び出す。

8. "the"にマッチする候補にフィルタリングする。

9. Tを押下してavy-action-teleport-lineを実行する。

10. 候補の行(イメージの直下にある)を選択する。ポイントがある位置に移動(またはtranspoae: 転地)するはず。

</div></details>

```elisp
(defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)
```

## 候補の位置までzap

これはビルトインのコマンドで、デフォルトではzにバインドされている。

[avy-zap-demo.mp4](https://karthinks.com/img/avy-zap-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer
Type in "in". This shows hints for all matches with "in", including "In Emacs…".
Press z to run avy-action-zap.
Select a candidate char, in this case "In Emacs…". The text between point and the candidate is killed.
-->

1. avy-goto-char-timerを呼び出す。

2. "in"とタイプする。"In Emacs "を含めて、"in"へのすべてのマッチにヒントが表示される。

3. avy-action-zapを実行するためにzを押下する。

4. 候補(この場合だと"In Emacs"にたいするヒントの文字を選択する。ポイントと候補の間にあったテキストがkillされるだろう。

</div></details>

## 単語やsexpの候補をマーク

<!--
Also built in, m by default. This isn’t different from jumping to the candidate using Avy and calling mark-sexp, but it is more convenient:
-->

これもビルトインのコマンドであり、デフォルトではmにバインドされている。Avyを使って候補にジャンプするのは、 mark-sexpを呼び出すジャンプと変わらないが、Avyの方がより使いやすくなっている:

[avy-mark-demo.mp4](https://karthinks.com/img/avy-mark-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer.
Type in text to filter with, in this case just "(".
Press m to run avy-action-mark.
Select a candidate word or sexp, in this case ("~/.local/share").
Repeat steps 1 to 4 twice to mark other candidates: (data_directory... and RotatingFileHandler
-->

1. avy-goto-char-timerを呼び出す。

2. フィルタリングするためのテキスト(ここでは"(")をタイプする。

3. avy-action-markを実行するためにmを押下する。

4. 候補から単語かsexp(ここでは("~/.local/share"))を選択する。

5. 他の候補(data_directory...とRotatingFileHandler)をマークするために、1-4のステップを2回繰り返す。

</div></details>

## ポイントから候補までのリージョンをマーク

<!-- 
Avy sets the mark before it jumps, so you could use C-x C-x to activate the region, but this saves you the trouble. 
-->

Avyジャンプする前にマークをセットするので、C-x C-xを使用してリージョンをアクティブにすることも可能だが、このコマンドを使うほうがトラブルを回避できるだろう。

[avy-mark-to-char-demo.mp4](https://karthinks.com/img/avy-mark-to-char-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer.
Type in text to filter with, in this case "’)".
Press SPC to run avy-action-mark-to-char.
Select a candidate char. This marks the region from point to the char and moves the point.
Call avy-goto-char-timer.
Type in text to filter with, in this case just a series of spaces.
Press SPC to run avy-action-mark-to-char.
Choose a candidate (series of spaces) that begins a line. This marks the region from point to the line.
-->

1. avy-goto-char-timerを呼び出す。

2. フィルタリングに使用するテキスト(ここでは"')")をタイプする。

3. avy-action-mark-to-charを実行するために、SPCを押下する。

4. 候補のヒント文字を選択する。これによりポイントからヒント文字までのリージョンをマークして、ポイントを移動する。

5. avy-goto-char-timerを呼び出す。

6. フィルタリングに使用するテキスト(ここでは単に一連のスペース)をタイプする。

7. avy-action-mark-to-charを実行するために、SPCを押下する。

8. 行の開始となる候補(ここでは一連のスペース)を選択する。これによりポイントからその行までがマークされる。

</div></details>

```elisp
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)
```

<!--
Next, some contextual actions automagicked by Avy: 
-->

Avyにより自動的に実行される、コンテキストに応じたアクションをいくつか示そう:

## 候補の単語にispell

<!--
This is built-in, bound to i by default.
-->

これはビルトインのコマンドであり、デフォルトではiにバインドされている。

[avy-ispell.mp4](https://karthinks.com/img/avy-ispell.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer (or any other Avy jump command)
Type in "can", this highlights matches for "candidate" (and its misspellings)
Press the dispatch key for avy-action-ispell, set to i by default.
Select one of the matches, in this case the misspelled "canddidate" match.
This runs ispell-word on the selection.
Pick the correct spelling.
Call avy-goto-char-timer again.
Type in "te", this highlights the match for "teh" (among others).
Press the dispatch key for avy-action-ispell
Select the candidate, in this case the "teh" match.
This runs ispell-word again, and 、"teh" can be corrected.
-->

1. avy-goto-char-timer(Avyの他のジャンプコマンドでもよい)を呼び出す。

2. "can"とタイプすると"candidate"(スペル間違い)がハイライトされる。

3. avy-action-ispell用のディスパッチキー(デフォルトではiにセットされている)を押下する。

4. マッチのいずれか(ここではスペル間違いの"candidate"のマッチ)を選択する。

5. これにより選択にたいしてispell-wordが実行される。

6. 正しいスペルを選ぶ。

7. もう一度avy-goto-char-timerを呼び出す。

8. "te"をタイプすると"teh"にたいするマッチが(他より強調されて)ハイライトされる。

9. avy-action-ispell用のディスパッチキーを押下する 。

10. 候補(ここでは"teh"のマッチ)を選択する。

11. これによりもう一度ispell-wordが実行されて、"teh"が修整される。

</div></details>

<!--
You can replace avy-action-ispell (built-in) with a version that automatically picks the top correction for a word, automating the process:
-->

このプロセスを自動化するために、一番上にある修整を自動的に採用するバージョンで、avy-action-ispell(ビルトイン)を置き換えることもできる。

```elisp
(defun avy-action-flyspell (pt)
  (save-excursion
    (goto-char pt)
    (when (require 'flyspell nil t)
      (flyspell-auto-correct-word)))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

;; セミコロンにバインド (flyspellが使用するのはC-;)
(setf (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell)
```

## 単語の定義

<!--
I use the dictionary package for Emacs, and I’m lazy about it:
-->

わたしはEmacsdictionaryパッケージを使っているので、単語の登録は怠けがちだ:

[avy-define.mp4](https://karthinks.com/img/avy-define.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer (or any other Avy jump command)
Type in "non", this highlights matches for "nonpareil" (among others)
Press the dispatch key for avy-action-define, set to = here
Select the candidate, in this case one of the "nonpareil" matches.
This produces the buffer with the definition of "nonpareil".
Call scroll-other-window (C-M-v) to scroll the dictionary window.
Call avy-goto-char-timer again.
Type in "fi", this highlights the match for "finch" (among others). Note that this match is in another buffer, the one with the definition. We did not have to switch buffers.
Press the dispatch key for avy-action-define
Select the candidate, in this case the "finch" match.
This produces the buffer with the dictionary definition of "finch"
-->

1. avy-goto-char-timer(Avyの他のジャンプコマンドでもよい)を呼び出す。

2. "non"とタイプすると"nonpareil"にたいするマッチが(他より強調されて)がハイライトされる。

3. avy-action-define用のディスパッチキー(ここでは=にセットされている)を押下する。

4. マッチのいずれか(ここでは"nonpareil"にたいするマッチのいずれか)を選択する。

5. これにより""nonpareil""の定義とともにバッファーが生成される。

6. こdefinitionバッファーをスクロールするために、scroll-other-window (C-M-v)を呼び出す。

7. もう一度avy-goto-char-timerを呼び出す。

8. "fi"をタイプすると"finch"にたいするマッチが(他より強調されて)ハイライトされる。これは定義をもつ別のバッファーでのマッチであることに注意。このバッファーへの切り替えは不要。

9. avy-action-define用のディスパッチキーを押下する 。

10. 候補(ここでは"finch"のマッチ)を選択する。

11. これによりもう一度finchの辞書定義とともにバッファーが生成される。

</div></details>

```elisp
;パッケージマネージャーを変えたりお気に入りの辞書パッケージに変えてもよい
(package-install 'dictionary)

(defun dictionary-search-dwim (&optional arg)
  "ポイント位置の単語の定義を検索する。
リージョンがアクティブなら、そのリージョンのコンテンツを検索する。
プレフィックス引数とともに呼び出した場合には、検索する単語の入力を求める。"
  (interactive "P")
  (if arg
      (dictionary-search nil)
    (if (use-region-p)
        (dictionary-search (buffer-substring-no-properties
                            (region-beginning)
                            (region-end)))
      (if (thing-at-point 'word)
          (dictionary-lookup-definition)
        (dictionary-search-dwim '(4))))))

(defun avy-action-define (pt)
  (save-excursion
    (goto-char pt)
    (dictionary-search-dwim))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?= avy-dispatch-alist) 'dictionary-search-dwim)
```

## シンボルのドキュメントを調べる

[avy-help-demo.mp4](https://karthinks.com/img/avy-help-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer
Type in text to filter with, in this case "pc".
Press H to run avy-action-helpful.
Select a candidate phrase, in this case "pcase-lambda". This pulls up a documentation buffer for this symbol.
scroll-other-window with C-M-v to scroll the help buffer.
call avy-goto-char-timer
Type in text to filter with, in this case "ma".
Press H to run avy-action-helpful.
Select a candidate phrase, in this case "macroexp-parse-body". Note that this is matched in the other (help) window. This pulls up the documentation for this symbol.
Repeat steps 5-9 to find the documentation of another symbol, in this case memq.
-->

1. avy-goto-char-timerを呼び出す。

2. フィルタリングするためのテキスト(ここでは"case")をタイプする。

3. run avy-action-helpfulを実行するためにHを押下する。

4. 候補フレーズ(ここでは"pcase-lambda")を選択する。これによりシンボルのドキュメントのバッファーがオープンする。

5. helpバッファーをスクロールするために、C-M-vでscroll-other-windowを呼び出す。

6. avy-goto-char-timerを呼び出す。

7. フィルタリングするためのテキスト(ここでは"ma")をタイプする。

8. avy-action-helpfulを実行するためにHを押下する。

9. 候補フレーズ(ここでは"macroexp-parse-body")を選択する。これは他のウィンドウ(helpバッファー)でのマッチであることに注意。これによりシンボルのドキュメントが表示される。

10. 他のシンボル(ここでは"memq"のドキュメントを調べるために、ステップ5-9を繰り返す。

</div></details>

```elisp
;パッケージマネージャーの置き換えや別のヘルプライブラリーの選択も可
(package-install 'helpful)

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)
```

## 単語やsexpにたいするGoogle検索[^4]

<!-- 
You’ll need an Emacs feature that can search Google for you. There are several. I use a CLI program named Tuxi for this, and it’s pretty handy:
-->

Googleを検索できるEmacs機能が必要になるだろう。[いくつか存在する](https://melpa.org/#/?q=google%20search)が、わたしが使っている[Tuxi](https://github.com/Bugswriter/tuxi%20)という名前のCLIプログラムはとても便利だ:

[avy-tuxi-demo.mp4](https://karthinks.com/img/avy-tuxi-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Call avy-goto-char-timer (or any other Avy jump command)
Type in "ema", this highlights matches for "Emacs" (among others)
Press the dispatch key for avy-action-tuxi, set to C-= here
Select the candidate, in this case one of the "Emacs" matches.
This produces the buffer with the Google’s description of Emacs.
Call avy-goto-char-timer again.
Type in "vi", this highlights the match for "Vi" (among others). Note that this match is in another buffer, the one with the Google result. We did not have to switch buffers.
Press the dispatch key for avy-action-tuxi
Select the candidate, in this case the "Vi" match.
This produces the buffer with Google’s description of Vi.
Repeat steps 6-10 but selecting the string "POSIX" instead.
-->

1. avy-goto-char-timer(Avyの他のジャンプコマンドでもよい)を呼び出す。

2. "ema"とタイプすると"Emacs"にたいするマッチが(他より強調されて)がハイライトされる。

3. avy-action-tuxi用のディスパッチキー(ここではC-=にセット)を押下する。

4. 候補(ここでは"Emacs"にたいするマッチのいずれか)を選択する。

5. これによりGoogleのEmacsの説明とともにバッファーが生成される。

6. もう一度avy-goto-char-timerを呼び出す。

7. "vi"とタイプすると"Vi"にたいするマッチが(他より強調されて)がハイライトされる。これはGoogleの説明が表示されている別のバッファーでのマッチであることに注意。このバッファーへの切り替えは不要。

8. avy-action-tuxi用のディスパッチキーを押下する。

9. 候補(ここでは"Vi"にたいするマッチ)を選択する。

10. これによりGoogleのViの説明とともにバッファーが生成される。

11. "POSIX"という文字列を選択して、ステップ6-10を繰り返す。

</div></details>

<!-- Now: We could continue populating avy-dispatch-alist with functions to do increasingly arcane contextual actions, but let's take a step back. We want a list of easily callable actions on pieces of semantically classified buffer text… now where have we seen something like this before? -->

さて、信じ難いほど難解なコンテキスト向けのアクションをこなす関数を、avy-dispatch-alistに追加し続けることも可能ではあるが、ひとまず一歩下がってみよう。わたしたちが望んでいるのは意味的にクラス分けされているバッファーテキストにたいして、簡単に呼び出すことが可能なアクションのリストだった。[これに似た何か](https://karthinks.com/software/fifteen-ways-to-use-embark)を以前どこかで目にしたことはないだろうか?

# Avy + Embark: どこでもAnyアクション

<!--
Avy and Embark plug into each other like LEGO blocks. Here are a couple of examples:
-->

AvyとEmbarkはレゴブロックのように互いに接続させることができる。例をいくつか示そう:

## 出現位置のハイライト

<!-- 
In this demo I highlight some keywords in a busy LaTeX document, then visit the bibliography entry of a citation key with Avy and Embark, without ever manually moving the cursor:
-->

このデモでは細部まで記述されたLaTeXドキュメントの一部のキーワードをハイライトして、その後に手作業でカーソルを移動するのではなく、AvyとEmbarkを用いて引用キーから参考文献にアクセスしている:

[avy-embark-demo-2.mp4](https://karthinks.com/img/avy-embark-demo-2.mp4)

<details><summary>ビデオ実況</summary><div>

<!---
Call avy-goto-char-timer (or any other Avy jump command)
Type in “flo” to filter matches that include  “Floquet”
Run avy-action-embark with o.
Select one of the matches for “Floquet”. This runs Embark on the match.
Select the embark-toggle-highlight action with H.
Repeat 1-5 to highlight “Parametric”.
Call avy-goto-char-timer again.
Type in “na” to match one of the citation keys (among others)
Run avy-action-embark with o.
Select the citation key match. This runs Embark on it.
Choose the bibtex-action to visit the Bib file, bound to e by the bibtex-actions package.
-->

1. avy-goto-char-timer(Avyの他のジャンプコマンドでもよい)を呼び出す。

2. "Floquet"を含むマッチにフィルタリングするために、"flo"とタイプする。

3. oでavy-action-embarkを実行する。

4. "Floquet"にたいするマッチからいずれかを選択する。これによりそのマッチにEmbarkが実行される。

5. Hでembark-toggle-highlightアクションを選択する。

6. "Parametric"をハイライトするために、1-5を繰り返す。

7. もう一度avy-goto-char-timerを呼び出す。

8. 引用キーの中から1つのキーにマッチさせるために、"na"とタイプする。

9. oでavy-action-embarkを実行する。

10. マッチした引用キーを選択する。これによりそのマッチにEmbarkが実行される。

11. Bibファイル(訳注: 文献データベースファイル)をvisitするために、bibtex-actionを選択する(bibtex-actionsパッケージによってeにバインドされている)。

</div></details>

<!-- 
## Run a skein through Emacs’ help systems
-->

## Emacsのヘルプシステムで複雑なもつれを切り抜ける

<!-- 
In this demo I explore my way through a package with Avy and Embark, threading help, apropos and customization buffers, again without manually moving the cursor.
-->

このデモではパッケージで何かを探す際にAvyとEmbarkを使ってhelp、apropos、customizationのバッファーを結びつける。ここでも手動でのカーソル移動は行わない。

[avy-embark-demo-1.mp4](https://karthinks.com/img/avy-embark-demo-1.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
1. 
Call avy-goto-char-timer.
Type in text to filter with, in this case “root”.
Run avy-action-embark with o.
Select “project-root”, one of the matches. This runs Embark on the match.
Press h, which makes Embark run describe-symbol on the match. This opens up a help buffer for the function. (Note: we bound a help command to Avy earlier, we could have used that.)
Press C-M-v (scroll-other-window) to scroll the help buffer.
Call avy-goto-char-timer again.
Type in text to filter with, in this case “proj”.
Run avy-action-embark with o.
Select “project-x”, which is one of the matches. This runs Embark on the match.
Call embark-cycle to change the target from a file (named “project-x”) to a library (named “project-x”)
Press h, which makes Embark run finder-commentary on the project-x library. This opens a buffer with some commentary.
Repeat the previous steps to run Embark on “project-x” again. This time, run the apropos-library action with a in Embark. This opens an apropos buffer.
Repeat the previous steps to run Embark again, this time on the symbol “project-x-local-identfier”.
Choose the customize-variable action with u in Embark. This opens a customization buffer for the variable project-x-local-identifier.
-->

1. avy-goto-char-timerを実行する。

2. フィルタリングするためにテキスト(ここでは"root")をタイプする。

3. oでavy-action-embarkを実行する。

4. マッチの中からproject-rootを選択する。これによりそのマッチにたいしてEmbarkが実行される。

5. hを押下するとマッチにたいしてEmbarkがdescribe-symbolを実行する。これによりそのマッチの関数にたいするヘルプバッファーがオープンする(これより前の箇所でAvyにヘルプコマンドをバインドしてあるので、そちらを使うこともできるだろう)。

6. ヘルプバッファーをスクロールするためにC-M-v (scroll-other-window)を押下する。

7. もう一度avy-goto-char-timerを呼び出す。

8. フィルタリングするためにテキスト(ここでは"proj")をタイプする。

9. oでavy-action-embarkを実行する。

10. マッチの中から"project-x"を選択する。これによりそのマッチにたいしてEmbarkが実行される。

11. embark-cycleを呼び出して、ターゲットを("poject-x"という名前の)ファイルから("poject-x"という名前の)ライブラリーに変更する。

12. hを押下するとproject-xライブラリーにたいしてEmbarkがfinder-commentaryを実行する。これによりコメントが表示されているバッファーがオープンする。

13. 前のステップを繰り返して、もう一度"project-x"にたいしてEmbarkを実行する。

14. 今度はシンボル"project-x-local-identfier"にたいして前のステップを繰り返す。

15. uでEmbarkのcustomize-variableアクションを選択する。これにより編集project-x-local-identfierにたいするcustomizationバッファーがオープンする。
-
</div></details>

## 責任の分担

<!-- 
We save ourselves a lot of redundancy and reuse muscle memory here. Avy provides its unique means of filtering and Embark does what it does best, run actions! The intermediate job of candidate selection is shared between Avy and Embark: Avy specifies the general location of the candidate, and Embark figures out the semantic unit at that position on which to act. The fact that the Filter → Select → Act process is helpfully chunked this way by Avy makes the elisp required to integrate the two completely trivial[^5].
-->

ここではわたしたち自身がもつ大量の冗長性と指に教え込んだ記憶を節約する。Avyには独自のフィルタリング手段を提供してもらい、Embarkには自分のベストを尽くすこと、すなわちアクションを実行してもらうのだ! 候補選択における中間的な作業はAvyとEmbarkで共有して行ってもらおう。Avyが候補の一般的な位置を指定して、その位置でアクションを行う際の意味的単位の解決はEmbarkが行う。Avyによってフィルター -> 選択 -> アクションというプロセスが使いやすいようにchunk化(訳注: あるものをより小さな断片に分割したり、より大きな断片にまとめすること)されているので、2つを統合するために必要なelispは簡単なもので事足りるはずだ[^5]。

```elisp
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
```

<!-- 
Note that if you don’t like the candidate that Embark picks as the unit to act on, you can call embark-cycle to cycle through the other targets.
-->

アクションを行う単位としてEmbarkが採用した候補が気に入らなければ、他のターゲットに巡回するためにembark-cycleを呼び出すことができる。

<!-- 
All that, and we didn’t even move the point.
-->

これまではすべて、ポイントを動かすことさえ行っていない[^6]。

# Avy + Isearch: 探してジャンプ

<!-- 
Isearch and Avy have different strengths: Avy jumps quickly to any visible element in any window, Isearch to any matching candidate in this buffer. Avy is faster when you want to cover some distance in a jump, Isearch when you’re moving a small distance or a very large one. Avy is useful when your eyes are already on a target, Isearch when you’re looking for one. But you don’t have you choose. You can handily combine the two by restricting Avy’s candidate pool to Isearch candidates: now you can start Isearch and finish with Avy:
-->

IsearchとAvyにはお互い違う強みがある。Avyは任意のウィンドウ上にある可視の要素すべて、Isearchはバッファー内の任意のマッチ候補に素早くジャンプできる。ある程度の距離範囲をジャンプしたければAvy、短距離または非常に長い距離を移動したければIsearchが適しているだろう。すでに視界にターゲットを収めている場合にはAvyが役に立つし、何かを探している場合にはIsearchが便利だ。しかしいずれか一方を選ぶ必要はない。Avy候補プールIsearchの候補に制限すれば、2つを簡単に組み合わせることができるからだ。こうすればIsearchを開始してAvyで完了できる:

```elisp
(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)
```

<!-- 
Again, consciously deciding which of the two commands to call every time is a bad idea. It’s not a bad idea to always Isearch and switch to Avy when necessary:
-->

繰り返す。2つのコマンドのいずれかを呼び出すかを毎回意識的に判断するのは悪いアイデアだ。常にIsearch呼び出して、必要に応じてAvyを呼び出すというのなら悪くないが。

[avy-isearch-basic-demo.mp4](https://karthinks.com/img/avy-isearch-basic-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Start Isearch with C-s. In the video I switched to isearch-regexp with M-r.
Type in a phrase to search for. In the video I typed in a regexp that ends in “-region”.
Navigate Isearch matches with C-s, recentering the screen with C-l if necessary.
Call avy-goto-char-timer. The candidate pool limits to the Isearch matches.
以前と同様に、ジャンプ先の Isearch 一致を選択します。Pick an Isearch match to jump to as before.
-->

1. C-sでIsearchを開始する。ビデオではM-rでisearch-regexpに切り替えている。

2. 検索するフレーズをタイプする。ビデオでは"-region"で終わるregexpをタイプしている。

3. Isearch のマッチ間のナビゲートはC-s、スクリーンの再センタリングは必要に応じてC-lで行っている。

4. avy-goto-char-timerを呼び出す。候補プールはIsearchのマッチによって限界が定められる。

5. 以前と同様、Isearchのいずれかのマッチを選んでジャンプする。

</div></details>

<!--
At least, that’s the usual pitch.

For us, however, “jump” in that description is replaced with “act”. We can act on any visible Isearch candidate with one of the above actions. Kill text between two isearch matches? Copy the previous line that contains a word to the current location? Check. Essentially we filter with Isearch and select and Act with Avy, indirectly decoupling Filter from the other two actions in Isearch.
-->

少なくとも、これが通常のピッチだろう。

ただしわたしたちにとって説明の中の"ジャンプ"は"アクション"に置き換えられる。Isearchの可視な候補であればどれでにたいしても、上述したいずれかのアクションを適用できる。Isearchのマッチとマッチの間のテキストのkill? ある単語を含む前の行をカレント位置にコピー? 試してみよう。要するにIsearchからフィルター以外の2つのアクションを間接的に切り離してフィルタリングと選択を行い、Avyでアクションを行えばよいのだ。

# Avyが賢こ過ぎるとき

<!--
This usage pattern has a failure mode. When there’s a single match, Avy jumps to the location and does not offer any actions. Oops.

While it’s possible to force Avy to show a selection char/hint for a single match, the default DWIM behavior is usually desirable. There are two options:
-->

<!--
Filter candidates conservatively, for example by typing in a single character. Using avy-goto-char or avy-goto-char-2 will almost always result in more than one match, preventing this problem. If you use one of the timer-based Avy commands, you can vary how much text to filter by on the fly. 
-->

<!--
Carry out the action the old-fashioned way after jumping, then jump back by popping the mark Avy sets. You can do this with the default set-mark-command (C-u C-SPC)7. You can do this for most commands that cause the point to jump, including Isearch. Vim users have the jumplist, accessed with C-o, and the changelist, accessed with g;.

In this demo I jump twice with Avy to edit text and then chain jump my way back to where I started:
-->

今度のパターンには失敗モードがある。マッチが1つしかなければ、その位置にAvyがジャンプするだけでアクションは何も提案しないのだ。あらら…

1つだけのマッチにたいして選択用のヒント/文字を表示するようAvyに強いることも可能だが、デフォルトのDWIMを選択する挙動のほうが通常は望ましいだろう。その場合に考え得るはオプションが2つある:

- たとえば1文字をタイプするといったような保守的な方法によって候補をフィルタリングする。avy-goto-charやavy-goto-char-2を使えば、ほとんど常に2つ以上のマッチを得られるので、この問題は回避できる。timerベースのAvyコマンドのいずれかを使えば、フィルターにどのくらいのテキストを用いるかをその場で変更できる。 

- ジャンプしたら昔ながらの方法でアクションを実行して、その後はAvyがセットしたマークpopして元の位置にジャンプして戻る。これはデフォルトのset-mark-command (C-u C-SPC)で行うことができる[^7]。これはポイントをジャンプさせるようなほとんどのコマンド(Isearchも含む)で行うことが可能だ。Vimユーザーならジャンプリスト(jumplist)へのアクセスはC-o、変更リスト(changelist)はgでアクセスできる。 

以下のデモはテキストを編集するためにAvyで2回ジャンプ、それから連鎖的にジャンプを辿って開始位置に戻っている:

[avy-jump-back-demo.mp4](https://karthinks.com/img/avy-jump-back-demo.mp4)

<details><summary>ビデオ実況</summary><div>

1. avy-goto-char-timerを呼び出して候補にジャンプする(またはアクシデントによりそこで終わる

2. 編集する(またはしない)。

3. プレフィックス引数とともにset-mark-commandを呼び出して戻る(C-u C-SPC)。これらのジャンプを連鎖して行うことができる。

</div></details>

# ところでポイントとは何ぞや?

<!-- 
This section is for the pedants. 
-->

これは知識をひけらかしたい人向けのセクションだ。

<!--
I’ve been using “point” and “cursor” interchangeably in this article. Yes, I’m aware of the distinction.
-->

この記述では"ポイント"と"カーソル"を互いに置き換え可能な用語として用いてきた。これら2つ用語の定義の違いについては理解している。

<!-- 
One of the illustrated advantages of using Avy to filter and select text to run actions on is that you can do it without moving the cursor. But as the above code snippets make clear with their save-excursion blocks, we do move the point, mostly just invisibly. The point is where the “gap” in Emacs’ gap-buffer data structure is located, so Emacs commands are all oriented around acting on the point, and usually more efficient when doing so. 
-->

Avyを用いることで得られる利点として、カーソルを移動せずにアクションを実行するテキストのフィルタリングと選択を行えることが証拠の1つとして挙げられる。しかしこれまで示してきたコードスニペットのsave-excursionブロックによりポイントを移動しないのではなく、単にほとんど不可視な状態でポイントを移動していたことが明らかになった。ポイントがあるのはEmacsのgap-bufferと呼ばれるデータ構造体の中であり、Emacsのコマンドはすべてそのポイントを中心に動作することを指向しているし、通常はその方が効率的なのだ。

<!--
Yes: it’s much faster to run an Avy action on some text in a different window than it is to call other-window, then Isearch to the text, run an action and switch back. But to me, the Point (har) is primarily a useful abstraction for writing elisp. The real advantage of Avy is in how it lets me think about the contents of the entire frame in the powerful Filter → select → Act paradigm. The fact that you can do this without the mental context switch involved in expressly navigating around the window or frame is a bonus. 

chatgptによる解答

「(har)」は「ハードウェアポイント（hardware point）」の略です。これは、Emacsのドキュメントやコミュニティで使われる用語の一つで、カーソルの位置や特定の場所を指す場合に使われることがあります。つまり、「Point (har)」はEmacs内のカーソル位置の概念を指しており、エディタ内のテキスト操作やプログラムの制御に関わる重要な抽象概念です。

この場合、著者はカーソル位置（Point）を特に強調して「ハードウェアポイント」として言及しているようです。これにより、カーソル位置がelispのスクリプトを書く際にいかに重要であるかを強調しています。

もし他の文脈や意味がある場合、追加の情報や異なる解釈が必要かもしれませんが、一般的にはこのように理解されます。

-->

正にその通り。other-windowを呼び出してからテキストをIsearch、アクションを実行して戻るよりも、別のウィンドウにあるテキストにたいして任意のAvyアクションを実行するほうがより高速だ。しかしわたしにとってポイント(har)は、elispを記述に役に立つ抽象化なのだ(訳注: harはポイントを実際に実装するgapではなくハードウェアポイント、つまりカーソル位置と等価な意味でのポイントを強調するためだと思われる)。フレーム全体のコンテンツについて、フィルター -> 選択 -> アクションという強力なパラダイムで考えさせてくれることがAvyの真価だ。意識的にウィンドウやフレームを移動することで生じる精神的なコンテキストスイッチなしで操作を行うことができるという事実は、ボーナスのようなものだろう。

# Avyにたいする心構え

<!-- 
In some ways, using Avy to jump around the screen is like using a mouse. You could make the case, quite successfully, that the mouse is faster here and thus preferable. This unfavorable comparison evaporates when you add your dispatch actions into the mix. Yanking a line from another window or running goto-definition on a symbol on the other edge of the screen is much faster with Avy than with the mouse selection/right-click business. And this is without taking into account the disruptive effect of frequent context switching, the reason to prefer all keyboard navigation (or all mouse, when possible) in the first place.
-->

Avyでスクリーン上を飛び回るのは、マウスを使うのとある点似ている。その見地からマウスのほうが速いので好ましいと主張するのは簡単だ。この組み合わせにアクションのディスパッチを追加すれば、この不公平な比較は消失する。他のウィンドウからの行のyankやスクリーン向こう端のシンボルにたいするgoto-definitionの実行では、マウスによる選択/右クリック稼業よりAvyのほうが遥かに高速だ。そもそもこれこそが頻繁なコンテキストスイッチによる破壊的効果の勘定を抜きにしても、キーボード(もし可能ならマウスでも)で全操作を行うことを好む理由なのだ。

## 嗅覚テスト

<!--
However, using Avy actions is a new way of interacting with text on your screen even if you already use Avy to jump around. To remember to use Avy actions or find new ones, I look for “smells” in my day-to-day Emacs usage:
-->

あなたがすでにAvyのジャンプを使っていたとしても、Avyのクションの使用はスクリーン上のテキストとのやりとりにおける新たな手段だろう。Avyアクションの使用や新たなアクションを発見する心構えとして、わたしはEmacsの日常的な使い方の"匂い"を探すよう努めている:

<!--
Switching windows multiple times to land my cursor on some text
Isearching through more than three matches to jump to the right one
Moving the point a long distance to run a lookup command
Activating the mark manually (C-SPC) all the time
Jumping to locations to delete single words
-->

- テキストにカーソルを合わせるためにウィンドウを何回か切り替えている
- Isearchで正しいマッチにたどり着くために4個以上のマッチをジャンプしている
- 何かを調べるコマンドを実行するためにポイントを長距離移動している
- 毎回C-SPCによる手作業でマークをアクティブにしている
- 単語1つの削除のために場所をジャンプしている

## キーマップに埋もれて


<!--
The other sense of “remembering to Avy” is that piling a new abstraction onto simple text editing means you have to learn a new keymap. Emacs already has too many of those!
-->

"Avyにたいする心構え"には別の意味がある。シンプルなテキスト編集の上に新たな抽象化レイヤーを積み重ねるということは、新たにキーマップを学ぶ必要があることをも意味しているのだ。Emacsのキーマップに関してはすでに多すぎるというのに!

<!-- 
This is true. But the effort is greatly mitigated by a choice of keys that is sensible to you. In the above code snippets, I made choices that mimic my Emacs’ keybindings (which are close to the default ones) so I don’t have to remember anything new

: -->

これは真実だが自分にとって道理に適ったキーを選択することで、労力は大幅に軽減されるだろう。これまでのコードスニペットでは、わたしのEmacsのキーバインディング(ほぼデフォルト)を真似たので、新たに何かを覚える必要はないはずだ:

| Action          | Avy keybinding | Emacs keybinding     | Emacs Default? |
|-----------------|----------------|----------------------|----------------|
| Copy            | w, W (line)    | M-w                  | Yes            |
| Yank            | y, Y (line)    | C-y                  | Yes            |
| Transpose       | t, T (line)    | C-t, M-t etc         | Yes            |
| Zap             | z              | M-z                  | Yes            |
| Flyspell        | ;              | C-;                  | Yes            |
| Mark            | m              | m in special buffers | Yes            |
| Activate region | SPC            | C-SPC                | Yes            |
| Dictionary      | =              | C-h =                | No             |
| Google search   | C-=            | C-h C-=              | No             |
| Embark          | o              | C-o                  | No             |


<!--
You can go beyond the mnemonic and simply reuse the same keybindings you use in regular editing, trading off a slightly longer key sequence for maximally reusing your muscle memory. If you’re an Embark user, you don’t even need the above keys, just one to call embark-act.
-->

ニーモニックに留めず通常の編集で使うのと同じキーバインディングを再利用するだけだ。若干長目のキーシーケンスという犠牲を払うことによって、指に覚えさせた記憶を最大限に再利用できる。Embarkユーザーであれば上記のキーすら必要なくなる。必要なのはembark-actの呼び出しだけだ。

# 未解決問題

<!--
There are two common editing actions that still require manually moving the point elsewhere, perhaps to another window: 
-->

一般的な編集においてポイントをどこか(別のウィンドウかもしれない)へ移動する際に、依然として手作業を要するアクションが2つ存在する:

<!--
Searching or jumping to the contents of other windows beyond the confines of the screen. This has a simple solution: 
-->

- スクリーンの境界を超えた別ウィンドウのコンテンツにたいする検索やジャンプ。これにはシンプルな解決策がある:

<details><summary>別ウィンドウをIsearchする</summary><div>

```elisp
(defun isearch-forward-other-window (prefix)
    "Function to isearch-forward in other-window."
    (interactive "P")
    (unless (one-window-p)
      (save-excursion
        (let ((next (if prefix -1 1)))
          (other-window next)
          (isearch-forward)
          (other-window (- next))))))

(defun isearch-backward-other-window (prefix)
  "Function to isearch-backward in other-window."
  (interactive "P")
  (unless (one-window-p)
    (save-excursion
      (let ((next (if prefix 1 -1)))
        (other-window next)
        (isearch-backward)
        (other-window (- next))))))

(define-key global-map (kbd "C-M-s") 'isearch-forward-other-window)
(define-key global-map (kbd "C-M-r") '
```

</div></details>

<!--
In keeping with C-M-v to scroll the other window, you can Isearch the other window with C-M-s without switching to it8. If you’re feeling adventurous, replace (other-window next) in the above functions with (ace-window).
-->

  C-M-vで他のウィンドウのスクロールしながら、そのウィンドウに切り替えずにC-M-sでIsearchできる[^8]。冒険してみたければ、上記関数の`(other-window next)`を`(ace-window)`に置き換えてみよう。

<!-- 
You can call Avy from Isearch as before, to run actions on essentially any text in the other-window’s buffer.
-->

  基本的には他のウィンドウのバッファーにある任意のテキストにたいして、以前と同じようにIsearchからAvyを呼び出すことができる。

- リージョンのテキストのコピー。これにはavy-copy-regionというAvyベースの解決策がある。とは言え、必要なのはAvyコマンドを1回呼び出すだけ、というのが冒頭の約束だった。現時点ではAvyだけで短いジャンプを繰り返すか、あるいはIsearchAvyを使うという退屈な方法でこれを行うことになるだろう。もっとelispっぽい解決策については、この記述のシリーズ第2弾を待ってほしい。

<!--
This post primarily concerned itself with the Act part as it connects with the ideas in the the previous one about ways to use Embark. But Avy is composed of modular pieces that makes it suitable for a wide variety of Filter → Select applications as well. In part II of this series, we will dig into the Avy API and see how to create unique commands.
-->

この記事は[Embarkの使用方法に関する記事](https://karthinks.com/software/fifteen-ways-to-use-embark)のアイデアと関連しているので、主にアクションについて記述されている。しかしAvyはモジュールの組み合わせにより構成されているので、フィルター -> 選択によって機能するさまざまなアプリケーションの広い範囲に適正がある。シリーズ第2弾ではAvyのAPI、および独自コマンドを作成する方法についても掘り下げていくつもりだ。

[^1]: 技術的には候補の並べ替えも含まれる。この内訳ではフィルターとしてひとまとめにしてある。

[^2]: Isearchはフィルター/選択/アクションを効果的に1つのコマンドに組み合わせている。とは言えこれは他にも多くのアクションをサポートする、非常に巧妙にデザインされたEmacs用ライブラリーなのだ。Swiperや類似コマンドによってIsearchを完全に置き換えてしまった人たちはチャンスを逃していると思う。

[^3]: あるいはEasymotionのEmacs版がAvyなのかもしれない。

[^4]: なぜsexpをググりたいのだろうか? もっとも一般的なのは、sexpがFink-Nottleのようにハイフン区切りフレーズの場合だろう。

[^5]: この関数を改善してくれたOmar Antolinに感謝する。

[^6]: Avyのすべてのアクション関数の内部でsave-excursionを使用しているので、これは技術的には誤りだ。ここでの実際の成果はEmbarkに言及する前に、このコード断片にほぼ4000語にのぼる単語を詰め込んだことだろう。

[^7]:  違うウィンドウに戻ってしまう場合には、かわりにpop-global-mark (C-x C-SPC)でグローバルマークをpopできる。今ではdogearsのような外部パッケージでこれを行うことができる。

[^8]: C-M-sはデフォルトではisearch-forward-regexpにバインドされている。isearch-forward-regexpにはC-s M-rのようにIsearchマップでM-rとタイプすればアクセスできる。

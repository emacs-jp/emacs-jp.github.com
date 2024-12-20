**これはKarthik Chikmagalurさんによって記述された記事を日本語に翻訳した記事であり、記事の所有権と著作権はKarthik Chikmagalurさんに帰属します。**

元の記事: [The Emacs Window Management Almanac | Karthinks](https://karthinks.com/software/emacs-window-management-almanac/)

- - -

["ウィンドウ管理"ってどんな意味だろう](#ウィンドウ管理ってどんな意味だろう)
* [この記事で説明しないことは…](#この記事で説明しないことは)
* [ウォーミングアップ](#ウォーミングアップ)
   * [other-windowと"次のウィンドウ(next window)" (built-in)](#other-windowと次のウィンドウnext-window-built-in)
   * [windmove (built-in)](#windmove-built-in)
   * [frames-only-mode](#frames-only-mode)
   * [winum-mode](#winum-mode)
   * [ace-window](#ace-window)
   * [マウスであれこれ (built-in)](#マウスであれこれ-built-in)
   * [transpose-frame (回転、フリップフロップ)](#transpose-frame-回転フリップフロップ)
      * [rotate-frame](#rotate-frame)
      * [flip-frame](#flip-frame)
      * [flop-frame](#flop-frame)
   * [window-prefix-map (built-in)](#window-prefix-map-built-in)
      * [split-root-window-rightとsplit-root-window-below](#split-root-window-rightとsplit-root-window-below)
      * [tab-window-detachとtear-off-window](#tab-window-detachとtear-off-window)
   * [other-window-prefix (built-in)](#other-window-prefix-built-in)
      * [イライラその１](#イライラその１)
      * [イライラその２](#イライラその２)
      * [イライラその３](#イライラその３)
   * [ウィンドウ構成の保存とリストア](#ウィンドウ構成の保存とリストア)
   * ["oops"オプション](#oopsオプション)
* [深堀り](#深堀り)
   * [back-and-forth手法](#back-and-forth手法)
   * [other-windowの改善](#other-windowの改善)
      * [一石二鳥](#一石二鳥)
      * [switchy-window](#switchy-window)
      * [other-window-alternating](#other-window-alternating)
   * [ace-windowのディスパッチによるウィンドウマジック](#ace-windowのディスパッチによるウィンドウマジック)
      * [Emacsウィンドウにとってのcompleting-readに匹敵するもの: aw-select](#emacsウィンドウにとってのcompleting-readに匹敵するもの-aw-select)
      * [tear-off-windowとtab-window-detach](#tear-off-windowとtab-window-detach)
      * [ace-window-one-command: 任意のコマンドをace-windowで](#ace-window-one-command-任意のコマンドをace-windowで)
      * [ace-window用のwindow-prefixコマンド](#ace-window用のwindow-prefixコマンド)
* [ウィンドウを切り替える必要があるのか?](#ウィンドウを切り替える必要があるのか)
   * [切り替えて留まる: ウィンドウ切り替え機能としてのAvy](#切り替えて留まる-ウィンドウ切り替え 機能としてのavy)
   * [切り替えて戻る: 別のウィンドウでのアクション](#切り替えて戻る-別のウィンドウでのアクション)
   * [scroll-other-window (built-in)](#scroll-other-window-built-in)
   * [isearch-other-window](#isearch-other-window)
   * [次のウィンドウでのバッファーの切り替え](#次のウィンドウでのバッファーの切り替え)
   * [master-modeとscroll-all-mode](#master-modeとscroll-all-mode)
   * [with-other-window: elispヘルパー](#with-other-window-elispヘルパー)
* [たくさんのウィンドウは必要か?](#たくさんのウィンドウは必要か)
   * [ウィンドウが作られたら無視する](#ウィンドウが作られたら無視する)
   * [ウィンドウの世話をしなくてよいようにウィンドウを取り扱う](#ウィンドウの世話をしなくてよいようにウィンドウを取り扱う)
   * [Popper、Popwin、shell-pop、vterm-toggle](#popperpopwinshell-popvterm-toggle)
* [未解決事項](#未解決事項)
   * [window-tree](#window-tree)
   * [タイル式ウィンドウマネージャー統合](#タイル式ウィンドウマネージャー統合)
* [ここからの眺め](#ここからの眺め)

- - -

<!-- this write-up assumes that you’ve finished at least the Emacs tutorial and are familiar with basic  Emacs terminology (what’s a buffer, window and a frame) and with window actions: splitting windows,  deleting them or deleting other windows, and switching focus.  -->
 
- この記事は少なくともEmacsのチュートリアルを終えていること、Emacsの基本的な用語(バッファー、ウィンドウ、フレーム)、およびウィンドウの分割や削除、他のウィンドウの削除、フォーカス切り替えのようなウィンドウアクションに親しんでいることを前提としている。
 
<!-- * There are only a few brief mentions of tabs, as they are primarily a tool for workspace management,  as opposed to window management. -->
- タブはウィンドウ管理というより、主にワークスペースの管理用ツールなので少しだけ触れるに留めている。


<!-- * I’m focusing on window/buffer management within an Emacs frame. Many of the below tools work across  frames just as well, but you’ll have to find the right switches to flip to enable cross-frame  support. -->
- 記事ではEmacsフレーム内でのウィンドウ/バッファー管理に焦点を当てている。この後述べるツールの多くはフレームを跨いでも同じように機能するが、フレーム間のサポートを有効にするためには、それに応じたスイッチを見つける必要があるだろう。

<!-- * Finally, this is more my almanac than a wiki: It covers only tools or ideas I’ve personally  explored over the years, with brief mentions of potentially useful packages that I haven’t tried.  Any omissions are not value judgments, please let me know if I miss something useful. -->
- 最後にこれは記事というよりもわたしのウィンドウ管理における年代記のようなものなので、何年かに渡って個人的に探求したツールやアイデアは網羅しているが、役に立つかもしれないが試していないパッケージについては簡単に言及するに留めている。したがって何かを省略していたとしても、有用性に基づいて省略した訳ではない。有用な何かをわたしが見逃しているようなら、是非知らせいただきたい。

<!-- At some point this transitions from listing well known tools to tips, then hacks, and finally unvarnished opinions. It’s front-loaded: the first chunk of the write-up gives you a 70% solution. If you are new to Emacs, feel free to stop at 30%. If you are an old hand, feel free to skip the first 30%. It also lists substitutes: several ways to do the same things, so you can pick just one method and ignore the rest. Things get progressively more opinionated and idiosyncratic in the second half. -->

この記事の内容はよく知られているツールからヒント、ハッキング、そして最後には忌憚のない意見へと遷移するポイントがいくつかある。この記事には前半ほど多くの内容を詰め込んだ。記述の冒頭部分を読めば解決策の70%が得られるだろう。Emacs初心者なら30%の部分で読むのを止めても構わない。代替え手段についてもリストしておいた。同じことを行うための方法はいくつもあるが、**1つだけ採用して他を無視** しても問題ない。後半になるにつれ、記事の内容は独善的になり特異度が増していくだろう。


<!-- If you are reading this in the future, this write-up is probably out of date. The Emacs core is very stable, but the package ecosystem tends to drift around as packages are developed and abandoned. The built-in solutions will still be around, but there are no guarantees on the third-party packages! That said, the longer a package has been around the more likely it’s going to stick around in a functional state – even if only as a frozen entry in the Emacs Orphanage. -->

あなたが読む頃には、この記事は古くなっていることだろう。Emacsのコア部分は非常に安定しているものの、パッケージの開発や放棄によってパッケージの生態系は揺さぶられ続ける傾向がある。組み込みソリューションは存続し続けるが、サードパーティのパッケージについてはその限りではないのだ! とは言えパッケージの存在期間が長くなるにつれて、機能的な存在としては生き残る可能性が高くなる(たとえ[Emacs Orphanage](https://github.com/emacsorphanage)の凍結エントリーであったとしても)。


<!-- As new ideas emerge, there will be new approaches to window management that aren’t covered here. These innovations don’t need to happen in the Emacs sphere – Emacs likes to steal reinvent ideas that originate elsewhere, much as other applications rediscover ideas that Emacs introduced in the 1990s. So this topic might be worth revisiting afresh in a few years. -->

新たなアイデアの誕生によって、ここでカバーしていないようなウィンドウ管理への新たなアプローチが生まれることだろう。このような革新の発現がEmacsの領域に制限される必要はない。Emacsが1990年代に導入したアイデアを他のアプリケーションが再発見するのと同じくらい、他のどこかで生まれたアイデアをEmacsが ~~拝借~~ 再発見するのはよくあることだ。そういった理由により、このトピックについては数年後に再検討する価値があるのかもしれない。

<!-- What we mean by "window management" -->

# "ウィンドウ管理"ってどんな意味だろう

![emacs-window-buffer-frame.png](https://karthinks.com/img/emacs-window-buffer-frame.png)

<!--
Emacs separates the idea of a window (a "viewport" or "pane" in the frame) from the buffer, a contiguous chunk of text that may or may not be the contents of a file. These concepts are usually fused in IDEs and text editors – this reduces the cognitive load of using the application, but closes the door on more flexible behavior and free-form arrangments. For example, many editors don’t let you have two views of the same file, which is trivial in Emacs. They’re often uncomfortable even with the idea of a dissociated buffer – a buffer that does not represent the (possibly edited) contents of a file. Reified concepts like Emacs’ indirect buffers are completely foreign to them[^1].
-->

Emacsではバッファー(連続したテキストの集合)とウィンドウ(フレーム内部にある"表示領域"あるいは"窓枠")を別の概念として分離している。IDEやテキストエディターでは普通はこれらの概念は融合されている。これによりアプリケーションを使う際の認知負荷は軽減されるが、それは同時により柔軟な動作や自由形式によるアレンジへの道を閉ざすことでもあるのだ。たとえば同じファイルに2つのビューをもつことは多くのエディターではできないが、Emacsでは簡単なことだ。Emacs以外のユーザーが接続されていないバッファー、すなわち(恐らくは編集する)ファイルの内容を表していないバッファーという概念にさえ不快感を覚えるのは珍しくない。Emacsの[インダイレクトバッファー(indirect buffer: 間接バッファー)](https://www.gnu.org/software/emacs/manual/html_node/emacs/Indirect-Buffers.html)のように、具現化された概念は彼らにとってはまったく異質な概念なのである[^1]。

<!--
Emacs allows you to do a lot more, but users have to contend with this cognitive cost. New users pay thrice: they have to deal with getting windows into the right places in the frame, getting buffers into the right windows, and they miss out on the upside because they don’t yet realize what this decoupling makes possible. Hopefully this write-up can address two of these costs.
-->

Emacsならもっと多くのことが可能になる。しかしこの認知コストはユーザーに対価の支払いを要求する。まったく初めてのユーザーは上述の分離がもたらす可能性を理解していないのでフレームの正しい位置へのウィンドウの配置、正しいウィンドウでのバッファーの表示、理解不足による機会逸失という3回払いでその代償を払うことになるのだ。この記事がこれらのうちの2つのコストに対処する道標となることを願っている。

<!--
For reference in the rest of this article, here’s a non-exploded schematic of an Emacs frame, with the left window selected:
-->

以降で参照できるように以下に左側のウィンドウが選択された、まだ目茶苦茶になる前のフレーム図を示しておこう:

![emacs-window-chart.png](https://karthinks.com/img/emacs-window-chart.png)

<!--
- Each colored block is a window, the numbers represent buffers being shown in them.  - The active window is the one with a black border.
-->

- 色付きのブロックはそれぞれウィンドウ、数字はそのウィンドウに表示されているバッファーを表す  

- 黒枠で囲んであるのがアクティブなウィンドウ  

<!--
# This article is not about…
-->

# この記事で説明しないことは…



<!--
Since actions with or on windows in Emacs are primitive, common and unavoidable operations at any level of Emacs usage, this topic is suprisingly subtle, broad and deep, and there’s only so much I can explore in 15,000 words. So we begin with some disambiguation and a narrowing of focus. This article is not about the following things.
-->


Emacsでのウィンドウにたいするアクション、あるいはウィンドウ上でのアクションは、Emacsのあらゆるレベルの使用において避けられないほど原始的かつ一般的な操作である。ウィンドウ関連のアクションは驚くほど微妙、広範かつ深いテーマであり15000語程度で探求できる範囲は限られている。そこでまずは曖昧さを取り除き、焦点を絞ることから始めようと思う。この記事は以下の事柄に関する**記事ではない**。

<details open><summary>バッファー表示の際のルール</summary>
<div>

<!--
> Emacs keeps popping up windows in the wrong places and destroying my window arrangement!
-->

> Emacsが間違った場所にウィンドウをポップアップし続けたので、わたしのウィンドウ配置が台無しだ!

<!--
The situation is… less than ideal. Displaying buffers in the right windows automagically is generally possible but this configuration is involved and requires knowledge of minutiae of the Emacs API, like window-parameters, slots and dedicated windows. the display-buffer API is so involved that describing it takes up a big chunk of the Elisp manual, and even that concludes by saying "just go with it". I mention automatic window management briefly towards the end, but this article is not about reining in the behavior of display-buffer. I recommend Mickey Peterson’s article on demystifying the window manager for this, this video by Protesilaos Stavrou, or the manual if you’ve got the stomach for it.
-->

状況は… 芳しくない。正しいウィンドウにバッファーを*自動的*に表示するのは一般的には不可能ではないものの、これには複雑な設定が必要でありウィンドウパラメーター、スロット、専用ウィンドウ(dedicated window)といったEmacsのAPIの詳細に関する知識が要求される。APIの`display-buffer`についてはElispマニュアル[^2]でかなりの紙面を割いて説明されているが、それでも[「とにかくやってみたまえ(just go with it)」](https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html)で結ばれている(訳注: "Just Go with It"は邦題"ウソツキは結婚のはじまり"という映画のタイトルでもあります。未婚者から既婚者が結婚についてしつこく聞かれたら、まあこう答えるかもですね)。自動的なウィンドウ管理についてはこの記事の終わりで簡単に触れているものの、この記事は`display-buffer`の手綱を握る術に関する記事では**ない**。このテーマについては[Mickey Peterson氏のウィンドウマネージャーの神秘を暴くことに関する記事](https://www.masteringemacs.org/article/demystifying-emacs-window-manager)、[Protesilaos Stavrou氏によるビデオ](https://protesilaos.com/codelog/2024-02-08-emacs-window-rules-display-buffer-alist/)、興味がそそられたら[マニュアル](https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html)を参照することをお勧めする。
 
</div></details>

<!--
<details><summary>
Window configuration persistence, workspaces or buffer isolation
</summary>
<div>

> I want Emacs to group together windows for a given task and persist them across sessions!

Two common factors affecting Emacs use:

- Emacs sessions tend to be long lasting, and  

- its gravity pulls users into using it for an increasing number of tasks.  

The result is that you end up with hundreds of buffers and start looking for ways to group them, isolate the groups and then preserve them. This is tied to window management, but only in the sense that your arrangement of windows is part of the state you want to preserve. This is a finicky and complex subject, and well beyond the scope of this write-up. Take your pick: between [tab-bar](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html), [tabspaces](https://github.com/mclear-tools/tabspaces), [eyebrowse](https://depp.brause.cc/eyebrowse/), [tab-bookmark](https://github.com/minad/tab-bookmark), [desktop.el](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html), [persp-mode.el](https://github.com/Bad-ptr/persp-mode.el), [perspective](https://github.com/nex3/perspective-el), [project-tab-groups](https://github.com/fritzgrabo/project-tab-groups), [beframe](https://gitlab.com/protesilaos/beframe) and [activities.el](https://github.com/alphapapa/activities.el) there is no paucity of projects to help you do this.

</div>
</details>
-->

<details open>
<summary>
ウィンドウ構成の永続化、ワークスペースやバッファーとの分離
</summary>
<div>

> タスク用にウィンドウ同士をグループ化して永続化して、セッションを跨いで使いたいんだ!

Emacsの使用に影響を及ぼす一般的な要因として以下の2つが挙げられる:

- Emacsのセッションは長時間に及ぶ傾向があり、

- ユーザーはセッション中ますます多くのタスクをもつ

その結果として幾百ものバッファーをもつことになり、それらをグループ化して分離、保存する術を探し始めることになる。

これはウィンドウ管理と関連はあるものの、あなたが保存したい状態の一部としてウィンドウ配置があるという意味でしかない。

これは好みの分かれる複雑なテーマであり、この記事の範囲を遥かに超えている。

好きなものを選んで欲しい: [tab-bar](https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html)、[tabspaces](https://github.com/mclear-tools/tabspaces)、[eyebrowse](https://depp.brause.cc/eyebrowse/)、[tab-bookmark](https://github.com/minad/tab-bookmark)、[desktop.el](https://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Save-Mode.html)、[persp-mode.el](https://github.com/Bad-ptr/persp-mode.el)、[perspective](https://github.com/nex3/perspective-el)、[project-tab-groups](https://github.com/fritzgrabo/project-tab-groups)、[beframe](https://gitlab.com/protesilaos/beframe)、[activities.el](https://github.com/alphapapa/activities.el)など、助けとなるプロジェクトには事欠かない筈だ。

</div>
</details>

<!--
<details><summary>
Paradigmatic changes to window behavior in Emacs
</summary>
<div>

> Why is window placement in Emacs so capricious? Tiling window managers solved this problem ages ago!

Some packages provide all-encompassing, radical solutions to window arrangment and management – essentially, they are window managers for Emacs. For example, [Edwina](https://gitlab.com/ajgrf/edwina) modifies Emacs’ manual window-tree based behavior to enforce a master-and-stack [DWM-style auto-tiling layout](https://dwm.suckless.org/tutorial/), with a complete suite of accompanying window management commands. [HyControl](https://www.gnu.org/software/hyperbole/man/hyperbole.html#HyControl) provides a control panel for window layout actions and can display windows in a uniform grid on the frame, among other features.[^3]

[^3]: Apologies for the terse and possibly inaccurate descriptions, I have only brief experience with these.w

In my experience "complete" solutions like these are great when you start using them, but eventually cause more friction than elation. This is the case the more you customize Emacs, as the abstractions they build on top of the Emacs API end up limiting, as opposed to liberating you in the long run.

</div>
</details>
-->

<details open>
<summary>
Emacsのウィンドウ動作にたいするパラダイム的な変更
</summary>
<div>

> Emacsのウィンドウ配置は何故こんなに気まぐれなのか? タイル式ウィンドウマネージャーはこの問題をとうの昔に解決済みなのに!

ウィンドウの配置と管理にたいしてすべてを網羅する先進的なソリューションを提供するパッケージがある。これらのパッケージは本質的にEmacs用のウィンドウマネージャーと言えるだろう。

たとえば[Edwina](https://gitlab.com/ajgrf/edwina)はウィンドウ管理コマンドの完全な一式とともに、Emacsの手作業によるツリーベースの挙動を[DWM-style auto-tiling layout](https://dwm.suckless.org/tutorial/)のマスター/スタックベースの挙動に変更するパッケージだ。

[HyControl](https://www.gnu.org/software/hyperbole/man/hyperbole.html#HyControl)はウィンドウレイアウトアクション用のコントロールパネルを提供するとともに、フレーム上の均一なグリッド上にウィンドウを表示できる等の機能を提供する[^3]。

[^3]: 簡単で恐らくは不正確な説明をお詫びする。これらを使った経験は少ししかないのだ。

わたしの経験ではこれらの"完璧"なソリューションは使い始めこそ素晴らしいが、最終的には抵抗感が高揚感を上回った。これはEmacsをカスタマイズするほど当てはまるだろう。長期的に見るとこれらのパッケージがEmacsのAPI上に構築する抽象化によって、ウィンドウ管理から開放されるのではなく制限されてしまうからだ。

</div>
</details>

では他に何があるだろう? この記事でのウィンドウ管理とは、**手作業で行う日常的な意味においてのウィンドウ管理**を意味している。つまりウィンドウフォーカスの切り替え、ウィンドウ周りのバッファー移動、ウィンドウの分割やクローズ等々のことだ。これらの作業はたとえあなたが`display-buffer`をすべて整理してウィンドウをワークスペースにグループ化したとしても、分単位の定期的な編集の過程において頻繁に行う必要がある作業だ。

では本格的に始める前に、一般的な考え方にたいする取捨選択を行っておこう。

<details open>
<summary>
2つのウィンドウという視点
</summary>
<div>

> この長い議論は無意味だ、必要なウィンドウは多くても2つなのだから。

異議あり: 一度に必要なウィンドウが多くて2つなのだ。Emacsではウィンドウを整理して並べても目茶苦茶になってしまうということも理由の1つだ。夢中で記述やコーディングを行っている場合を除外すれば参考資料、検索、検索やコンパイルの結果、ファイルアクセス、シェルやREPL、目次等々への簡単なアクセスを要する場合があるかもしれない。それらすべてを同時にスクリーン上に表示するのか、それとも必要に応じて簡単に表示できればよいかはスクリーンサイズと好みの問題だろうが、いずれのケースでも手作業によるウィンドウやバッファーとの対話が必要となる。どちらのアプローチを採用するにしてもこれは"ウィンドウ管理"の範疇であり、この記事で取り上げるべき対象だろう。

</div>
</details>

<details open><summary>
位置選択のための処方箋
</summary>
<div>

> マウス使えば? ほとんどのソフトウェアにとってこれは話題にすらない問題だ。

確かにウィンドウを操作する手段として、マウスはもっとも自然な方法だろう。動作のエコ度、反復性ストレス障害(訳注: マウス肘、Emacs小指など)、個人的嗜好のような物議を醸す議論に踏み込まないかぎり、マウスによるアプローチの主な問題点は(キーボードと比較して)学習曲線がないことと、(キーボードと比較して)表現力の欠落の釣り合いがとれていることだろう。

たとえそうであったとしてもEmacsであれば他のほとんどのアプリケーションに比べて、[より多くの表現力をマウスから絞り出す](https://karthinks.com/software/different-strokes-for-different-folks/)ことができる筈だ[^4]。

[^4]: [ACME editor](https://en.wikipedia.org/wiki/Acme_(text_editor))は特筆すべき例外かもしれない。

わたしはウィンドウの管理にマウスを使うことがよくある。とはいえ特定のコンテキストにおいてのみではあるが。[マウスであれこれ](#マウスであれこれ-built-in)を参照して欲しい。

</div>
</details>

# ウォーミングアップ

前菜: もっとも有名で一般的に推奨されているウィンドウマネージャーの選択肢を一通り見ておこう。これらのウィンドウマネージャーのカバー範囲にはバッファー管理やお決まりのウィンドウアクション以外にも、フォーカスのあるウィンドウの変更や移動、間違いの取り消しが含まれている[^5]。

[^5]: これらのウィンドウマネージャーを何個か試した経験があれば、ここは飛ばしてしまっても大丈夫だ。[深堀り](#深堀り)へ進もう。


## other-windowと"次のウィンドウ(next window)" (built-in)

`other-window`が提供すること: ウィンドウの選択

`other-window` (`C-x o`)はウィンドウ切り替えというエクスペリエンスにたいする基準線となるコマンドだ。これこそEmacsのチュートリアルがウィンドウを切り替えるコマンドとして教えるコマンドであり、ウィンドウの数が少ないうちは十分に機能するだろう:

![other-window-chart.png](https://karthinks.com/img/other-window-chart.png)

ウィンドウは(大雑把には)フレームを時計回りに循環して選択される。このアプローチの利点は単純さだ。コマンドとキーバインディングをそれぞれ1つしか使わない。お察しの通り(あるいは経験済みかもしれないが)、ウィンドウが蓄積されるほどに目的地到着までの呼び出しも徐々に増えていくので、3つ以上のウィンドウを一度に表示することが滅多にないケースで最適に機能する。

<details open>
<summary>
other-windowの基本的な小技とチューニング
</summary>
<div>

1. もっとも使われているEmacsコマンドの1つかもしれない。`M-o`のようなもっと便利なキーにバインドしよう。

2. 前方あるいは後方、つまり反時計回りにスキップするウィンドウの数の指定には数引数を使用できる。`M-3 M-o`なら3個前、`M-- M-2 M-o`なら2つ後のウィンドウが選択されるといった具合だ。しかし残念ながらこのアプローチは、循環がどのような順序で発生するかについての視覚的な理解が要求される。ウィンドウのレイアウトが複雑になってくると、どのウィンドウが3つ先のウィンドウなのかが判りにくくなってくるだろう。

3. `repeat-mode` (`M-x repeat-mode`)をオンにしよう。そうすれば`C-x o o o o...`のように`o`だけ、後方なら`M-o o o o...`のように`O`だけでウィンドウを連続して切り替えられる。これなら`C-x o C-x o C-x o...`より余程高速だ。

</div>
</details>

<details>
<summary>
other-windowのハッキング
</summary>
<div>

ウィンドウパラメーター`no-other-window`をセットすれば、`other-window`で切り替える先となるウィンドウからそのウィンドウを除外させることができる。ウィンドウパラメーターとはEmacsのデータ構造をもつプロパティであり、それらをセットするためのElisp関数が存在する。これは手動で切り替えるのではなく、特定のバッファークラスにたいして前もってあらかじめ`display-buffer-alist`にセットして使用するパラメーターだ。`other-window`が何故(dired-sidebarやdirvish-sideのような)見た目も派手なファイルマネージャーのウィンドウを選択しないのか疑問に思ったことがないだろうか。つまりこれが答えだ。

</div>
</details>

あなたが今までEmacsで同時に2つまでしかウィンドウウィンドウ表示したことがないとか、多少キーを押す回数が増えても気にしない人ならここで読むのを終えても問題はない。なぜならこの記事の残りの部分は、あなたなら多分解決の必要を感じない(そして恐らく実際に必要ないのだろう)問題にたいする、さまざまな最適化レベルに関する話題だけだからだ!

<details open>
<summary id="org-target--window-management-next-window">
"次のウィンドウ(next window)"とは何か
</summary>
<div>

"次のウィンドウ"とは`other-window`が通常選択するウィンドウ、つまりカレントウィンドウから時計回りで次のウィンドウのことだ。これはelispから`next-window`関数を呼び出してアクセスできるウィンドウでもある。Emacsのフレームにおける時計回りのウィンドウ順は、日々の使用で自ずと身についていく。考えるというより、勘で判るという意味でだが。次のウィンドウというのは、ウィンドウ選択以外でも使用する概念なので身につけておけば役に立つだろう。ウィンドウを選択するためにはもっとよい方法も存在する。でなければ記事を書いた意味がない! 次のウィンドウ、他のウィンドウを操作する、`scroll-other-window`のようなコマンドにとってはデフォルトとなるウィンドウでもある。[ウィンドウを切り替える必要があるのか?](#ウィンドウを切り替える必要があるのか)を参照のこと。

</div>
</details>

## windmove (built-in)

`windmove`が提供すること: ウィンドウの選択、ウィンドウのバッファーの切り替え、それらの削除

Windmoveとは方向によってウィンドウを跨いでフォーカスを移動したり、バッファーを移動するためのEmacsのビルトイン関数だ。Vimユーザーにとっては期待通りに動作するだろう。`evil-mode`ユーザーなら気づいていなかっただけで、あなたはすでにWindmoveのユーザーだ。

`other-window`をEmacsのAlt-TABに例えるとしたら、Windmoveはタイル式のウィンドウマネージャーに匹敵するだろう。フレーム内のウィンドウの空間的な配置とウィンドウ選択を結びつけるのだ。これはマウスの使用を除けば、もっとも自然な方法のように思える。

Windmoveの使い方は簡単だ。`windmove-left`(および`-right`、`-up`、`-down`)を、修飾キーやプレフィックスキーと方向を表す`WASD`や`HJKL`、もしかしたら矢印キーの組み合わせにバインドすればよい。

![windmove-chart.png](https://karthinks.com/img/windmove-chart.png)

別れ道: この図での右のウィンドウへの移動は、カーソルにたいして正確に右にあるのはどのウィンドウなのかに依存する。バッファー1の上部から`windmove-right`を呼び出すとバッファー2、下部からだとバッファー3にフォーカスが移動する。

Windmoveでウィンドウのバッファーを直接入れ替えることも可能だ。これを使えばフレーム上のウィンドウを手軽に再配置できる[^6]。関連するコマンドは`windmove-swap-states-left`、`-right`、`-up`、`-down`だ。

[^6]: もう一度言おう。`evil-mode`はWindmoveを使ってこれを行っている。

![windmove-swap-chart.png](https://karthinks.com/img/windmove-swap-chart.png)

これを行うことで、バッファーと一緒にフォーカスも移動することに注意。

Windmoveにはまだまだ機能がある。たとえば`windmove-delete-*`を使えば任意の方向にある次のウィンドウを削除できるが、以下ではこれを行うもっとよい方法を説明しよう。

<details>
<summary>
タイル式マネージャーとの統合
</summary>
<div>

もしタイル式の環境下でEmacsを使用すれば、ネスト(入れ子)されたタイル式ウィンドウマネージャーという状況を手にすることになる。同じキーでEmacsのウィンドウとOSのウィンドウをシームレスに行き来できるように、2つを統合したほうが便利な場合もあるかもしれない(tmuxでVimを使っているユーザーにはお馴染みだろう)。これには多少の手間は要するものの実現は可能だ。Emacs+i3wm(おそらくSwayも可)の統合についてはPavel Korytovによる[i3 integration](https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/)のポスト、ウィンドウマネージャーについてはわたしも1つ[qtile用の設定](https://github.com/karthink/.emacs.d/blob/master/plugins/emacs-wm.el)を記述したことがある。このプロジェクトについては[詳細に述べる](#タイル式ウィンドウマネージャー統合)ことにしよう。

</div>
</details>

## frames-only-mode

[frames-only-mode](https://github.com/davidshepherd7/frames-only-mode)が提供すること: Emacsのウィンドウ処理をOSに任せる

![frames-only-mode.png](https://karthinks.com/img/frames-only-mode.png)

議論しているテーマはタイル方式についてだが、ネストされたウィンドウマネージャー、つまりタイル式ウィンドウマネージャー内部でのEmacsという状況には、ウィンドウ管理でEmacsの手を煩わせないという別次元の解も存在する。ウィンドウではなく新たなフレームですべてのバッファーをオープンして、それらのフレームの配置や整頓をウィンドウマネージャーの役目にしてしまうのだ。これによりEmacsのバッファーはOSのウィンドウと同格になり、両方を同じキーで管理できるようになる。

この記事で紹介する他のほとんどのコマンド(Avy、winum、ace-window、scroll-other-windowなど)は、フレームを跨いで使ってもウィンドウのときと同様に機能する。つまり2つのアプローチのいいとこ取りができるということだ。とはいえその他のEmacsコマンドの場合と同じように、例外は必ず存在する。そのようなコマンドの多くは、フレームを自由に分割できることを前提とするコマンドだ[^7]。

[^7]: これは特にorg-modeのコマンドが該当する。ありがたいことにゆっくりとではOrgの状況も改善されつつある。

:::note info
Linuxユーザーへ: コンポジット方式のWaylandでは、わたしは`frames-only-mode`を試したことがまだない。
:::

## winum-mode

[winum](https://github.com/deb0ch/emacs-winum)が提供すること: ウィンドウの選択と削除

ウィンドウ間の切り替え向上は`other-window`のO(n)から`windmove`のO(√n)、そしてその努力の自然な流れによってWinumのO(1)へと進化を遂げた。

![winum-chart.png](https://karthinks.com/img/winum-chart.png)

他にも役に立つボーナス機能が2つある:

- 負のプレフィックス引数とともにコマンドを呼び出すとウィンドウを削除

- ミニバッファーがアクティブなら常に数字の0を割り当てる

シンプルで短く、Emacsのフレームも跨いで機能する。わたしがウィンドウ切り替えにもっとも使用する方法が`winum-mode`だ。

<details open>
<summary>
winumによるウィンドウアクセスの高速化
</summary>
<div>

わたしの好みからするとデフォルトのキーバインディング(ウィンドウ`n`を選択する場合には`C-x w \<n>`)は、他の2ステップのキーバインディングと同様冗長すぎる。`M-0`から`M-9`による数字引数へのアクセスを失うことを気にしないのであれば、かわりにウィンドウの選択に流用するのはどうだろう:

```lisp
(defvar-keymap winum-keymap
  :doc "winum-modeアクション用のキーマップ"
  "M-0" 'winum-select-window-0-or-10
  "M-1" 'winum-select-window-1
  "M-2" 'winum-select-window-2
  "M-3" 'winum-select-window-3
  "M-4" 'winum-select-window-4
  "M-5" 'winum-select-window-5
  "M-6" 'winum-select-window-6
  "M-7" 'winum-select-window-7
  "M-8" 'winum-select-window-8
  "M-9" 'winum-select-window-9)
(require 'winum)
(winum-mode)
```
</div>
</details>

ウィンドウにたいするウィンドウの切り替えや削除以外のアクションを追加するように`winum-mode`を拡張することもできるが、次に説明するパッケージのおかげでその必要はほとんどないだろう。

## ace-window

提供すること: ウィンドウおよびバッファーの管理にたいするすべてのアクション

キーボード駆動によるEmacsのウィンドウ制御というゲームを終わらせてしまったのが[ace-window](https://github.com/abo-abo/ace-window)だ。

`ace-window`コマンドはウィンドウそれぞれの上端に"ヒント"を配置する。そのキーをタイプすればそれに応じたウィンドウにフォーカスが切り替わるのだ。

![ace-window-chart.png](https://karthinks.com/img/ace-window-chart.png)

現在のところは2ステージ版のwinumより若干遅いが、`ace-window-display-mode`をオンにすればwinumのウィンドウ番号のようにヒントが常時表示されるようになるので、このプロセスも多少スピードアップするだろう:

![ace-window-display-mode.png](https://karthinks.com/img/ace-window-display-mode.png)

Avyがスクリーンにたいして行うことを、ウィンドウにたいして行うのがace-windowだ[^8]。

しかしそのAvyにとっては、文字に応じてスクリーン上をジャンプするのは[Avyでできること](https://karthinks.com/software/avy-can-do-anything/)のうちのほんの一部に過ぎない。ace-windowもできることがウィンドウの切り替えだけだったら、それほど推奨はしなかったと思う。切り替えだけではなく、ace-windowは必要であればEmacsのすべてのフレームを跨いで、ウィンドウを"ピックアップ"する汎用的な方法を提供するのだ。そのウィンドウにたいして何を行うかは、あなたの選択に任されている。ace-windowはAvyのようにスクリーン上の任意のウィンドウに、アクションをディスパッチできるのだ。選択したウィンドウを離れることなくウィンドウの削除、移動や入れ替え、分割、バッファーの表示等々を行うことができる。これらはace-windowの一部として提供される、ただの組み込みアクションに過ぎない:

[^8]: デザインが似ているのは偶然ではない。どちらも作者は[Oleh Krehel](https://oremacs.com/)だ。

![ace-dispatch-chart.png](https://karthinks.com/img/ace-dispatch-chart.png)

ace-windowの使用時に`?`を押下すると、ディスパッチメニューが立ち上がる[^9]。

[^9]: このアイデアの詳細については、[Fifteen ways to use Embark](https://karthinks.com/software/fifteen-ways-to-use-embark/)を参照して欲しい。

![ace-dispatch-demo.mp4](https://karthinks.com/img/ace-dispatch-demo.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. ウィンドウを2つ以上オープンして`ace-window`を呼び出す(ウィンドウが2つ以下なら変数`aw-dispatch-always`を`t`すること)。

2. `?`を押下してディスパッチメニューを立ち上げる。

3. ウィンドウを水平に分割するディスパッチキーを押下する(ビデオでは`v`)。

4. 分割したいバッファーに応じたace-windowキーを押下する(ビデオでは`e`)。

5. ステップ1とステップ2を繰り返す。

6. ウィンドウを垂直に分割するディスパッチキーを押下する(ビデオでは`s`)。

7. 分割したいバッファーに応じたace-windowキーを押下する(ビデオでは`w`)。

</div>
</details>

## マウスであれこれ (built-in)

マウスが提供すること: 任意のウィンドウやバッファーへの管理アクション

さあ遂にマウスの出番だ。

ウィンドウ管理でのマウスの利点は即効性と明確さだろう。マウスの基本的な使い方を自然に延長したやり方でウィンドウを選択できるし、ウィンドウのリサイズも簡単だ。コンテキストメニュー(右クリック)とドラッグアンドドロップをサポートしている。Emacsが新しくリリースされる度にこのサポートは、非常に直感的なものへと進化してきている[^10]。しかし残念なことにEmacsユーザーはマウスの使用に関して特に偏向した持論をもつ傾向があるので、欠点の緩和について議論する前に、部屋にいるこの齧歯類(訳注: マウスには英語で鼠という意味もある)について少し話しておくべきだろう。

わたしはEmacsでは絶対にマウスを使わない。ただし他の何かですでにマウスを使っていた場合は別だ。そのようなタイミングでEmacsをマウスで操るのは、正に[もっとも抵抗感が少ない流れ](https://karthinks.com/software/different-strokes-for-different-folks/)と言える。すでに手がキーボードから離れているのなら、マウスでEmacsを操作するのはとても容易だ:

[^10]: `context-menu-mode`を調べて欲しい。話しをウィンドウ管理に限らなければ、Emacsのメニューバーを介した発見性は驚くほど良好である。

![https://karthinks.com/img/strokes-window-handling.mp4](strokes-window-handling.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

このデモでは以下の操作を、マウスのジェスチャーを用いて行う方法を紹介する:

- フレームを垂直および水平に分割

- ウィンドウの削除

- ウィンドウ内のバッファーを循環させる

- 左右のウィンドウを入れ替え

- ウィンドウに最後に表示された2つのバッファーを交互に表示

</div>
</details>

focus-follows-mouseの動作をオンにしたいと思ったら:

```lisp
;; 負の値にセットするのもあり
(setq mouse-autoselect-window t)
```

## transpose-frame (回転、フリップフロップ)

[transpose-frame](https://github.com/emacsorphanage/transpose-frame) が提供すること: ウィンドウレイアウトを簡単に変更できる

transpose-frameはフレーム上のウィンドウの回転(rotate)、反転(mirror)を行うためのコマンドを提供します、というのがこのパッケージの謳い文句だ。`rotate-frame`、`flip-frame`、`flop-frame`を適切なキーにバインドするほど、わたしはこれを頻繁に使用している。皮肉なことに`transpose-frame`コマンド自体が役に立ったことはほとんどない。これはフレームの対角線を軸にウィンドウを転置するコマンドだ。

### rotate-frame

![rotate-frame-chart.png](https://karthinks.com/img/rotate-frame-chart.png)

### flip-frame

![flop-frame-chart.png](https://karthinks.com/img/flop-frame-chart.png)

### flop-frame

![flip-frame-chart.png](https://karthinks.com/img/flip-frame-chart.png)

## window-prefix-map (built-in)

`window-prefix-map`が提供すること: 特別誂えのウィンドウ管理コマンド

`window-prefix-map`(デフォルトでは`C-x w`にバインドされている)には、役に立ついくつかのウィンドウ管理コマンドが集められている。

### split-root-window-rightとsplit-root-window-below

フレームのルートウィンドウを分割する。説明を聞くより見たほうが早い :

![emacs-window-root-frame-split.png](https://karthinks.com/img/emacs-window-root-frame-split.png)

これらはそれぞれ`C-x w 3`と`C-x w 2`にバインドされている。

<details open>
<summary>
ウィンドウツリー
</summary>
<div>

いい機会なので説明しておこう。Emacsにおいてウィンドウはツリー構造にアレンジされている。このツリー構造では"現実のウィンドウ"がleafノード、すなわち葉ノードとなる。分割アクションを行う度に葉ノードが2つのウィンドウ(分割したウィンドウと新たに分割されたウィンドウ)の親ノードになるという訳だ。これは`i3`や`bspwm`のような手動タイル式ウィンドウマネージャーによるウィンドウのアレンジによく似ているので、冗長性を改善するために[パッチを入力](#タイル式ウィンドウマネージャー統合)したほうがよいだろう。

</div>
</details>

わたしの知る限り、これらはただのEmacsの組み込みコマンドだ。これらのコマンドを使えば、(`delete-other-windows`のように)単にツリー全体をクリアーするのではなく、葉ノード以外のレベルでツリー構造を変更できる。現実的側面から考えると、フレーム内でタスクを論理的に分割するために空きを作成するなど、役に立つことが多い(デフォルトの分割コマンドは既存のウィンドウをさらに細分化するだけだ)。

ツリーの配置を理解することでよりキメの細かい制御が可能になる筈だが、それ用のツールはまだ登場していない。 [後述が提言](#xswindow-tree)を参照のこと。

### tab-window-detachとtear-off-window

ウィンドウを新たなタブやフレームに手軽に移動できるコマンド。

![emacs-window-tear-off.png](https://karthinks.com/img/emacs-window-tear-off.png)

ルートウィンドウのときと同様、これらのコマンドは論理的なウィンドウ管理に非常に役に立つ。ウィンドウを取得したら新たなタブかフレームに移動して、そこで新しいタスクを開始するといった具合だ。

これらのコマンドは、`C-x w ^ t`と`C-x w ^ f`という何ともやれやれなキーにバインドされている。かわりにace-windowのディスパッチアクションにすることもできる。ace-windowであれば何でも可能だ。デフォルトではバインドされていない、もう少しまともな`C-x w t`や`C-x w f`にバインドしてもよい。わたしはウィンドウを切り離す必要がある場合には、マウスを好んで使っている:

```lisp
;; わたしのマウスはmouse-9が"前進"のボタン
(keymap-global-set "M-\<mouse-9>" 'tear-off-window)
```

## other-window-prefix (built-in)

`other-window-prefix`が提供するのはバッファーの表示からウィンドウ選択を切り離して、ウィンドウにまつわる**3つ**のイライラの解決だ。

### イライラその１

Emacsの多くのコマンドは、バッファーとウィンドウという主要なアクションを密接に結合している。たとえば`find-file`を実行するということにはファイルの選択、バッファーの作成、そのバッファーをカレントウィンドウに表示するというアクションが含まれている。ウィンドウの選択をコマンドから切り離したい場合には`find-file-other-window`、`find-file-other-tab`、`find-file-other-frame`(それぞれ独自のキーバインドをもつ)といったいくつかの選択肢から1つをピックアップする必要がある。読み取り専用モードでファイルをオープンしたければ `find-file-read-only`、`find-file-read-only-other-window`、`find-file-read-only-other-tab`、`find-file-read-only-other-frame`があり、それらの分のキーバインドが増えた。

同様の選択をバッファーの選択にも適用したい? ならば`switch-to-buffer-⋆`という別のコマンド群も加えよう。`bookmark-jump`でブックマークをオープンする場合用には? いくつかある`bookmark-jump-*`コマンドのうちから1つ選択しよう。これは狂気への道だ。

カップリング、つまり結合されていることが問題なのだ。バッファーを表示するウィンドウの選択は、コマンドの主機能、たとえばこの例ではファイルのオープンから分離可能であるべきなのだ。この問題にたいする解決策となるのが、`other-window-prefix`だ(`C-x 4 4`にバインドされている)。

このコマンドを使えば次のコマンド(バッファーをウィンドウに表示するコマンドなら何でも)が[次のウィンドウ](#org-target--window-management-next-window)に表示される(必要ならウィンドウを作成する)。これで必要なコマンドは`find-file`、`find-file-read-only`、`switch-to-buffer`だけになった。このプレフィックスを使えば、ウィンドウでの表示を必要とするバッファーを、以下のように別のウィンドウにリダイレクトできる:

1. `other-window-prefix`(`C-x 4 4`)を呼び出す

2. `find-file`、`find-file-read-only`、`switch-to-buffer`、`bookmark-jump`、あるいはバッファを表示する別のコマンドを呼び出す

3. 結果: バッファーは次のウィンドウに表示されるだろう


この問題を解決する方法として、以前の記事で[Embarkに触れたこと](https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window)がある。

ビルトインの`other-window-prefix`よりもEmbarkの方が、この問題をエレガントに解決できるというのは嘘ではない。

しかしコマンド増殖の回避は`other-window-prefix`が解決する3つの問題のうちの1つに過ぎない。

### イライラその２

上述の例では、少なくとも`*command*`のかわりに`*command*-other-window`を呼び出すという選択肢が1つはあった。多すぎる。選択肢が1つでも存在するというのがラッキーなのだ。ほとんどの場合には選択肢など存在せず、望まぬ動作にただ翻弄されるがままだ。リンクのようなオブジェクトをアクティブにしたときなどが、典型的なケースだ。以下の例([Forge](https://github.com/magit/forge/)パッケージより)ではissueのタイトルで`RET`を押下すると、そのissueがカレントバッファーでオープンされる。

![other-window-prefix-without.mp4](https://karthinks.com/img/other-window-prefix-without.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

これはForgeパッケージで表示しているコードレポジトリのissueリストだ。


1. issueの上で`RET`を押下する

2.  カレントウィンドウでオープンされるので[リストとアイテムのパターン](#back-and-forth手法) (完全なリストと選択したissueを同時に閲覧する)は拒絶される。

</div>
</details>

この記事の執筆時点では、Forgeは別のウィンドウで"リンクをオープン"する手段を何も提供していない。`other-window-prefix`に助けてもらおう:

![other-window-prefix-with.mp4](https://karthinks.com/img/other-window-prefix-with.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. `C-x 4 4`で`other-window-prefix`を呼び出す

2. issueの上で`RET`を押下、"次のウィンドウ"でissueがオープンする(次のウィンドウがないので新たにウィンドウを作成してオープンする)

</div>
</details>

### イライラその３

このコマンドが解決してくれる3つ目の問題は、上述の1つ目と2つ目が組み合わさったパターンだ。Forgeの兄弟分であるMagitは問題を解決する手段を用意している。考えてみよう。一般的にMagitは`RET`の前にユニバーサル引数(`C-u`)を使用することによって、"リンク"を次のウィンドウオープンする。Orgモード、Notmuch、Elfeed、EWWには次のウィンドウでオープンする手段がないか、別のウィンドウでリンクをオープンするために互いに別の手段を提供したりする。もしもForgeが別の方法を何か提供していたとしたらそれは実際のところ、ある意味においては状況を悪化させていたということになっていたのではないだろうか。ところがありがたいことに`other-window-prefix`なら、アクションがどのように動作すべきかというパッケージ作者それぞれの理念にカスタマイズを加えたりあるいは迎合する必要から開放されるのだ。`other-window-prefix`を実行して、それから"リンク"オブジェクトをアクティブにしよう。お好みによってはマウスでクリックしてもよい。リンクは一貫して次のウィンドウでオープンされるようになる。

他のコマンドも参照して欲しい: `same-window-prefix` (`C-x 4 1`)はカレントウィンドウに次のコマンドのバッファー(もしあれば)の表示を強制する。`other-frame-prefix` (`C-x 5 5`)と`other-tab-prefix` (`C-x t t`)はそれぞれ次のコマンドのバッファーを新たなフレーム、あるいはタブにオープンする。

<details>
<summary>これらのキーバインディングで何ができるか?
</summary>
<div>

`C-x 4 4`、`C-x 4 1`、`C-x 5 5`のように狂気じみた見た目のキーバインディングにはそれなりの理由がある。

メニューのように特定のウィンドウアクションはプレフィックスによってグループ化されている。`C-x 4`、つまり`ctl-x-4-map`には、大まかにいうと`other-window`を使うコマンドがグループ化されている。たとえば`C-x 4 .`は(デフォルトで`M-.`が行うように)ポイント位置のオブジェクトの定義にジャンプする。ただし`other-window`でだ。`ctl-x-5-map`のほとんどは新たにフレームを作成するコマンドだ。一方`C-x t`にはタブバー関連のアクションがグループ化されている。

各マップにおいて"基本"となる最後のキーは、一貫したパターンにしたがっている。ファイルのオープンは`f`、何かを読み取り専用でオープンするなら`r`、バッファーの切り替えは`b`等々といった具合だ。そこで`C-x 4 4`、`C-x 5 5`、`C-x t t`の最後の`4`、`5`、`t`だが、これらはそれぞれ次のバッファーアクションを別のウィンドウ、新たなフレーム、新たなタブにリダイレクトするという動作を補強する意味合いであることが判るだろう。

</div>
</details>

さらに以下ではace-window(他に何があるというのだ)によって[このアイデアを極限まで推し進めて](https://karthinks.com/software/emacs-window-management-almanac/#a-window-prefix-command-for-ace-window)、次のコマンドのバッファーを任意のウィンドウ(ジャストインタイムで作成したウィンドウを含む)にリダイレクトする方法を議論しよう。

## ウィンドウ構成の保存とリストア

切れ味は鈍いものの`window-configuration-to-register`は、赤い大きなリセットボタンとしてEmacs初心者にとっては完璧だろう。どんなタイミングでもこのコマンド(デフォルトでは`C-x r w`にバインドされている)で、カレントのウィンドウ構成をレジスター[^11]に保存できる。危惧した通りにフレーム上が目茶苦茶になった後でも、`jump-to-register` (`C-x r j`)で保存した構成をリストアできる。これだけだ。


[^11]: レジスターとは多くの種類を保持できる名前付きのバケツのことだ。レジスターはそれぞれ(`a`から`z`のような)文字が割り振られていて、プレフィックス`C-x r`でレジスターの操作ができる。

<details>
<summary>セッション間でウィンドウ構成を永続化する
</summary>
<div>

`current-window-configuration`関数は`window-configuration-to-register`のelispバージョンであり、この関数のリターン値を変数にバインドして、`set-window-configuration`でそれをフレームに適用できる。lispオブジェクトのデータをディスクに永続化する`prin1`、あるいは`persist`や`multisession`のようなライブラリー組み合わせることによって、Emacsのセッション間に跨がる状態リストア機能の種とすることも可能だ。言うまでもないと思うがこれは原始的なアプローチであり、[window configuration persistence](https://karthinks.com/software/emacs-window-management-almanac/#org-target--window-configuration-persistence)に挙げた多くのパッケージリストから選んで使うほうがよいだろう。

</div>
</details>

この方法の問題の1つは、それぞれのウィンドウのカーソル位置までウィンドウ配置がリストアされることだ。これが期待通りにリストアされることは滅多にないだろう。

もう1つ別の問題として適切なタイミングでウィンドウ構成を忘れずに保存するためには、とてつもないレベルの先見性が要求される。ウィンドウ構成が変更される度に、Emacsが自動的に実行してくれれば…

## "oops"オプション

…前節からの続きだが、Emacsによる自動的な保存はもちろん可能だ。Emacsに過去のウィンドウ配置用スタックの保守させて、バッファーの変更にたいしてundoやredoを行うのと同じように、スタックを循環すればよいのだ。

Emacsを使用する目的に応じて3つのマイナーモードがある。これらはそれぞれ独立してオンに切り替えられる。

- `winner-mode`

タブを使用しない人向け。`winner-undo`、`winner-redo`をを呼び出せばそれぞれウィンドウ構成の変更をundoまたはredoする。ウィンドウ構成のヒストリーはフレームごとに個別に保守される。

- `tab-bar-history-mode`

タブを使う人向け。タブはそれぞれ独自のヒストリースタックをもつ。コマンドは`tab-bar-history-back`と`tab-bar-history-forward`。

- `undelete-frame-mode`および`tab-undo`

しょっちゅうフレームやタブを作成、削除している人向け。間違えてフレームを閉じてしまったら`undelete-frame` (`C-x 5 u`)、タブなら`tab-undo`(`C-x t u`)だ。

これらのオプションは前の配置に戻す前に選択されているウィンドウを一時的に最大化したい等の寄り道をしたい場合に役に立つだろう。

たとえばEmacsが間違った場所へのウィンドウのポップアップしたり分割したウィンドウを勝手にリサイズするなどして、手作業で慎重に配置したウィンドウが目茶苦茶にされた際の応急処置としても、`winner-mode`とその仲間たちが推奨されることがよくある。これはアンチパターンのように思う。`winner-undo`(または同等機能)を使ってEmacsの振る舞いを毎回修正している自分に気づいたのなら、そもそもEmacsが間違った場所にバッファーを表示するのが問題であって、つまりそれは苛立たしいデフォルトが原因なのだ。[whack-a-mole problem](https://karthinks.com/software/emacs-window-management-almanac/#deal-with-windows-so-we-don-t-have-to-deal-with-windows)を参照して欲しい。

# 深堀り

食欲が湧いてきたら、いよいよメインコースに取り掛かろう。ここまでで上手く機能すると判ったツールにたいする微調整、カスタマイズ、そしてバリエーションについてだ。


Emacsにイライラさせられるのには2つの段階があるように思える。最初は馴染みのないものばかり、キーバインディングと用語が判りにくい、他のソフトウェアと同じように機能するものが何もないのであなたはイライラする。パッケージのインストールによって欠点と判断した部分の修正を試みると、意味不明の暗号のようなエラーに見舞われる。シングルスレッド故に、処理速度をとんでもなく遅くするようなミスを犯すのはあまりに容易だ。ガベージコレクタは最悪のタイミングで発動する。上手くいくはずの事が上手くいかない。Emacsの欠点と思われる部分にイライラさせられる。**ウィンドウ管理がこんなに複雑であっていい訳がない!**


時を経て(数年? 数十年?)、あなたは内部で何が起こっているかについて、より改良されたメンタルモデルを育むかもしれない。Emacsのイベントループがどのように機能するか、バッファーやウィンドウ、キーマップ、テキストプロパティやオーバーレイ、すなわちEmacsを構築するデータ構造についてのメンタルモデルだ。ぎこちなく動く巨大な怪物(再表示のことだ)を密かに盗み見することさえあるかもしれない(訳注: 再表示、つまりredisplay機能はEmacsとともに配布されるデバッグ用の"etc/DEBUG"で個別に説明される程のトピックです)。 Elispの一般的なイディオムやマクロ、一般的な罠についての知見も深まった。そして遂にEmacsの実際の欠点であるAPIにイライラするのだ。**ウィンドウ管理がこんなに複雑であっていい訳がない!**

おっと、失礼。

…ということでわたしたちは今ここに集った訳だ。この記事の残りの部分は、上述した2段階のイライラの間のどこかにいる人を対象に記述した。ほとんどわたしが提案したアイデアではあるが、それらの多くは互いに排他であり、ウィンドウを処理する独自のアイデアをあなたに与えるかもしれない。これらのアイデアの実装には若干の調整が必要であり、コードをそのままコピペしてもあなたの期待する結果は得られないかもしれない。これらの理由により、Emacs初心者の人はもう少しEmacsを習得した後にここに戻ってくることをお勧めする。

## back-and-forth手法

提案: 素早いウィンドウ選択

観察: スクリーン上に同時に何個のウィンドウがあり、何個必要なのかとは関係なく、必要なのはそれらのうちの2つの間を切り替えることだけという場合がほとんどだ。**コードとREPL**の組み合わせ、**コードとgrep** (検索結果)の組み合わせ、**散文とノート**の組み合わせも例に含まれる。**リストとアイテム**パターンはプログラミングや散文に属さない例だ。これには展開された入力ウィンドウを備えたカレンダーやアジェンダのウィンドウ、電子メールの受信ボックスとオープンされた電子メールが含まれる。

これら以外のスクリーン上のウィンドウは、通常は役に立つ情報(ドキュメント、デバッグ情報、メッセージ、ログ、コマンド出力、目次、ファイルマネージャー、ドキュメントのプレビュー)を表示していて、頻繁に見ることはあっても切り替えることは滅多にないだろう。

メジャーモードは関連のある2つのウィンドウ間を行き来するために、ある程度一貫性のあるキーバインディングを提供するのが普通だ。一般的な例としてはEmacsのプログラミング用モードが用いる`C-c C-z`がある。これはコードウィンドウとREPLを切り替えるキーバインディングだ[^12]。

[^12]: これは`org-babel-switch-to-session`を介してOrg-babelブロックでも機能する。`org-babel-map`では`C-c C-v C-z`という若干異なるキーにバインドされる。

しかしこのアイデアに汎用性をもたせれば、任意のウィンドウペアー間を切り替えるコマンドを提供できるだろう:

![window-back-and-forth.png](https://karthinks.com/img/window-back-and-forth.png)

```lisp
(defun other-window-mru ()
  "フレーム上でもっとも最近使用されたウィンドウを選択"
  (interactive)
  (when-let ((mru-window
              (get-mru-window
               nil nil 'not-this-one-dummy)))
    (select-window mru-window)))

(keymap-global-set "M-o" 'other-window-mru)
```

ウィンドウ間のback-and-forth、すなわち行き来においてつ目のウィンドウを選択する方法は重要ではない。[マウス](https://karthinks.com/software/emacs-window-management-almanac/#mousing-around--built-in)、[ace-window](https://karthinks.com/software/emacs-window-management-almanac/#ace-window)、[winum](https://karthinks.com/software/emacs-window-management-almanac/#winum-mode)、あるいは他の任意の手段を使ってもよい。その後は`other-window-mru`がサポートする。

## other-windowの改善

`other-window`の基本的なアイデア(同じ循環順でフレーム上のウィンドウ間を移動)を維持しつつ、よりDWIM[^13]に合わせた順序に改善できる筈だ。

[^13]: Do-What-I-Mean

`other-window`のアイデアはシンプルだ。この記事の中ではもっともシンプルだが、どのように作業を行うかに合わせてウィンドウの選択順序を調整することができる。

### 一石二鳥

手始めにウィンドウが1つしかないときは、フレームを分割するような、ウィンドウがない場合に使用できる`other-window`コマンドを定義しよう。

```lisp
(advice-add 'other-window :before
            (defun other-window-split-if-single (&rest _)
              "ウィンドウが1つならフレームを分割"
              (when (one-window-p) (split-window-sensibly))))
```

### switchy-window

![switchy-window-order.png](https://karthinks.com/img/switchy-window-order.png)

直感で判ったかもしれないが、もう1つは時計回りという空間的な順序ではなく、Alt-TABや一部ブラウザのタブ循環のように最後に使ったウィンドウ順で循環させるための変更だ。これは努力を払えば達成可能だが`other-window`に代わる`switchy-window`を提供する[switchy-window](https://elpa.gnu.org/packages/switchy-window.html)パッケージで行うことにしよう。



ウィンドウを循環する際に、switchy-windowはウィンドウが選択されている状態を数秒間待機してから、そのウィンドウを使用済みとマークして、もっとも最近使用したウィンドウ用の最新リストを更新する。実際にこれは非常にシームレスに機能する。`switchy-window`を呼び出せば、移動する必要があったウィンドウに移動する場合がほとんどだ。

とは言ったものの、わたしが通常好んで使うのは[back-and-forth手法](https://karthinks.com/software/emacs-window-management-almanac/#the-back-and-forth-method)に記した、よりシンプルなバージョンだが。

### other-window-alternating

back-and-forthと言えば`other-window`にはもう1つバリエーションが考えられる。最初聞いたときは混乱するかもしれないが、結果的にはDWIMの好事例だということが判るだろう。`other-window`を連鎖させる場合を除き、呼び出しごとにウィンドウ切り替えの方向を反転させるのだ。ウィンドウが2つしかなければ違いは生じない。3つ以上ある場合には、たとえ循環順においてウィンドウが隣接していなくても、2つのウィンドウ間を自然に交互に行き来できるだろう。

```lisp
(defalias 'other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "毎回方向を切り替えて`other-window'を呼び出す"
        (interactive)
        (if (equal last-command 'other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))

(keymap-global-set "M-o" 'other-window-alternating)

;; repeat-modeに統合
(put 'other-window-alternating 'repeat-map 'other-window-repeat-map)
(keymap-set other-window-repeat-map "o" 'other-window-alternating)
```

## ace-windowのディスパッチによるウィンドウマジック

ウィンドウにたいして`ace-window`は`completing-read`における文字列リスト、Avyにおいてはスクリーン上の文字に匹敵する。任意のウィンドウに任意のアクションを呼び出すための3ステッププロセス、その最初の2つのステップであるフィルター、選択としては理想的だ。

![emacs-window-selection-pattern.png](https://karthinks.com/img/emacs-window-selection-pattern.png)

### Emacsウィンドウにとってのcompleting-readに匹敵するもの: aw-select

"ace-windowアクション"を定義して、`aw-dispatch-alist`[^14]にバインディングを追加するという方法により、ace-windowは拡張されるべくデザインされた。これはウィンドウを受け取り、そのウィンドウを用いて何か有用なことを行う関数だ。そのエントリーポイントとして機能するのが`ace-window`コマンドである。

[^14]: [上記の図](https://karthinks.com/software/emacs-window-management-almanac/#org-target--ace-window-default-actions)のように事前定義されたいくつかのアクションとともに配布されている。

![emacs-window-selection-via-ace.png](https://karthinks.com/img/emacs-window-selection-via-ace.png)

この制御フローは、Avyが機能する方法とほぼ同様。しかし`completing-read`の代替えとしては何か足りていない。コマンドではパターンを逆にして、`ace-window`の選択方法を使いたいのだ。都合のいいことに`aw-select`が行っているのが正にこれだ。

![emacs-window-selection-ace-inside.png](https://karthinks.com/img/emacs-window-selection-ace-inside.png)



基本的なパターンは極めてシンプル。`(aw-select nil)`[^15]呼び出しにより選択したウィンドウがリターンされる。このウィンドウをタスク用に使用できるのだ。

`scroll-other-window`がスクロールする[ウィンドウのセット](https://karthinks.com/software/emacs-window-management-almanac/#scroll-other-window--built-in)は、このようなタスクの一例だ。他にもあるがまだ試さないで欲しい! 以下ではこのアイデアをさらに汎用化していこう。

[^15]: `aw-select`の引数は選択プロセス中にモードラインにメッセージを追加するためだが、わたしたちにとっては十分ではない。

### tear-off-windowとtab-window-detach

Emacsのインタラクティブ(対話的)なウィンドウコマンドは、すべてカレントウィンドウに作用する。

ここでは[`window-prefix-map` (`C-x w`)](https://karthinks.com/software/emacs-window-management-almanac/#the-window-prefix-map--built-in)に、任意のウィンドウにインタラクティブに何かを適用できるコマンドをいくつか作成しよう。

```lisp
(defun ace-tear-off-window ()
  "ace-windowでウィンドウを選択してフレームから切り離す

これによりウィンドウは新たなフレームで表示される; `tear-off-window'を参照"
  (interactive)
  (when-let ((win (aw-select " ACE"))
             (buf (window-buffer win))
             (frame (make-frame)))
    (select-frame frame)
    (pop-to-buffer-same-window buf)
    (delete-window win)))

(defun ace-tab-window-detach ()
  "ace-windowでウィンドウを選択して新たなタブに移動"
  (interactive)
  (when-let ((win (aw-select " ACE")))
    (with-selected-window win
      (tab-window-detach))))
```

もちろんこれを行うためにアクションそれぞれにたいしてace-windowベースのコマンドを定義するのは、スケーラブルではないし役にも立たないだろう。ウィンドウ選択ステップをアクションステップから切り離して、後者に汎用性をもたせる方が望ましい。これを行うために別個に2つのアプローチを検討しよう。まずは…

### ace-window-one-command: 任意のコマンドをace-windowで


上記の汎用化の例は、`ace-window`パターンを反転させることで何がもたらされるかについて、非常に良い着眼点を与えてくれた。もっとも汎用性があり構成可能なバージョンは以下のようになるだろう:

1. ウィンドウを選択するために`aw-select`を呼び出す(`completing-read`に相当するステップ)

2. そのウィンドウで任意のアクションを実行

3. 元のウィンドウに戻るために切り替え

Emacsのイベントループをシミュレートしてこれを行うことができるが、選択されたウィンドウでウィンドウを切り替えて、元のウィンドウに戻る切り替えの前に、任意のキーシーケンスを読み取って実行する必要がある。

```lisp
(defun ace-window-one-command ()
  (interactive)
  (let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((command (key-binding
                         (read-key-sequence
                          (format "Run in %s..." (buffer-name)))))
               (this-command command))
          (call-interactively command))))))

(keymap-global-set "C-x O" 'ace-window-one-command)
```


以下のデモでは、ウィンドウ選択の前にアクションを実行している点を除けば、`ace-window`と同じように見えるだろう。ここでの`win`はアクションである。シンプルなコマンドであれば何でも機能する筈だ。`aw-dispatch-alist`にアクションを事前に設定する必要はない。セットアップや記憶しておく必要もないのだ。デモでは`ace-window-run-command`を使って、`C-x -`(`shrink-window-if-larger-than-buffer`という何とも説明的な名前の関数)で選択されていないウィンドウを縮小している。

![ace-window-one-command-demo.mp4](https://karthinks.com/img/ace-window-one-command-demo.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. 行を点滅させてどのウィンドウがアクティブなのか確認

2. `ace-window-one-action`を呼び出して左上隅のOccurバッファーを選択、何か1つコマンドを実行するまでEmacsは待機

3. `C-x -`で`shrink-window-if-larger-than-buffer`を実行してOccurバッファーを縮小、カーソル位置とウィンドウには変化なし

</div>
</details>

`ace-window-one-command`は別のウィンドウで任意のコマンドを素早く実行するのに役に立つ。このアイデアについては[以降](https://karthinks.com/software/emacs-window-management-almanac/#switch-and-return-actions-in-other-windows)で検討しよう。

<details>
<summary>
Embarkは?
</summary>
<div>

Emacs(やace-window)のアクションして選択という通常のパラダイムの逆転こそEmbarkのハートであり、わたしも[ways to use Embark](https://karthinks.com/software/fifteen-ways-to-use-embark/)という記事で触れたことがある。もちろん、この"オブジェクト·ファースト"とも言えるアプローチは1つの見え方に過ぎない。Embarkにはたくさんのハートがあるのだ。

</div>
</details>

### ace-window用のwindow-prefixコマンド

[`other-window-prefix`システム](https://karthinks.com/software/emacs-window-management-almanac/#the-other-window-prefix--built-in)は`other-window`コマンドと同様に役に立つが、同じ問題点も抱えている: 

選択するウィンドウに厳格な循環順を強制する。一貫して期待できるのは、アクティブなウィンドウが次のコマンドに渡されないことだけだ。もっと上手い方法がある筈だ。

より制御に優れた特別仕立ての解決策を与えてくれるのが`aw-select`だ。次のコマンドがウィンドウへのバッファー表示を伴うのなら、わたしたちが選択するウィンドウを使う筈だ。以下の例ではmanページを表示するウィンドウを明示的に選択している。"次のウィンドウ"の位置が望ましくないからだ:

![ace-window-prefix-demo-alt.mp4](https://karthinks.com/img/ace-window-prefix-demo-alt.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. 行を点滅させてどのウィンドウがアクティブなのか確認(左下)

2. `ace-window-dispatch` (`C-x 4 o`)、その後に`M-x man`を実行して`curl(1)`を選択、Emacsは何か1つウィンドウを選択するまで待機

3. "e"で右のウィンドウを選択、manページがそのウィンドウで表示される

elispライブラリーのManは、実際にはどこに表示すべきかをカスタマイズするための一連のオプションを提供していることに注意。このEmacsのすべての物事に共通の扱いにくい作業全体を回避したことになる。

</div>
</details>

以下の例は、たくさんのウィンドウがひしめくフレームでForgeリンクを閲覧する[上述の例](https://karthinks.com/software/emacs-window-management-almanac/#org-target--window-management-forge-video)を流用する。ランダムなウィンドウが選択される`other-window-prefix`、特定のウィンドウを選択できる`ace-window-prefix`、両者の違いを比較する。

![ace-window-prefix-demo.mp4](https://karthinks.com/img/ace-window-prefix-demo.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

このフレームでForgeのtopicウィンドウ(下のウィンドウ)からみて"次"のウィンドウは左上のウィンドウだ。

1. 最後にリストされているトピックまで移動して行点滅(アクティブなウィンドウを確認)

2. `other-window-prefix` (`C-x 4 4`) 呼び出して"リンク"の上で`RET`を押下、左上のウィンドウでオープンするがこのウィンドウは望ましくない

3. ウィンドウ構成をリストアするために`tab-bar-history-back`を呼び出す

4. 今度は`ace-window-prefix` (`C-x 4 o`)を呼び出して`RET`を押下、結果バッファーを表示するウィンドウが選択されるまでEmacsは待機

5. "r"で右のウィンドウを選択、Forgeはそのウィンドウでリンクの内容を表示する

</div>
</details>

`ace-window`は可視なフレームを跨いで使用できるので、スクリーン上の任意のEmacsウィンドウを選択できる。さらに素晴らしいのは、オンザフライでウィンドウを新たに作成して、そのウィンドウを使うこともできることだ。以下では次のコマンドが使用するウィンドウの作成に`ace-window`を使用している:

![ace-window-prefix-demo-2.mp4](https://karthinks.com/img/ace-window-prefix-demo-2.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

Orgモードのリンクをオープンすると、普通だとOrgの設定次第でカレントウィンドウか次のウィンドウでオープンされる。わたしたちは違う選択をしよう。

1. リンク上で`RET`を押下してイメージを次のウィンドウでオープン

2. `q`を押下して閉じてOrgバッファーに戻る

3. `ace-window-prefix`を呼び出してリンク上で`RET`を押下、リンクされたファイルを表示するウィンドウを選択するまでEmacsは待機

4. ウィンドウの分割には`ace-window`アクションを使用、アクションが終了するとそのウィンドウでリンクされたイメージが表示される

</div></details>

実際のところ`ace-window-prefix`の実装は`other-window-prefix`よりもシンプルだ:

```lisp
(defun ace-window-prefix ()
  "次のコマンドのバッファーの表示に`ace-window'を使用する。
次のバッファーとはこのコマンド(ミニバッファーの読み込みは除外)の
直後に呼び出される次のコマンドが表示するバッファのこと。
バッファー表示前に新たにウィンドウを作成する。
`switch-to-buffer-obey-display-actions'が非nilなら
`switch-to-buffer'コマンドもサポートする。"
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))
```

`⋆-window-prefix`用のキーバインディングパターンを踏襲して、`C-x 4 o`にバインドしよう。

```lisp
(keymap-global-set "C-x 4 o" 'ace-window-prefix)
```

# ウィンドウを切り替える必要があるのか?

ちょっと立ち止まって基本的な質問に答えて欲しい: そもそも何でウィンドウを切り替える必要が? 少々単純化して考えてみると答えは2つ、ただ2つの可能性だけに行き着く:

- **切り替えてそこに留まる**: 切り替え先のウィンドウである程度の"作業"(あらゆる形式のテキスト編集が含まれる)を永続的に行うためだろう。このイベントでは切り替え先のウィンドウが主要な作業エリアになる。

- **切り替えてその後に戻る**: そのウィンドウやウィンドウの内容にたいして短時間やり取りをするためだ。もしかしたら戻る前にスクロールしたり、何かテキストをコピーしたり、あるいはそのウィンドウを削除するかもしれない。このイベントではウィンドウは副次的な目的用の一時的な切り替え先だろう。

いずれのケースにおいてもウィンドウ切り替えはコストであって、目的ではない。理想的にはウィンドウ切り替えは編集プロセスの一環として自動的に発生するべきだろう。では何故この些細な雑用を主要なアクションである編集に"折り込んで"しまわないのだろうか?

## 切り替えて留まる: ウィンドウ切り替え機能としてのAvy


Emacsにおいてナビゲーションに関するすべての操作は最終的には[Avyへと行きつく](https://karthinks.com/software/avy-can-do-anything/)。テキストの編集(や選択)のためにウィンドウを切り替える場合には、スクリーン上の特定のポイントに移動することになる。そこにカーソルを移動するためにはウィンドウの切り替え、カーソルの正しい位置への移動という、2つのステップからなるプロセスが必要になる。このプロセスを単一のアクションに短縮するのがAvyだ。Avyはフレームをジャンプする位置からなる単一のプールとみなす。スクリーン上の任意の位置にジャンプする際には、ウィンドウを横断してシームレスな移動を行うのだ:

![avy-basic-demo.mp4](https://karthinks.com/img/avy-basic-demo.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. `avy-goto-char-timer`を呼び出す

2. "se"をタイプすると、"se"のマッチすべてにヒントが表示される(その中には"sentence"も含まれる)

3. "sentence"に相当する`g`をタイプする。

</div></details>

少し考え方を切り替えれば、少なくともナビゲーションという目的においては、ウィンドウを別個のオブジェクトと考えることを完全に止めることができるだろう。(Emacsの可視なウィンドウとフレームを飛び越えて)すべての目的に、任意の文字を数回のキー入力すれば到達できるのだ。これがウィンドウを跨いでジャンプする唯一の方法という訳ではない。たとえば`pop-global-mark`で(処理中に切り替えたウィンドウを横断して)スタート位置に戻ることができる。

![avy-jump-back-demo-2.mp4](https://karthinks.com/img/avy-jump-back-demo-2.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. `avy-goto-char-timer`を呼び出す

2. "demo"とタイプするとその文字列の候補は1つだけなので、Avyは別のウィンドウのその場所にジャンプする

3. "jumpとタイプすると、"jump"にたいするすべてのマッチにヒントが表示される。

4. マッチの1つを選択すると再びAvyはジャンプする(3つ目のウィンドウ)

5. `pop-global-mark` (`C-x C-SPC`)を呼び出して前の場所にジャンプして戻る([詳細は以下参照](https://karthinks.com/software/emacs-window-management-almanac/#org-target--pop-global-mark-advice))

6. `pop-global-mark` (`C-x C-SPC`)をもう一度呼び出して前の場所にジャンプして戻る。

</div>
</details>

<details>
<summary>
Avyがウィンドウを認識しない
</summary>
<div>

Avyがウィンドウやフレームを跨いで移動しない場合には、多分`avy-all-windows`をカスタマイズする必要がある。ついでに`avy-style`のカスタマイズも検討して欲しい。Avyでジャンプする方法は1つではないのだから!

</div>
</details>

もちろんこれはAvyでできることの表面を少し引っ掻いただけだが、この時点における舗装としては十分ではないだろうか。

## 切り替えて戻る: 別のウィンドウでのアクション

では別のケースだ。単一の論理的アクションを行うという理由でウィンドウを切り替えるのは珍しいことではない。メインバッファーに戻る前に、何かを見るためにisearchのようなアクションでフォーカスの移動する等の複合アクションかもしれない。これこそ切り替え→アクション→切り替えて戻るパターンのダンスだ。

わたしたちはこのダンスを自明かつ具体的な解決策から、反復可能で一般的な解決策、さらに最終的には抽象化された汎用性のある解決策へと段階的に自動化していこう。

まず最初は明白な点から: このダンスを繰り返し行っている自分に気づいたなら、キーボードマクロで自動化することが可能だ(読者の練習用に詳細は省く)。そのアクションがいつもあなたが行っているアクションである場合には、一歩進めて汎用目的のコマンドを記述できるだろう。上述した`ace-window-one-command`はこれを行う1つの方法だ。Emacsがわたしたちのために切り拓いてくれた道は…

## scroll-other-window (built-in)

`scroll-other-window`と`scroll-other-window-down`が古くからEmacsの一部である理由は、Emacsのデフォルトセッティングである2-ウィンドウパラダイムに正しく準拠しているからだろう。あるウィンドウで編集する際にもう一方のウィンドウの内容をリファレンスとして使用するパラダイムのことだ。編集を行うウィンドウを離れずに、もう一方を上下にスクロールできる。これは任意の個数のウィンドウで機能することに注意。スクロールされるウィンドウは"[次のウィンドウ](https://karthinks.com/software/emacs-window-management-almanac/#other-window-and-the-next-window--built-in)"、すなわちカレントウィンドウから時計回りで次のウィンドウだ。以下の図では内枠線のあるウィンドウが選択されたウィンドウ、`scroll-other-window`がスクロールするウィンドウには矢印がついている:

![scroll-other-window.png](https://karthinks.com/img/scroll-other-window.png)

3つ以上ウィンドウがある場合に期待通り動作させるためのウィンドウ配置には、配慮が求められる。たとえば横並びの3つのバッファー(上図の1か3)では、2で`scroll-other-window`すると3がスクロールされるので、1を参照しながら2と3で作業することはできない。しかしありがたいことに、わたしたちはスクロールするウィンドウを選択するルールを指定できるのだ。1つ目のオプションとしては

```lisp
(setq other-window-scroll-default #'get-lru-window)
```

これは常に最近もっとも使用されていないウィンドウをスクロールする。リファレンスとして使用する1に頻繁に移動することはないだろうというのが理由だ。バッファー2と3を切り替えて使っているときにはバッファー1を無視して、`scroll-other-window`でもう一方をスクロールしたいと思うかもしれない:

```lisp
(setq other-window-scroll-default
      (lambda ()
        (or (get-mru-window nil nil 'not-this-one-dummy)
            (next-window)               ;fall back to next window
            (next-window nil nil 'visible))))
```

これは[back-and-forth手法](https://karthinks.com/software/emacs-window-management-almanac/#the-back-and-forth-method)において極めて良好に機能する。

<details>
<summary>
スクロールするウィンドウを設定する
</summary>
<div>

別の方法でもスクロールするウィンドウを変更することができる。変数`other-window-scroll-buffer`をセットすれば、次のウィンドウのかわりにスクロールすべきバッファーのウィンドウを指定できるのだ。ただしこれは主にパッケージ作者用のオプションである。これをオンザフライで行うには、以下のように別のelispコマンドを記述する必要があるだろう

```lisp
(defun ace-set-other-window ()
  "ace-windowでウィンドウを選択して、
カレントウィンドウの"別のウィンドウ"
としてセットする"
  (when-let* ((win (aw-select " ACE"))
              (buf (window-buffer buf)))
    (setq-local other-window-scroll-buffer buf)))
```

これが役に立つのは、この関連付けを永続化したい場合だけだろう。そうでないほとんどのケースにおいては、LRU/MRU手法こそあなたが必要とする手法だ。以下の[master-mode](https://karthinks.com/software/emacs-window-management-almanac/#master-mode-and-scroll-all-mode)も参照して欲しい。
</div></details>

<details open>
<summary>
別のウィンドウをスクロール: 詳細
</summary>
<div>

1. `scroll-other-window`のデフォルトバインディング(`C-M-v`と`C-M-S-v`)の実行頻度は、あなたの修飾キーにたいする寛容さにもよるだろう。特にモーダルな入力メソッドを使用している場合には、リマップ候補として有力だと思われる。元々`C-M-v`は`ESC C-v`でも呼び出せるので、わたしはもう1つを`ESC M-v`にバインドしている。

2. `scroll-other-window`はミニバッファーからも機能する。スクロールされるウィンドウは通常だとミニバッファーを使用するコマンドから呼び出されたウィンドウであり、`minibuffer-scroll-window`の値で明示的にセットできる。

3. Emacs 29以降の`scroll-other-window`はスクロールを特別な関数で処理することにより、PDFのような非テキストのバッファーの扱いに優れている。標準のスクロールコマンド(`scroll-up-command`と`scroll-down-command`)に何がバインドされていようと、それを呼び出すようになったのである。"次のウィンドウ"の位置にあるPDFバッファーをpdf-toolsパッケージが管理していて、そのバッファーをスクロールしたければ:

```lisp
(with-eval-after-load 'pdf-tools
     (keymap-set pdf-view-mode-map "\<remap> \<scroll-up-command>"
                 #'pdf-view-scroll-up-or-next-page)
     (keymap-set pdf-view-mode-map "\<remap> \<scroll-down-command>"
                 #'pdf-view-scroll-down-or-previous-page))
```

別の例として`pixel-scroll-precision-mode`を通じて通常のページングコマンドをリバインドすれば、`scroll-other-window`に別のウィンドウをスムーズにスクロールさせることができる:

![scroll-other-window-precision-demo.mp4](https://karthinks.com/img/scroll-other-window-precision-demo.mp4)

</div>
</details>

## isearch-other-window


リファレンスとして別のウィンドウでバッファーを表示するというアイデアを推し進めれば、`scroll-other-window`を直接拡張して"次のウィンドウ"を検索させるという考えに行き着くだろう[^16]。上述の`scroll-other-window`でスクロールするよう設定したウィンドウと同じウィンドウで検索すればよいことになる。

[^16]: `isearch`は素晴らしいナビゲーションツールだ。

```lisp
(defun isearch-other-window (regexp-p)
    "次のウィンドウでisearch-forwardする関数

プレフィックス引数REGEXP-Pを指定すると正規表現検索
    (interactive "P")
    (unless (one-window-p)
      (with-selected-window (other-window-for-scrolling)
        (isearch-forward regexp-p))))

(keymap-global-set "C-M-s" #'isearch-other-window)
```

`other-window-for-scrolling`はわたしたちが選択した上述の`other-window-scroll-default`にしたがい、適切なウィンドウをリターンする関数だ。

以下はシェルとドキュメントでの`isearch-other-window`の動作例だ:

![isearch-other-window-demo.mp4](https://karthinks.com/img/isearch-other-window-demo.mp4)

<details>
<summary>
ビデオ実況
</summary>
<div>

1. Curlコマンドの一部をタイプする

2. Manバッファーを検索するために`isearch-other-window` (ここでは`C-M-s`)を呼び出す

3. わたしたちが調べたいオプション`--ssl revoke`を検索する(このマッチングの特殊な振る舞いは`isearch-whitespace-regexp`の設定によるもの)

4. `RET`を押下するとisearchは終了してシェルに戻る

5. `scroll-other-window`で別のウィンドウをスクロールしたら、`hippie-expand`を使って入力したい引数をタイプ入力する

</div>
</details>

`C-M-s`というキーはすでに`isearch-forward-regexp`にバインド済みだが、このコマンドを呼び出す方法は他にもたくさんある。`isearch-forward`にプレフィックス引数を与える(`C-u C-s`)、あるいはisearchの途中で`M-r`でregexpに切り替える等だ。

<details>
<summary>
他のウィンドウでのアクションの実行
</summary>
<div>

elispから別のウィンドウに一時的に切り替える簡単な方法は2つある。`(save-window-excursion (select-window somewin) ...)`と`(with-selected-window somewin ...)`だ。

わたしたちの目的に照らして比較しよう。前者は実行時のウィンドウ構成をリストアすることだ。これにはウィンドウからの相対的なバッファー位置やそのバッファーの`(point)`の値が含まれる。一方後者はフレーム全体にわたって変更を維持する。こちらがわたしたちの求めていたものだ。変更が永続的でなければ、この演習の意味がないではないか!

</div></details>

## 次のウィンドウでのバッファーの切り替え

Emacsではバッファーなら幾らでももてるが、ウィンドウの方は一握りだけだ。実際のところ、これがウィンドウ管理における問題の元なのである。したがって包括的な解決策にはすべて、既存のウィンドウに表示されているバッファーを変更する必要がついてまわるのだ。ace-windowの[ディスパッチシステム](https://karthinks.com/software/emacs-window-management-almanac/#ace-window)は1つの解決策だろう。しかし別のウィンドウに表示されているバッファーの変更という、より容易な80点の別回答を提案するのが、ビルトインのコマンド`next-buffer`と`previous-buffer`だ。これを行うための専用の`next-buffer-other-window`コマンドは必要ない。`next-buffer`を新たな関数に置き換えるだけで済む。

```lisp
(defun my/next-buffer (&optional arg)
  "次のARG番目のバッファーに切り替える

ユニバーサルプレフィックス引数を与えると次のウィンドウで実行する"
  (interactive "P")
  (if-let (((equal arg '(4)))
           (win (other-window-for-scrolling)))
      (with-selected-window win
        (next-buffer)
        (setq prefix-arg current-prefix-arg))
    (next-buffer arg)))

(defun my/previous-buffer (&optional arg)
  "前のARG番目のバッファーに切り替える

ユニバーサルプレフィックス引数を与えると次のウィンドウで実行する"
  (interactive "P")
  (if-let (((equal arg '(4)))
           (win (other-window-for-scrolling)))
      (with-selected-window win
        (previous-buffer)
        (setq prefix-arg current-prefix-arg))
    (previous-buffer arg)))
```

そして`next-buffer`と`previous-buffer`を置き換えればよい。

```lisp
(keymap-global-set "<remap> <next-buffer>"     'my/next-buffer)
(keymap-global-set "<remap> <previous-buffer>" 'my/previous-buffer)
```

そして最後は`switch-to-buffer`のフォールバック版を定義して、これらすべてをrepeat-mapに放り込んでしまえば`n`、`p`、`b`で連続して呼び出せるだろう:

```lisp
;; switch-to-buffer、ただしもしかしたら次のウィンドウで
(defun my/switch-buffer (&optional arg)
  (interactive "P")
  (run-at-time
   0 nil
   (lambda (&optional arg)
     (if-let (((equal arg '(4)))
              (win (other-window-for-scrolling)))
         (with-selected-window win
           (switch-to-buffer
            (read-buffer-to-switch
             (format "Switch to buffer (%S)" win))))
       (call-interactively #'switch-to-buffer)))
   arg))

(defvar-keymap buffer-cycle-map
  :doc "バッファーを循環させるための`repeat-mode'にかわるキーマップ"
  :repeat t
  "n" 'my/next-buffer
  "p" 'my/previous-buffer
  "b" 'my/switch-buffer)
```

キーマップを頑張って、右上にキー説明も表示されるようにした。

![next-buffer-with-repeat.mp4](https://karthinks.com/img/next-buffer-with-repeat.mp4)

<details open>
<summary>
ビデオ実況
</summary>
<div>

- `my/next-buffer`か`my/previous-buffer`を呼び出す(わたしは`next-buffer`のデフォルトのバインディング`C-x \<right>`はリマップせずに、`C-x C-n`と`C-x C-p`にバインドした)

- repeat-mapの`buffer-cycle-map`がアクティブになるので`n`と`p`でバッファーを循環させ続けられる

- 他のキーを押下してrepeat-mapから抜け出す

- プレフィックス引数とともに`my/next-buffer`(`C-u C-x C-n`)を呼び出すと別ウィンドウで`buffer-cycle-map`がアクティブになり、`n`と`p`で別ウィンドウのバッファーを循環できる

- repeat-mapアクティブ時に`b`を押下すると選択されたウィンドウで`switch-to-buffer`が呼び出されるが、これは必要なウィンドウがバッファー履歴の1、2個先になかったときのフォールバックだ

</div></details>

別ウィンドウでのバッファー表示に`b`を用いているのは、`ace-window`のディスパッチ版の動作と整合をとるためだ。

## master-modeとscroll-all-mode

少し脱線 : Emacsは`master-mode`を提供している。ウィンドウを離れずに別のウィンドウでアクション行うための特別誂えの解決策である。あるバッファーをカレントバッファー("master")の"slave"バッファーに指定できるのだ。このモードはカレントウィンドウを離れずにslaveバッファーをスクロールするためのキーマップをオープンする。上述した`other-window-scroll-default`の方が透明性に勝り、即効性のある解決策なので、このモード自体は候補としては劣っている。しかしコマンド`master-says`でこのキーマップを追加できる。これは伝声管のようなコマンドで、slaveバッファーで事前定義したアクションを行う助けになるだろう。たとえば以下はslaveバッファーを再センタリングするビルトインアクションだ:

```lisp
(defun master-says-recenter (&optional arg)
  "slaveバッファーを再センタリングする
`recenter'を参照のこと"
  (interactive)
  (master-says 'recenter arg))
```

しかしこれはどんなアクションでも構わないのだ。すべてのプロジェクトにたいしてシェルバッファーやコンパイルバッファーをslaveバッファーにして`master-mode`を使ってページ操作や最後の出力のコピー、コマンド送信等々を行うことができる。

スクロールに焦点を当てて考えると、`scroll-all-mode`ならフレーム上のすべてのウィンドウのスクロールアクションをで結びつけることができる。ときには2つ以上のウィンドウのビューの同期を保たせていたい場合もあるだろう。アクティブなウィンドウをスクロールしてから別のウィンドウをスクロールするより、こちらのモードの方が手軽ではないだろうか。

## with-other-window: elispヘルパー

切り替え→アクション→切り替えというバックダンサーを自動化する汎用目的のコマンドを記述するより優れた方法はないだろうか? 汎用目的コマンドを自動的に記述する汎用目的のマクロだ! マクロを使って切り替えからアクションを切り離してみよう:

```lisp
(defmacro with-other-window (&rest body)
  "other-windowでBODYのフォームを実行する"
  `(unless (one-window-p)
    (with-selected-window (other-window-for-scrolling)
      ,@body)))
```

上記の例をそのままマクロによるアプリケーションにしてしまう

```lisp
(defun isearch-other-window (regexp-p)
  (interactive "P")
  (with-other-window (isearch-forward regexp-p)))

(defun isearch-other-window-backwards (regexp-p)
  (interactive "P")
  (with-other-window (isearch-backward regexp-p)))
```

これはインタラクティブな`ace-window-one-command`にたいするelispの対等物といってよいだろう。

# たくさんのウィンドウは必要か?

エディターから見た世界は、まるで単一のUIに収束しつつあるかのようだ。メインウィンドウが1つ、上端にタブバー(そしてタブごとにウィンドウ)、左側のサイドバーにはディレクトリーやコンテンツ、右側にはオプションの安っぽいお飾り、下端には端末エミュレータ。

![modern-editor-layouts.jpeg](https://karthinks.com/img/modern-editor-layouts.jpeg)

すべてのエディターが回覧を受け取ったな… だがEmacs…お前は別だ、みたいだね。このウィンドウレイアウトとワークフローをEmacsで再現することは可能だ。ところで他のものでも何でも構わないが、怒り狂うがごとくウィンドウ管理に励む我々にこそ、より基本的な疑問を問う必要がある: *何で2つ以上のウィンドウが必要なの?*

この考え方には幾らかのメリットがある。ウィンドウ切り替えではなくバッファーを切り替えれば、1つのバッファーにスクリーンを使用できる。ウィンドウのリサイズ、調べものや通常の編集でポップアップする(ドキュメントウィンドウのような)すべてのものは、通常は`q`を押下すれば閉じることができるだろう。ファイルブラウザのような特別なバッファーには、`dired-jump`のような専用コマンド経由でアクセスできる。

同時に2つのウィンドウという要件を緩和して、2つ目のウィンドウを生きたリファレンスとして運用するのだ。そうすれば手間のかからない操作のほとんどはそのまま残したまま、増えた利点だけを享受できるだろう。その証拠が`scroll-other-window`や他のコマンドだ。Emacsはデフォルトでこのような使い方をするように元々セットアップされているのである。Emacsには上からのお達しで押し付けられた厳格なレイアウトは存在しない。そちらであれば無秩序なカオス、雑草のごとくポップアップするウィンドウも存在しないだろうが。

そして実のところ、はわたしたちは1周回って[The Zen of Buffer Display](https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html)に戻ったことになる。たしたちが拒絶する原因は、Emacsがわたしたちにウィンドウ管理の自由を与えたせいだというのは何とも皮肉な話しだ。スクリーン上に同時に何個のウィンドウをもちたいとか気にせず、ウィンドウを何とかしようとする試みすべてを止めて問題を回避する手もある。

そこでウィンドウ管理についてさらに2つのストラテジーを紹介しよう。どちらもウィンドウの取り扱いが最小限のストラテジーだ。1つ目はもっとも緩い基準としての"ウィンドウ管理"を行う方法について説明しよう:

## ウィンドウが作られたら無視する

[Window-agnostic jumping with Avy](https://karthinks.com/software/emacs-window-management-almanac/#switch-and-stay-avy-as-a-window-switcher)は汎用的なアイデアにおける特殊ケースだ。Emacsを使う際にわたしたちが主に関心をもつのはテキストである。テキストコンテナとして考えたとき、ウィンドウは不要な抽象化なのかもしれない。これが`xref-find-definitions`による定義へのジャンプのように、目標がスクリーンコンテンツ外部にあれば自然な枠組みと言えるだろう。


しかし他にもこのウィンドウ不価値論を適用する方法がいくつか存在する。ジャンプ前の場所を追跡する`mark-ring`と`global-mark-ring`では、それぞれ`pop-to-mark-command` (`C-u C-SPC`)と`pop-global-mark` (`C-x C-SPC`)で元の場所にジャンプして戻ることができる。後者の方は必要に応じてウィンドウを跨いでジャンプしてくれるだろう。[dogears](https://github.com/alphapapa/dogears.el)のようなパッケージなら、優れたUIでステップの再トレースをよりきめ細かく制御してくれる筈だ。

<details>

<summary>pop-to-bufferでウィンドウを跨いでジャンプする</summary>

<div>

`pop-global-mark`はデフォルトでは常にカレントウィンドウのバッファーを切り替える。これをウィンドウ切り替えにも使いたいものだが、それには若干のadviceが必要だ:

```lisp
(define-advice pop-global-mark (:around (pgm) use-display-buffer)
  "`display-buffer'で`pop-to-buffer'にバッファーをジャンプさせる"
  (cl-letf (((symbol-function 'switch-to-buffer)
                         #'pop-to-buffer))
                (funcall pgm)))
```

</div></details>

後で戻る場所に手作業で目印をセットするためには、`point-to-register` (`C-x r SPC`)と`jump-to-register` (`C-x r j`)がある。前に触れたと思うが、これにはウィンドウが切り替えるという副次的な効果がある。


これらの方法の中間にも、ユーザーやEmacsにとって意味をもつ場所にウィンドウを跨いで移動するためのオプションがEmacsにはたくさん存在する。これらのオプションはEmacsウィンドウの単一ウィンドウにおいても、21世紀的なレイアウトをもつ標準的なIDEと同じことを行うことができるのだ。

## ウィンドウの世話をしなくてよいようにウィンドウを取り扱う

*解*: モグラ叩き問題を何とかする

これはEmacsウィンドウにまつわる手作業のアクションに関する記事なので通り過ごす訳にはいかない。`display-buffer-alist`とウィンドウの自動的な振る舞いについては記事のどこかで触れておく必要があるだろう。アイデアはシンプルだ。elispのコードがバッファーを表示しようとする度に、この変数のルールリスト照らしてバッファーが表示される。このリストに一致したバッファーのエントリーには、バッファーが如何に表示されるべきかが指定されている。

日常的なEmacsの使用において目にするすべての種類のバッファーにたいしてウィンドウのサイズ、位置、役割り、フォーカスといったルールをセットアップすればウィンドウ管理のほとんどは片付くだろうが…おわかり頂けただろうか?

そう正に今、わたしたちの強いあこがれに現実が忍び寄ってきた。`display-buffer-alist`の問題は機能しないことではないのだ。あまりに多くの作業を要するのが問題なのだ。バッファーを表示するルールを作成するには、ほとんどのユーザーにとっては十分な理解のレベルを遥かに超えるバッファー、さらに多くの述語、ウィンドウタイプやスロット、display-bufferアクション関数、ウィンドウパラメーター、ちんぷんかんぷんなたくさんの用語といったEmacs APIに大量に存在するAPIの側面にたいする理解が要求されるだろう。そしてelispマニュアルの探窟を終えた今となっては、"わたしのウィンドウ配置を乱さないでください"のようにシンプルな意図を容易に表す術はもはや残されていない[^17]。これはパッケージの作者がウィンドウの自動的な振る舞いを指定することで、より親しみやすいインターフェイスを自分のパッケージに提供することを主目的としたツールなのである。

[^17]: display-bufferを過度に設定したりオーバーライドすることは可能だが、数十に及ぶ境界条件や予期せぬ挙動がもたらされるだろう。

しかし年代記の精神に則れば、このトピックを手ぶらで去る訳にはいかない。

- [Shackle](https://depp.brause.cc/shackle/)は`display-buffer-alist`の奇妙な点を克服して、シンプルなウィンドウルールを指定するためのelispインターフェイスを提供するパッケージだ。いつもウィンドウ配置を台無しにして、あなたが`winner-undo`に手を伸ばす原因となっている厄介なバッファータイプは、柵で囲ってしまうのが最善手だ

- Emacsのディストリビューションには通常は、これらの設定を指定するためのインターフェイスを提供している。いずれかを使っているならサポートされているだろう[^18]。

[^18]: Doom Emacsはこれを行うために`set-popup-rule!`という便利なコマンドを提供している。

- 色んな部分を弄ったりハッキングすることに興味があるようなら、冒頭触れたように [Mickey Peterson氏の記事](https://www.masteringemacs.org/article/demystifying-emacs-window-manager)、[Protesilaos Stavrou氏のビデオ](https://protesilaos.com/codelog/2024-02-08-emacs-window-rules-display-buffer-alist/)、[the elisp  manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html)はすべて、ウィンドウ管理の神秘を解き明かすための優れたリソースだ。

## Popper、Popwin、shell-pop、vterm-toggle

ウィンドウが散らかっていない最小限のワークスペースという理想を目指す我々ではあるが、他にも役に立つツールとしてポップアップマネージャーがある。

すべてのバッファーが同じように作成される訳ではないという観察に基づくEmacsパッケージが[Popwin](https://github.com/emacsorphanage/popwin)と[Popper](https://github.com/karthink/popper)だ。バッファーにはわたしたちが時間のほとんどを過ごすバッファー(プライマリーバッファー)があり、リファレンスとして用いたりドキュメントの参照、シェルコマンドの実行、タスクやコンパイルの状態チェック、検索結果へのアクセス、メッセージの読み取り等々で一時的にアクセスしたいバッファー(ポップアップバッファー)が存在するのだ。`display-buffer-alist`や同等の手法を使うことで、これらのバッファーにもっと小さく補助的なウィンドウを使わせて、表示時にカーソルを奪わないようにすることができる。しかしアクセス問題の解決にはならない。わたしたちが求めているのはこれらのポップアップバッファーをキー1つで召喚して閉じたり、循環させたり、強制的に閉じる簡単な方法なのだ。

Popperはどんな種類のバッファーにたいしても、あなたがポップアップさせたいバッファーを選択(事前に選ぶことも可能)すると、必要に応じてこれらの補助的なウィンドウのオープンやクローズを行うことで、1つ(あるいは2つ)のウィンドウというパラダイムにたいするあなたの強迫観念を支援してくれる。以下のイメージは、その時点で利用可能なポップアップをタブラインとして表示しているところ。これらのポップアップへのアクセスや循環は1つのキーで行うことができる:

![popper-tab-line-demo.png](https://karthinks.com/img/popper-tab-line-demo.png)

このアイデアより古く、より包括的に実装されているのがPopwinだ。アクセス用のクイックキー、独自にカスタムメイドされたdisplay-buffer構成(あなたの好みにはそぐわないかもしれない)が付属している。キーアクセス1回でシェルバッファーの召喚や終了を行いたい場合には、必要なのは[shell-pop](https://github.com/kyagi/shell-pop-el)や[vterm-toggle](https://github.com/jixiuf/vterm-toggle)かもしれない。

# 未解決事項

最後は存在して然るべきウィンドウ管理用のオプション話題で締めよう… 存在はしないが。

## window-tree

Emacsによるウィンドウの表現方法と、わたしたちがこれまで議論してきたアプローチによるウィンドウ操作の間には、根本的とも言える断絶が存在する。

Emacsのフレームにおけるウィンドウはツリーとして表されている。leafノード、すなわち葉ノードは"生きた(実際の)"ウィンドウ、残りは"内部的(仮想的)"なinternalノードだ[^19]。


[^19]: 技術的にはミニバッファーはこのツリーに属さないものの、このツリーを辿って到達することは可能だ(`window-tree`関数を参照のこと)。

![emacs-window-tree-illustration.png](https://karthinks.com/img/emacs-window-tree-illustration.png)

ウィンドウ操作においてほとんどのユーザーが直面する`other-window`やWindmoveのような機能によるウィンドウ間の移動は、ウィンドウの空間的な位置を調べることによって機能するので、このツリー構造は無視される。これはウィンドウの分割や削除の際に予期せぬ直感に反する振る舞いを引き起こしたり、作成可能な分割にたいして意味不明な制約が課せられることが多々ある。たとえば以下のような変換を行う手段は存在しない

![emacs-window-tree-deficit.png](https://karthinks.com/img/emacs-window-tree-deficit.png)

ここで分割する必要があるウィンドウはフレームのルートではないし、leafウィンドウでもない。ツリーの中のinternalノードのうちのどれかだ。

window-treeの操作用にコマンドを追加することによって、新たな可能性に通じるたくさんのドアが開くだろう。分割、入れ替え、反転等々のフレーム変換は、window-tree上の枝にたいする初歩的な操作と言える。複数ウィンドウの選択はinternalウィンドウを"選択"すれば可能だ。そうすればウィンドウ構成の部分的な操作によって他のタブやフレームへの引き渡し、複製や永続化も可能になるだろう。ツリーの枝が`display-buffer`や同類の関数[^20]によって目茶苦茶にされることが防がれるし、柔軟で制約が緩い動作を許容することで、フレームの一部を1つのタスク専用にすることもできるのだ。

[^20]: この全か無かの如きウィンドウ動作は、現在のところElispのatomicウィンドウAPI経由で有効になってはいるものの非常に制約のあるアプローチだ。

この仮説に過ぎないwintree、あなたが記述してみるというのはどうだろう?

1. Elispはウィンドウツリーを問い合わせる関数をすでに提供済みだ: ツリー自体をリターンするのは`window-tree`、`frame-root-window`はツリーのルート、`window-parent`、`window-child`、`window-*-sibling`はあなたが期待する通りの動作をするだろう

2. `walk-window-tree`と`walk-windows`を通じたツリーの横断がサポートされている

3. 通常の方法で生きたウィンドウの分割や削除を行う方法以外は、ツリーを変更するための初歩的な関数が存在しない。

4. internalウィンドウの"選択"という概念がないので、おそらくはサブツリー(部分木)のウィンドウそれぞれの内側に、UIを通じて[ボーダーを追加](https://github.com/captainflasmr/selected-window-accent-mode)してシミュレートする必要があるだろう。

こうして必要な要素は提示された。あと不足している要素は"window.el"に足を踏み入れて自らの手を汚してやろうという、やる気に満ちたEmacsユーザー(あなたかも)だけだ!

## タイル式ウィンドウマネージャー統合

Emacsのウィンドウツリーモデルは、i3やbspwmのような手動のタイル式ウィンドウマネージャーとほぼ等しいが、i3のタブ付きウィンドウ環境が及ぼす力のようなものに欠けている。これは次のような自然な疑問へとつながる。何故にタイル式ウィンドウマネージャーの中でタイル式マネージャーを使うのか?[^21] あなたがi3やbspwm、あるいはEmacs内部でtmuxを使っているなら、同じキーバインディングで両者をシームレスに操作したいと思うのは自然だ。これを行うためのEmacsパッケージはいくつか存在する。Pavel Korytovの[i3-integration](https://sqrtminusone.xyz/posts/2021-10-04-emacs-i3/)があるし、わたしも [qtile](https://github.com/karthink/.emacs.d/blob/master/plugins/emacs-wm.el)用にハックしたことがある。しかしこれを行うためにより明解で統一されたインターフェイスをEmacsが提供できれば、すべてのウィンドウマネージャーの統合が遥かに容易になり得る。繰り返そう。必要な要素は既に提示されている:

[^21]: よう、タイル式が好きらしいな…

1. ウィンドウマネージャーはアクティブなウィルスクラスを識別する何らかの方法、ウィンドウ間を跨いだ移動したりウィンドウを扱うプログラム的な手段をを提供する必要がある。これはシェルコマンド、socketまたはサーバーベースのIPC、(Linuxなら)D-Busメソッドを通じて行うことができるだろう。ほとんどのウィンドウマネージャー、端末多重化アプリを網羅できる筈だ

2. Emacs側では通信手法に依存しない、ほとんどのウィンドウマネージャーのウィンドウ操作を模倣した操作の共通サブセット、あるいは(より野心的には)それらを統合した操作をサポートするインターフェイスが必要だ。

3. ウィンドウマネージャーでウィンドウが切り替わったらアクティブなウィンドウがEmacsかチェックしいぇ、必要に応じてウィンドウマネージャーに制御を渡す。

これも前と同じ。不足している要素はあなただ!

# ここからの眺め

信じる信じないは自由だが、これが短いバージョンだ。記事の範囲を抑えるために`tab-line-mode`関連すべて、atomicウィンドウや専用ウィンドウ(dedicated window)、サイドウィンドウといったいくつかのウィンドウストラテジーは除外せざるを得なかった。そしてわたしたち全員にとって、`display-buffer`にまつわる問題は避けておく方が無難だ。


わたしたちは何処に辿り着いたのだろう? 手に残されたのはウィンドウの切り替え、移動、飛び回り方、作成、削除、さもなくばウィンドウの取り扱いやウィンドウ構成に関する数十の方法、コマンド呼び出しでオンザフライでウィンドウ表現を制御する多くの手法、ウィンドウを跨いで作業する半ダースの方法、さらにはウィンドウ管理について完全に思考を停止することについてだ。繰り返しておくがこれはわたしがEmacsでウィンドウに悪戦苦闘してきた経験によって脚色、制限されたコレクションである。もっとシンプルだったり役に立つものを見逃していたら、是非教えて欲しい!

Emacsにおけるウィンドウ管理は良くも悪しくも、それほど複雑ではなく制限もない。素材といくつかの手順はEmacsによって提供されており、その素材自体も基本的な食事として耐え得るものだ。

でも少し調理すれば美味しい何かができるかもしれない。Bon appétit(さあ、たんとお食べ)

<details open>
<summary>
更新と修正
<div>

修正、提案をしてくださった以下の方々に感謝いたします:

- winner-modeがEmacs フレームごとにウィンドウ構成履歴を個別に保守しているので、複数フレームの使用時にも値が保たれることを指摘してくれたJD Smith氏に。


- pop-global-markがデフォルトではEmacsウィンドウを跨いで機能しないので少しadviceする必要があることを思い出させてくれたGrant Rosson氏に。

- winum-keymapの間違いを指摘してくれたu/simplex5d氏に。

- スクリーン上のウィンドウが2つ以下の際には
ace-windowアクションが利用可能になるようにaw-dispatch-alwaysをセットする必要があることを指摘してくれたkotatsuyaki氏に。

</div></details>

<!--
[^1]: Unfortunately for Emacs, its current design rules out some clever ideas that other editors have implemented. One example of this is the 4coder’s yeetsheet or Dion systems’ views: You can have buffers whose contents are "live" substrings of multiple other buffers, i.e. you can mix and match pieces of buffers. In Emacs the most you can have is indirect buffers, i.e. full "live" copies of a buffer.
-->

[^1]: Emacsにとっての不幸は他のエディターが実装済みの優れたアイデアが、現在の設計指針の埒外にあることだろう。例として[4corderのyeetsheet](https://github.com/perky/4coder_loco)や[Dion systemのview](https://www.youtube.com/watch?v=GB_oTjVVgDc)が挙げられる。これは他の複数のバッファーの"生きた"部分文字列を内容としてもつバッファーという概念であり、バッファーを組み合わせたり一致させることができる。一方、Emacsでもつことができるのはインダイレクトバッファー、すなわちバッファー全体の"生きた"コピーである。

<!--
[^2]: And this is the one aimed at developers using elisp. It’s not even the Emacs user manual!
-->

[^2]: Emacsのユーザーマニュアルの話しではない。elispを使いこなす開発者向けのマニュアルでさえこの記述なのだ!

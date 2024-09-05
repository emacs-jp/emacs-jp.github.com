---
layout: page
author: ayatakesi
title: Embarkを使う15の方法
tags:
  - Emacs
  - emacs-lisp
private: true
updated_at: '2024-08-09T22:37:13+09:00'
id: 5521fed7fd4f90213602
organization_url_name: null
slide: false
ignorePublish: false
---

これはKarthik Chikmagalurさんによって記述された記事を日本語に翻訳した記事であり、記事の所有権と著作権はKarthik Chikmagalurさんに帰属します。

元の記事: [Fifteen ways to use Embark \| Karthinks](https://karthinks.com/software/fifteen-ways-to-use-embark/)

<!-- 
Embark is a fantastic and thoughtfully designed package for Emacs that flips Emacs’ action → object ordering without adding a learning curve. It’s completely changed how I use Emacs, and I’m going to show you why. 
-->

[Embark](https://github.com/oantolin/embark/tree/98121bacef39abaf6f6849f87a439ba2184c03e2)は非常に優れたパッケージであり、思慮深く設計されたパッケージでもある。Emacsのアクション -> オブジェクトという順序を反転させるパッケージであり、学習曲線の追加なしで習得できる。EmbarkはわたしのEmacsの使い方を完全に変えてしまったんだ。この記事でその理由について説明しよう。

<!-- 
By default, Emacs’ action model is to specify the action (find-file), then the object (the file):
-->

Emacsのアクションモデルは、デフォルトではアクションを指定(たとえばfind-file)した後にオブジェクト(たとえばファイル)を指定する:

![emacs-pattern.png](https://karthinks.com/img/emacs-pattern.png)

<!--
This mirrors how one does things in a shell: 
-->

これはシェルで何か物事を行う方法を反映したモデルだ:

![shell-pattern.png](https://karthinks.com/img/shell-pattern.png)

<!--
The difference is that before you submit your shell command, you’re free to edit both the action and the object, since it’s just a line of text. In Emacs you can change the object freely, but to change the action you’d have to hit C-g and call a different command.
-->

違いはシェルコマンドのほうは単なるテキスト行なので、実行前であればアクションとオブジェクトはどちらも自由に編集できることだ。Emacsではオブジェクトは自由に変更できても、アクションを変更するにはC-gを押下してから別のコマンドの呼び出しが必要となる。

<!--
Things work the other way in a GUI program like a file manager. You select some representation of the object (usually an icon), then choose the action you would like to perform on it: 
-->

ファイルマネージャーのようなGUIプログラムでは別の方法によって物事が運ぶ。オブジェクト表現(通常はアイコン)を選択して、それにたいして実行したいアクションを選択するのだ:

![gui-pattern.png](https://karthinks.com/img/gui-pattern.png)

<!--
Either paradigm works fine, but this is Emacs, there’s no reason to choose just one! Embark lets you go back and forth between the two patterns. So you can call find-file (say) and pick a file, only to realize that you want to do something else with it, like open it in another window, or copy the file to a different directory: 
-->
どのパラダイムでも良好に機能する。しかしこちらはEmacsだ。ただ1つの方法を選択する謂われなど存在しない! **Embarkによって2つのパターンの間を行き来することが可能になる** 。たとえばファイルを別のウィンドウでオープンしたい? 別のディレクトリーにコピーしたい? 自分が何がしたいのかを決める前に、まずはfind-fileを呼び出してファイルを選ぶといった操作が可能になるのだ。

![embark-pattern.png](https://karthinks.com/img/embark-pattern.png)

<!--
With Embark, this is a breeze. 
-->

Embarkがあれば容易いことだ。

<!-- embark-act: Actually…. & But first… -->

# embark-act: 実は… & でもまずは…

<!--
embark-act is your “Actually…” command. As in, I called package-install and picked a package but actually I’d like to read the package description instead!
-->

あなたにとって"実は…"を実現するコマンドがembark-actだ。つまりpackage-installを呼び出してパッケージを選んだものの、実はパッケージの説明が読みたかったんだ! を実現するコマンドと言える。

<!-- 
embark-act is your “Yes, but first…” command as well. As in, I called find-file but first I’d like to copy it elsewhere to be safe, then continue to open this file! 
-->

同時にembark-actは"ああ、でもまずは…"を実現するコマンドでもある。つまりfind-fileを呼び出しました。でもまずは念のためどこか別の場所にコピーしたいな。その後でファイルのオープンを再開しよう! を実現するコマンドと言える。

<!-- 
Or perhaps you want to think of it as a keyboard driven analog of a “right-click menu” in a GUI environment. That works too, but the former maps better to the idea of “late-binding” and laziness that I think of Embark as enabling. 
-->

それともGUI環境における"右クリックメニュー"に相当する、キーボード駆動型の類似機能と考えるかもしれない。その類比も機能はするが、前者のほうがEmbarkによって実現が可能となった、"遅延バインディング"と怠惰の概念によく合うように思える。

<!-- 
Emacs makes you specify and fix the action/verb first (find-file, say), then choose the thing it acts on (the file). If you call embark-act, this is reversed. Now the object (file) is fixed, and you’re free to choose the action. 
-->

Emacsではまずアクション/動詞(たとえばfind-file)を指定·決定してから、それが作用する対象(たとえばファイル)を選択する。embark-actを呼び出すとこれが逆転する。今度はオブジェクト(ファイル)を決定してから、アクションを自由に選択することができるのだ。

<!-- 
I know: It sounds like I’m describing Helm actions. The difference is that Embark works everywhere, across all types of “objects”, and with every initial and wait-I-changed-my-mind command. There is no predetermined set of alternative actions configured to work with another predetermined set of initial actions. No one (including yourself) needs to have anticipated in advance what actions go together.1 This uniform, consistent integration into Emacs makes the difference between them one of kind and not of quantity, although it takes a bit of time to see this. 
-->

Helmアクションの説明に聞こえる? 分かっている。Embarkはどこでも、すべてのタイプの"オブジェクト"に跨って機能する点が異なる。最初のコマンドでも、wait-I-changed-my-mindコマンドでも機能する点が異なるのだ。事前定義された初期アクションの集合にたいして機能するように構成された、代替えアクションの集合が事前に別途定義されている訳ではない。(あなた自身を含めて)誰も連携させるアクションを前もって予想する必要はない[^1]。この均一かつ一貫したEmacsへの統合により、確認には若干の時間を要するが、両者の間に量ではなく質の違いがもたらされるのである。

<!--
This means you can start a command and select a candidate in the minibuffer, then call embark-act and M-x some-other-command to run that command on the candidate instead. If you are about to kill a buffer with C-x k but want to switch to it instead, you can call embark-act followed by C-x b. You can even do this without losing the kill-buffer prompt if you just want a quick peek at the buffer! 
-->
これはあるコマンドを開始してミニバッファーで候補を選択、それからembark-actを呼び出して、M-x some-other-commandでかわりそのコマンドを実行できることを意味している。C-x kでバッファーをkillしようとしたが、かわりにそのバッファーに切り替えたくなったのなら、embark-actを呼び出した後にC-x bで切り替えることができる。そのバッファーをちょっと確認したいだけであれば、kill-bufferのプロンプトを残したまま確認することすら可能なのだ!

<!--
The categories of objects Embark understands covers most common cases: filenames, buffers, bookmarks, URLs, text regions, variables, commands, symbols and more. 
-->

Embarkはファイル名、バッファー、ブックマーク、URL、テキストリージョン、変数、コマンド、シンボル等、一般的なオブジェクトカテゴリーのほとんどを理解する。

<!-- 
When you call embark-act, Embark also activates a keymap with direct access to common actions you might want to run for each category of object. This makes it unnecessary to use M-x to run your I-changed-my-mind action all the time, although you always have that option. You can, of course, add your own commands to this keymap as I do below. 
-->

embark-actを呼び出すと、Embarkがオブジェクトカテゴリーそれぞれにたいして、ユーザーが実行したいと思うような一般的なアクションに直接アクセスするためのキーマップのアクティブ化も行う。たとえあなたが実行するのが常にI-changed-my-mindコマンドであっても、このアクションを実行するたびに本来は毎回M-xを使用する必要があるのだが、これを行うことによって不要になる。もちろんわたしが行っているように、このキーマップに独自コマンドを追加することもできる。

<!--
I use embark-act literally hundreds of times every day. Here are a few of my common uses. A few of these are built in, others need some elisp to work, all are surprisingly useful. To be clear, this list barely scratches the surface of the sphere of possibilities with Embark. 
-->

わたしはembark-actを文字通り日に数百回使用する。以下にわたしがEmbarkを使用する際の一般的な使い方を示す。これらのうちのいくつかはビルトイン、他のものについては機能させるために若干のelispコードが必要だが、どれも驚くほど役に立つものばかりだ。誤解のないよう言っておくが、このリストはEmbarkがもつ可能性のほんの表面をなぞったに過ぎない。

<!-- A recipe for reproducing these demos -->

<details><summary>デモ再現のためのレシピ</summary><div>

<!--
I use Embark in conjunction with various Consult commands (consult-grep, consult-locate, consult-dir etc) in Emacs 27.2. If you want to reproduce these demos exactly in your Emacs, you will need the following packages:

embark
marginalia
vertico
consult
embark-consult
orderless
consult-dir
ace-window
0x0
Additionally you may need to bind the relevant commands (embark-act, embark-export, consult-*) to suitable keys.
-->

わたしはEmacs 27.2でConsultのさまざまなコマンド(consult-grep、consult-locate、consult-dir等)と組み合わせてEmbarkを使用している。これらのデモをあなたのEmacsで正確に再現したければ、以下のパッケージが必要になるだろう:
- embark
- marginalia
- vertico
- consult
- embark-consult
- orderless
- consult-dir
- ace-window
- 0x0

適切なキーで使用したければ、追加で関連コマンド(embark-act、embark-export、consult-*)のバインドが必要になるだろう。

</div></details>

<!-- Open any buffer by splitting any window -->

## 任意のウィンドウを分割して任意のバッファーをオープンする

<!--
This needs a little background. The ace-window package allows you to switch to a window based on keyboard hints. A less well known feature is that it also provides a “dispatch menu” that lets you act on windows in ways beyond simply switching to them: 
-->

少し背景の説明が必要だろう。ace-windowはキーボードヒントにもとづくウィンドウ切り替えを提供するパッケージだ。あまり知られていないが、単にウィンドウを切り替える以外の方法でウィンドウにアクションを行う、"ディスパッチメニュー"という機能も提供している。

[ace-dispatch-demo.mp4](https://karthinks.com/img/ace-dispatch-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
With two or more windows open, call ace-window
Press ? to bring up the dispatch menu.
Press the dispatch key to split a window horizontally (v in my video)
Press the ace-window key corresponding to the buffer you want to split (e in my video)
Repeat steps 1 and 2
Press the dispatch key to split a window vertically (s in my video)
Press the ace-window key corresponding to the buffer you want to split (w in my video)
-->

<ol>

<li>2つ以上のウィンドウをオープンして<code>ace-window</code>を呼び出す。</li>
<li>?を押下してディスパッチメニューを立ち上げる。</li>
<li>ウィンドウを水平方向に分割するためにディスパッチキー(ここではv)を押下する。</li>
<li>分割したいバッファーに応じたace-windowキー(ここではe)を押下する。</li>
<li>ステップ1と2を繰り返す。</li>
<li>ウィンドウを垂直方向に分割するためにディスパッチキー(ここではs)を押下する。</li>
<li>分割したいバッファーに応じたace-windowキー(ここではw)を押下する。</li>

</ol>

</details>

<!--
So you can kill windows, move them around, split them and more by using the dispatch keys. (Hit ? to bring up the dispatch menu.) 
-->

ディスパッチキーを使用してウィンドウのkill、移動、分割を行うことができる(?を押すとディスパッチメニューを表示)。

<!--
Now: You can call ace-window via Embark to display a candidate anywhere, including in splits that you create using the above dispatch menu. This means any buffer/file/bookmark I open is always placed exactly where I want it to be on the screen. 
-->

さて、これでEmbark経由でace-window呼び出して、任意の場所(上記ディスパッチメニューを使って作成した分割ウィンドウを含む)にある候補を表示できるようになった。これはわたしがオープンしたバッファー/ファイル/ブックマークを、スクリーン上でわたしが表示したい場所に常に配置できることを意味している。

<!--
In the below demo, I open a bookmark (with consult-bookmark), a file (with find-file) and a buffer (with consult-buffer) in sequence. Each time, I run embark-act and select the ace-window action, which activates ace-window. You can then display the buffer in any existing window by making a selection with ace-window. I actually go one step further in the demo: I split one of the existing windows using ace-window’s dispatch feature from above and display the relevant buffer in that split!
-->

以下のデモでは(consult-bookmarkで)ブックマーク、(find-fileで)ファイル、(consult-bufferで)バッファーを順にオープンしている。オープンする際には毎回embark-actを実行してからace-windowアクションを選択してace-windowをアクティブにしている。ace-windowで選択を行うことによって、既存のウィンドウのどれにでもバッファーを表示できる。実際のデモでは一歩進んで、ace-windowのディスパッチ機能を使って既存ウィンドウの1つを分割して、関連するバッファーの分割したウィンドウへの表示まで行っている!

[embark-ace-open-demo.mp4](https://karthinks.com/img/embark-ace-open-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
Run a command that requires selecting a file, bookmark or buffer, perhaps switch-to-buffer
Select one and run embark-act
Run the my/embark-ace-action with o (see below)
Select the window where you want the buffer to be placed, OR
Split an existing window with v or b (see aw-dispatch-alist) followed by a window selection, and display the buffer in the new split.
-->

<ol>
<li>ファイル、ブックマーク、バッファーの選択を要するコマンド(switch-to-bufferとか)を実行する。</li>
<li>選択したら<code>embark-act</code>を実行する。</li>
<li><code>o</code>で<code>my/embark-ace-action</code>を実行する(以下参照)。</li>
<li>バッファーを配置したいウィンドウを選択する。</li>
<li>または<code>v</code>や<code>b</code>(<code>aw-dispatch-alist</code>を参照)で既存のウィンドウを分割してウィンドウを選択、新たに分割されたウィンドウにバッファーを表示する。</li>
</ol>

</details>

<!--
To get this to work, you’ll need to add a few ace-window functions to the Embark file actions map: 
-->

これを機能させるにはEmbarkのファイルアクションマップにace-windowの関数をいくつか追加する必要がある:

```elisp
(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
      (interactive)
      (with-demoted-errors "%s"
       (require 'ace-window)
       (let ((aw-dispatch-always t))
        (aw-switch-to-window (aw-select nil))
        (call-interactively (symbol-function ',fn)))))))

(define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
(define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
(define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
```

<!--
I also add actions to open the buffer in a vertical or horizontal split. But you probably don’t need this, since you can do this and a lot more with ace-window’s dispatch menu! 
-->

わたしはさらにウィンドウを垂直または水平に分割してバッファーをオープンするアクションも追加しているが、ace-windowのディスパッチメニューで同等以上のことができるので、あなたには多分必要ないだろう!

```elisp
(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                      (symbol-name fn)
                      "-"
                      (car (last  (split-string
                                   (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

(define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
(define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
(define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

(define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
(define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
(define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))
```

## ファイルのオープン時にリモートへコピーする

[embark-copy-remote-demo.mp4](https://karthinks.com/img/embark-copy-remote-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
Run any command that requires selecting a file, perhaps find-file-other-window
Select one and run embark-act
Run the copy-file action with c. Embark has a key for this but you can also M-x copy-file here.
Navigate to the destination path. In the video I used the consult-dir package to instantly switch the path to one of my bookmarks, a remote location.
Press RET to copy the file. You can type in a name to copy it as.
-->

<ol>

<li>ファイルの選択を要する任意のコマンド(<code>find-file-other-window</code>とか)を実行する。</li>
<li>ファイルを選択して<code>embark-act</code>を実行する。</li>
<li><code>c</code>で<code>copy-file</code>アクションを実行する。Embarkにはこれ用のキーがあるが、ここで<code>M-x copy-file</code>の実行も可。</li>
<li>目的となるパスへ移動する。ビデオでは<code>consult-dir</code>を使って、わたしのブックマークからリモートにあるパスに一時的に切り替えている。</li>
<li><code>RET</code>を押下してファイルをコピーする。コピー先での名前を入力できる。</li>

</ol>

</details>

<!--
Here’s what happened. In any file prompt, you can call embark-act and select the copy action to copy the file instead. (You could just as well call M-x copy-file.) In this case I then use consult-dir to insert a bookmark that points to my server into the destination prompt, and the file is copied using Tramp. 
-->

ここでは何が起きているのだろうか? ファイルの入力を求める任意のプロンプトでembark-actを実行することで、(M-x copy-fileを呼び出したかのように)かわりにファイルをコピーするアクションを選択できるのだ。デモではコピー先の入力を求めるプロンプトにたいして、consult-dirを使用してわたしのサーバーを指すブックマークを挿入、ファイルのコピーはTrampが行っている。

<!--
You can even do this without losing the find-file prompt! Calling embark-act with a prefix argument keeps the prompt alive: 
-->

find-fileプロンプトを残したまま、これを行うことすらできるのだ! embark-act呼び出しでプレフィックス引数を指定することにより、プロンプトをそのまま残すことができる。

[embark-copy-remote-persist-demo.mp4](https://karthinks.com/img/embark-copy-remote-persist-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
Run any command that requires selecting a file, perhaps find-file-other-window
Select one and run embark-act with a prefix argument. That is, if your binding for embark-act is C-., run C-u C-..
Run the copy-file action with c. Embark has a key for this but you can also M-x copy-file here.
Navigate to the destination path. In the video I used the consult-dir package to instantly switch the path to one of my bookmarks, a remote location.
Press RET to copy the file. You can type in a name to copy it as.
Continue to use your find-file-other-window prompt as before.
-->
<ol>

<li>ファイルの選択を要する任意のコマンド(find-file-other-windowとか)を実行する。</li>
<li>ファイルを選択してプレフィックス引数とともにembark-actを実行する。つまりC-.にembark-actをバインドしている場合にはC-u C-.。</li>
<li>cでcopy-fileアクションを実行する。Embarkにはこれを行うキーが用意されているがM-x copy-fileでも可。</li>
<li>目的のパスへ移動する。ビデオではconsult-dirを使って、わたしのブックマークからリモートにあるパスに一時的に切り替えている。</li>
<li>RETを押下してファイルをコピーする。コピー先での名前を入力できる。</li>
<li>操作の前に行っていたfind-file-other-windowプロンプトへの入力を継続する。</li>

</ol>

</details>

<!--
At the end I quit the find-file prompt manually and check the remote directory to ensure that the file has been copied. 
-->

最後はfind-fileプロンプトを手作業で閉じてから、ちゃんとファイルがコピーされたかリモートディレクトリーをチェックしている。

<!--Insert a minibuffer candidate into the buffer-->
## バッファーへのミニバッファー候補の挿入

シンプルだけど非常に優秀。

[embark-insert-demo.mp4](https://karthinks.com/img/embark-insert-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Run any command that requires you to make a selection using the minibuffer. The selection can be anything, it just has to display some text.
In the video I chose a distant directory with consult-dir and selected a file in that directory.
Run embark-act
Press i to insert the text of the selection into the main buffer. In the video I used I instead to insert the selected file’s relative path. I does different things based on the type of object you’re selecting. For example, I with a buffer candidate inserts the contents of the buffer instead.
-->

1. ミニバッファーを使って何かの選択を要求する任意のコマンドを実行する。選択は何でもよいが、何かテキストを表示するもの。
2. ビデオではconsult-dirで離れたディレクトリーを選び、そのディレクトリーのファイルを選択している。
3. embark-actを実行する。
4. iを押下してメインとなるバッファーに選択したテキストを挿入する。ビデオではIで選択したファイルの相対パスを挿入した。選択したオブジェクトのタイプに応じてIは違うことを行う。たとえば候補がバッファーなら、バッファーのコンテンツを挿入する。

</div></details>

<!-- Run a shell command on a minibuffer candidate file without losing your session -->
## セッションを継続しつつミニバッファーのファイル候補のシェルコマンドを実行する

*でもまず必要なのは…* の完璧な例

[embark-shell-cmd-demo.mp4](https://karthinks.com/img/embark-shell-cmd-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
Run any command that requires selecting a file, perhaps find-file
I switched to a distant directory using consult-dir.
Select a file and run embark-act with a prefix argument. That is, if your binding for embark-act is C-., run C-u C-..
Press & to run the async-shell-command action. Embark has a key for this in its keymaps but you could run M-x async-shell-command or call its default keybinding (M-&) instead.
Type in the command at the prompt. The file name is already filled in for you. I used the file shell command for more info on a file.
Press RET to run the command and return to the find-file prompt.
-->

<ol>

<li>ファイルの選択を要求する任意のコマンド(find-fileとか)を実行する。</li>
<li><code>consult-dir</code>で離れたディレクトリーに切り替える。</li>
<li>ファイルを選択して、プレフィックス引数とともにembark-actを実行する。つまり<code>C-.</code>に<code>embark-act</code>をバインドしている場合には<code>C-u C-.</code>。</li>
<li><code>&</code>を押下して<code>async-shell-command</code>アクションを実行する。Embarkのキーマップにはこれを行うためのキーがあるが<code>M-x async-shell-command</code>、あるいは<code>async-shell-commandの</code>デフォルトのキーバインディング(<code>M-&</code>)でも実行可。</li>
<li>プロンプトにコマンドを入力する。ファイル名はすでに用意されている。ファイルについてさらに情報を得るために、シェルコマンドfileを使用した。</li>
<li><code>RET</code>を押下してコマンドを実行して、<code>find-file</code>の入力プロンプトにリターンする。</li>

</ol>

</details>

<!--
I called the “file” shell command for more info on the file without ending the find-file prompt. 
-->
ファイルについてさらに情報を得るために、find-fileのプロンプトを終了させずにシェルコマンドの"file"を呼び出した。

<!-- Open a file as root without losing your session -->
## セッションを維持したままrootでファイルをオープン

<!--
Emacs’ version of forgetting to add sudo before the command. In the shell you can go back to the start of the prompt and type it in, or engage in the sudo !! ritual. In Emacs I use an Embark action: 
-->

コマンドの前のsudo忘れちゃったの、のEmacs版である。シェルならプロンプトの先頭に戻ってタイプして追加、あるいはsudo !!を召喚して儀式を執り行うところだが、EmacsではEmbarkアクションを使用する。

[embark-sudo-demo.mp4](https://karthinks.com/img/embark-sudo-demo.mp4)

<details><summary>ビデオ実況</summary>

<!--
Run any command that requires selecting a file. I used consult-locate to locate a root-owned file on my filesystem.
Select one and run embark-act with a prefix argument. That is, if your binding for embark-act is C-., run C-u C-..
Select the sudo-find-file action with S. Note: You’ll need to add this action to the keymap, see below. Alternatively you can run M-x sudo-find-file or its global binding.
-->

<ol>

<li>ファイルの選択を要求する任意のコマンドを実行する。わたしはファイルシステム上でroot所有のファイルをlocateするためにconsult-locateを使用した。</li>
<li>ファイルを選択してプレフィックス引数とともに<code>embark-act</code>を実行する。つまり<code>C-.</code>に<code>embark-act</code>をバインドしている場合には<code>C-u C-.</code>。</li>
<li><code>S</code>で<code>sudo-find</code>アクションを選択した。注意:このアクションをキーマップに追加する必要があるだろう(以下参照)。かわりに<code>M-x sudo-find-file</code>やそれのグローバルバインディングの実行でも可。</li>

</ol>

</details>

<!--
<li>For the sudo program there is the sudo-edit package, although I used a snippet from my init file that I</li> can’t ascertain the provenance of anymore: 
-->

わたしはconsult-locateをコマンドを実行したが、前の例と同じようにファイルの入力を求めるコマンドなら何でも機能するはずだ。わたしはinitファイルからもはや来歴不明なスニペットを流用したが、sudoプログラム向けのsudo-editパッケージもある。

```elisp
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))
```

<!--
To use sudo-find-file as an Embark action, you can run it (with M-x or a global keybinding) after calling embark-act, or shorten the process further by adding an entry to Embark’s file actions map: 
-->

Embarkアクションとしてsudo-find-fileを使用するには、embark-act呼び出し後にM-xやグローバルバインディングから実行するか、あるいはEmbarkのファイルアクションのマップに追加してさらに短縮することもできる:

```elisp
(define-key embark-file-map (kbd "S") 'sudo-find-file)
```

<!-- Upload a region of text to 0x0 -->
## テキストリージョンを0x0にアップロード

[embark-0x0-region-demo.mp4](https://karthinks.com/img/embark-0x0-region-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Select a region of text in a buffer.
Run embark-act.
Press U to choose the 0x0-dwim action. Note: You’ll need to add this action to the keymap, see below.
The region text will be uploaded to 0x0 and URL added to the kill-ring. (See message at the end of the video.)
-->

1. バッファーでテキストのリージョンを選択する。
2. embark-actを実行する。
3. U私押下して0x0-dwimアクションを選択する。注意: このアクションをキーマップに追加する必要があるだろう(以下参照)。
4. リージョンのテキストが0x0にアップロードされて、URLがkill-ringに追加される(ビデオの最後のメッセージに注目)。

</div></details>

<!--
I’m using the 0x0 package for the 0x0-dwim function. When called as an Embark action on a URL, this shortens it. When called on a file, it uploads the file. The echo area message at the end (from 0x0-dwim) tells me the upload URL has been copied to the kill ring. As with the other examples, you can call 0x0-dwim after running embark-act or define a short key for it in one of Embark’s keymaps: 
-->

0x0-dwim関数のために[0x0パッケージ](https://melpa.org/#/0x0)を使用している。URLにたいしてEmbarkアクションとして呼び出すとURLを短縮、ファイルの場合にはファイルをアップロードする。最後の(0x0-dwimからの)エコーエリアのメッセージによって、アップロードURLがkillリングにコピーされたことがわかる。他の例と同じように、embark-actの後に0x0-dwimを呼び出したり、Embarkのキーマップの1つとしてより短いキーを定義してもよい:

```elisp
(define-key embark-region-map (kbd "U") '0x0-dwim)
```

<!-- Visit a package’s URL from the minibuffer -->
## ミニバッファーからパッケージのURLをvisit

[embark-package-url-demo.mp4](https://karthinks.com/img/embark-package-url-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Run any command that requires selecting a package, perhaps describe-package (C-h P by default)
Select a package and run embark-act
Press u to run the embark-browse-package-url action.
-->

1. パッケージの選択を要する任意のコマンド(describe-packageとか; デフォルトではC-h P)を実行する。
2. パッケージを選択してembark-actを実行する。
3. uを押下してembark-browse-package-urlアクションを実行する。

</div></details>

<!--
In this case I ran the describe-package command before going “Actually… URL please”, but in this example as all the others, there’s nothing special about describe-package. Any command that gives you a list of packages at the minibuffer will proffer the same set of Embark actions. 
-->

今回は **"実は…URLを頼む"** の前にdescribe-packageコマンドを実行したが、describe-packageでも特別な扱いは不要であり、他の例と同様である。ミニバッファーでパッケージのリストを表示するすべてのコマンドは、同じ一連のEmbarkアクションを提供する。

<!-- Set a variable from anywhere it appears in a buffer -->
## バッファー内の任意の場所から変数をセット

<!--
Super handy for quickly setting variables, especially when testing code. 
-->

素早く変数をセットする。コードのテスト時には非常に役に立つ。

[embark-set-var-demo.mp4](https://karthinks.com/img/embark-set-var-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Move point to a variable in a buffer. (Alternatively, run a command that requires selecting a variable at the minibuffer, like describe-variable)
Run embark-act.
Press = to run the set-variable action. Embark has a key for this in its keymaps, but you could call M-x set-variable instead.
Set the new value of the variable.
-->

1. バッファー内の変数にポイントを移動する(describe-variableのようにミニバッファーで変数の選択を要求するコマンドでも可)。
2. embark-actを実行する。
3. =を押下してset-variableアクションを実行する。Embarkはこれを行うためのキーをキーマップに所有するが、M-x set-variableの呼び出しでも可。
4. 変数に新しい値をセットする。

</div></details>

<!--
In this case Embark has an entry for set-variable in its variables keymap (bound to =), but you can just call M-x set-variable. 
-->

このデモではEmbark自身の変数キーマップにset-variable用のエントリーを所有している(=にバインド)が、単にM-x set-variableで呼び出しでも可。

<!-- Add a keybinding for a command name from anywhere it appears -->
## 任意の場所からコマンド名にキーバインディングを追加

<!-- Set all the keys. -->

すべてのキーをセットする。

[embark-set-key-demo.mp4](https://karthinks.com/img/embark-set-key-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Move point to a command name in a buffer. (Alternatively, run a command that requires selecting a command at the minibuffer, like describe-command)
Run embark-act.
Press g to run the global-set-key action. Embark has a key for this in its keymaps, but you could call M-x global-set-key instead.
Set the new keybinding for the command.
-->

1. バッファー内でコマンド名が表示されている場所にポイントを移動する(describe-commandのようなミニバッファーでコマンドの選択を要求するコマンドを実行してもよい)。
2. embark-actを実行する。
3. gを押下してglobal-set-keyアクションを実行する。Embarkのキーマップにはこれを行うキーが定義されているが、M-x global-set-keyを呼び出してもよい。
4. コマンドに新たなキーバインディングをセットする。

</div></details>

<!--
Embark provides an action in its keymap to run global-set-key, but you could just call M-x global-set-key after running embark-act with the point on a command name. There is also local-set-key in the embark keymap. 
-->

Embarkは自身のキーマップでglobal-set-keyを実行するアクションを提供しているが、コマンド名にポイントを配置してembark-actを実行した後にM-x global-set-keyを呼び出してもよい。embarkのキーマップにはlocal-set-keyもセットされている。

<!-- embark-export: I want a gist, so give me a list -->
# embark-export: 要点が知りたいのでリストください

<!--
If that was everything Embark did I’d be a happy camper. But embark-act isn’t even its best feature. That would be the gem of composability that is embark-export (and its lesser kin embark-collect). These commands create persistent collections from minibuffer candidate lists: It’s one part ivy-occur and one part glue that ties together Emacs libraries better than Emacs does. The examples illustrate why. 
-->

すべてがEmbarkだったなら、わたしは幸せだったこちだろう。しかしembark-actさえもっとも優れた機能ではないのだ。構成を可能にするということにかけては、embark-export(と劣化版のembark-collect)こそがEmbarkの至宝と言えるだろう。これらのコマンドはミニバッファーの候補リストから永続的なコレクションを作成する。一方はivy-occur、もう一方はEmacsよりも巧妙にEmacsライブラリー同士を結びつける接着剤の役割りを果たすのだ。理由は例を使って説明しよう:

<!-- Export Emacs package candidates to a package menu -->
## Emacsのパッケージ候補をパッケージメニューにエクスポート

<!-- 
Want a package-menu-mode buffer with all packages involving shells in Emacs? embark-export has you covered: 
-->
package-menu-modeでEmacsのシェル関連のすべてのパッケージを見たい? それならembark-exportの守備範囲だ:

[embark-package-export-demo.mp4](https://karthinks.com/img/embark-package-export-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!-- 
Run any command that requires selecting a package, perhaps  c describe-package (C-h P by default)
(Optional) Type in some text to filter the completions list
Run embark-export
-->

1. 何かパッケージの選択を要求するコマンド(describe-packageとか; デフォルトではC-h Pにバインドされている)を実行する。
2. (オプション)何かテキストを入力して補完リストをフィルタリングする。
3. embark-exportを実行する。

</div></details>

<!--
The clever idea behind embark-export is to reuse Emacs’ built-in functionality whenever possible: the package-menu library already handles displaying packages. If you’re generating a list of packages with user-specified conditions, why reinvent the wheel? 
-->

embark-exportの背後には、可能なら常にEmacsのビルトイン機能を再利用するという優れたアイデアが存在する。すでにpackage-menuライブラリーがパッケージの表示を処理しているので、それを再利用するのだ。ユーザーが指定した条件でパッケージのリストを生成しているのに、車輪を再発明する必要があるだろうか?

<!-- Collect imenu candidates in an “imenu-list” -->
## "imenu-list"候補の収集

<!--
embark-collect creates persistent collections of minibuffer completion candidates (filtered by user input) in a way that basically obsoletes every “listing” package for me. In this example I create a filtered list of imenu items that sticks around and that I can use to navigate around the file: 
-->

embark-collectは(ユーザーの入力によってフィルタリングされた)ミニバッファーの補完候補にたいする永続的なコレクション作成する。これにより、基本的に何かを"リスト"するようなすべてのパッケージは、わたしにとって時代遅れのものとなってしまった。以下の例ではファイルに何かを行う際に使用できるimenuアイテムのフィルター済みリストを作成している:

[embark-imenu-list-demo.mp4](https://karthinks.com/img/embark-imenu-list-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!-- 
When visiting any file, run imenu or a version of it (I ran consult-imenu-all)
(Optional) Type in some text to filter the completions list
Run embark-export. (This actually runs embark-collect under the hood. You could run embark-collect directly instead, but it’s simpler to just use one command.)
Press RET in the persistent collections buffer to navigate to that symbol in the file.
-->

1. 何かファイルをvisitする際にimenuやその別バージョン(わたしの場合はconsult-imenu-all)を実行する。
2. (オプション)何かテキストを入力して補完リストをフィルタリングする。
3. embark-exportを実行する(実際のところ水面下ではembark-collectを実行している; embark-collectを直接実行してもよいが使うコマンドは1つだけのほうがシンプルだろう)。
4. 永続的なコレクションバッファーでRETを押下すれば、ファイル内のそのシンボルに移動する。

</div></details>

<!--
I didn’t show this in the demo, but all embark-act actions are available in the Collections buffer, and you can even call them directly (i.e. without calling embark-act first) by turning on embark-collect-direct-action-minor-mode. 
-->

デモでは示さなかったが、Collectionバッファーではすべてのembark-actアクションが利用できる。embark-collect-direct-action-minor-modeをチューニングすれば、(最初にembark-actを呼び出さなくとも)直接呼び出すことさえ可能だ。

<!-- Export file candidates to a dired-buffer -->
## dired-bufferへのファイル候補のエクスポート

<!--
Have a list of files you arrived at in a tortuous manner that you want to keep around? dired was created to list files, and embark-export respects this: 
-->

入手までに紆余曲折を経た、保管しておきたいファイルリストをお持ちでは? ファイルのリストを作成するために作られたdired、作成されたファイルリストへの配慮はembark-exportに任せよう:

[embark-file-export-demo.mp4](https://karthinks.com/img/embark-file-export-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!-- 
Run any command that requires selecting a file. I used consult-fd to find all files matching a pattern below a directory.
(Optional) Type in some text to filter the completions list. With consult-fd you do this by adding a # to end the string to search on and begin the string used to filter the results.
Run embark-export.
-->

1. 何かファイルの選択を要求するコマンドを実行する。わたしはディレクトリー配下で、あるパターンにマッチするすべてのファイルを検索するためにconsult-fdを使用した。
2. (オプション)何かテキストを入力して補完リストをフィルタリングする。consult-fdでは検索を行う文字列を#で終了してから、その結果をフィルタリングするために用いる文字列を開始する。
3. embark-exportを実行する。

</div></details>

<!-- This obsoletes find-name-dired, another “listing” based feature. -->

"リスティング"に基づいたこれとは別の機能であるfind-name-diredも過去のものとなった。

<!-- Export buffer candidates to ibuffer -->
## ibufferへのバッファー候補のエクスポート

<!-- You saw this coming: Any list of buffers gets exported to an ibuffer. -->
バッファーのリストなら何でもibufferにエクスポートできる(そう来ると思った、と言われそう)。

[embark-buffer-export-demo.mp4](https://karthinks.com/img/embark-buffer-export-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Run any command that requires selecting a buffer. I used consult-buffer.
Type in some text to filter the completions list:
I pressed b SPC to narrow the consult-buffer list to buffers, then
typed in ! followed by * to omit all buffers2 that begin with a *, i.e. “special” buffers.
Run embark-export.
-->

1. 何かバッファーの選択を要求するコマンドを実行する。わたしが使ったのはconsult-buffer。
2. 何かテキストを入力して補完リストをフィルタリングする:
   - consult-bufferのリストをバッファーに絞り込むためにb SPCを押下して、
   - \*で始まるすべてのバッファー(つまり"スペシャル"バッファー)を除外するために!と*をタイプする[^2]。
3. embark-exportを実行する。

</div></details>

<!--
This is a great way to transition from looking up a variable to a full-fledged apropos on relevant items when you need to. 
-->

これは変数の検索から、必要に応じて関連アイテムへの本格的なaproposへと遷移するための非常に優れた手段として使用できる。

<!-- Export grep or line candidates to a grep buffer -->
## grepバッファーへのgrepや行の候補のエクスポート

<!-- 
Any occur-like results (from consult-line, grep, xref etc) get exported into a grep buffer. 
-->

occurのような結果(consult-lineやgrep、xrefなどの出力)であればgrepバッファーにエクスポートできる。

[embark-grep-demo.mp4](https://karthinks.com/img/embark-grep-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Run a command that generates a list of grep-style matches. I used consult-ripgrep in the demo. Other Consult-based options are consult-grep, consult-git-grep and consult-line.
Type in some text to find grep results.
Not necessary: I flipped through the various files that matched with vertico-next-group and vertico-previous-group.
Run embark-export.
Turn on next-error-follow-minor-mode in the grep buffer with C-c C-f. This jumps each match as I…
…navigate the grep buffer with M-n and M-p (compilation-next-error and compilation-previous-error), and move to the next/previous matching file with } and {.
-->

1. grepスタイルのマッチリストを生成するコマンドを実行する。デモでわたしが使用したのはconsult-ripgrep。Consultベースの他のオプションとしてはconsult-grep、consult-git-grep、consult-lineがある。
2. テキストをタイプしてgrep結果を検索する。
3. (必須ではない)わたしはvertico-next-groupとvertico-previous-groupでマッチしたファイルを散策した。
4. embark-exportを実行する。
5. C-c C-fでgrepバッファーでnext-error-follow-minor-modeをオンに切り替える。これにより…
6. grepバッファーで M-nとM-p(compilation-next-errorおよびcompilation-previous-error), でマッチ間を移動して、}と{でマッチした次/前のファイルに移動する。

</div></details>

<!--
Note that this is a regular grep buffer, so you can use all your tricks, like wgrep to edit the grep buffer and save changes in all the files.
-->

これは通常のgrepバッファーであることに注意してほしい。つまりgrepバッファーを編集して変更のすべてをファイルに書き戻す、wgrepのようなあなたのいつものテクニックのすべてが使用可能なのだ。

<!-- BONUS: Use Embark Actions like Helm -->
# ボーナス: EmbarkのアクションをHelmのように使う

<!--
In the above examples, the available embark actions were displayed in some window in the frame. Embark has multiple “prompters” listing the preset actions, and with a little elbow grease you can set up something similar to Helm:
-->

これまでの例では利用可能なembarkアクションは、そのフレームのどこかのウィンドウに表示されていた。Embarkにはプリセットされたアクションをリストする"プロンプター(prompter)"が複数あり、少し手間をかければHelmと似たような何かをセットアップできる[^3]:

[embark-helm-demo.mp4](https://karthinks.com/img/embark-helm-demo.mp4)

<details><summary>ビデオ実況</summary><div>

<!--
Run any command involving minibuffer selection, consult-buffer in the video.
(Optional) type in something to filter the completions list or select a buffer.
Press TAB to switch to the list of embark actions.
Press TAB again to switch back to the list of candidates.
Search for an action by name (in this case “kill”) to filter the list of actions
Erase and search for a different action (“diff”) and choose the diff-buffer-with-file action
Press RET to run diff-buffer-with-file on the selected buffer
(Optional) Navigate the diff hunks with diff-hunk-next and diff-hunk-prev
(Optional) Fold the diff sections with outline-cycle
Run consult-buffer again and select a buffer
Switch to the actions list again with TAB
Press @ to call an action by its keybinding instead of selecting it by name
Call the embark action for killing a buffer with k to kill the selected buffer.
-->

1. 何かミニバッファーでの選択を含むようなコマンドを実行する(ビデオではconsult-buffer)。
2. (オプション)何かタイプして補完リストをフィルターするか、バッファーを選択する。
3. TABを押下してembarkアクションのリストに切り替える。
4. TABをもう一度押下して候補リストに戻る。
5. アクションリストをフィルターするために、名前でアクションを検索する(ビデオでは"kill")。
6. 消して別のアクションを検索(今度は"diff")して、diff-buffer-with-file actionを選択する。
7. RETを押下して、選択されたバッファーでdiff-buffer-with-fileを実行する。
8. (オプション)diff-hunk-nextとdiff-hunk-prevでdiffのhunk間を移動する。
9. (オプション)outline-cycleでdiff選択を折りたたむ。
10. もう一度consult-bufferを実行して、バッファーを選択する。
11. TABでもう一度アクションリストを切り替える。
12. @を押下して、名前ではなくキーバインディングでアクションを呼び出す。
13. kでバッファーをkillするembarkアクションを呼び出して、選択されたバッファーをkillする。

</div></details>

<!--
Here I switch back and forth between the list of actions and the list of candidates (like in Helm) with C-<tab>. In the actions list you can either type the action (matched with completing-read), or call the action directly by prepending its keybinding with @. 
-->

デモではC-\<tab\>で、(helmのように)アクションリストや補完リスト間を行ったり来たり切り替えている。アクションリストではアクションをタイプするか(マッチにはcompleting-readを使用)、@を前置してキーバインディングで直接アクションを呼び出すことができる。

<!-- Elbow grease: -->
手間の部分:
```elisp
(defun with-minibuffer-keymap (keymap)
  (lambda (fn &rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (use-local-map
           (make-composed-keymap keymap (current-local-map))))
      (apply fn args))))

(defvar embark-completing-read-prompter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") 'abort-recursive-edit)
    map))

(advice-add 'embark-completing-read-prompter :around
            (with-minibuffer-keymap embark-completing-read-prompter-map))
(define-key vertico-map (kbd "<tab>") 'embark-act-with-completing-read)

(defun embark-act-with-completing-read (&optional arg)
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (embark-indicators '(embark-minimal-indicator)))
    (embark-act arg)))
```

<!--
Replace vertico-map above with your completion system of choice’s active minibuffer keymap. The default is minibuffer-local-completion-map. 
-->

上記例のvertico-mapについては、お好みの補完システムのミニバッファー用のアクティブキーマップに置き換えてほしい。デフォルトはminibuffer-local-completion-mapだ。

<!--
Remember that unlike with Helm, you’re not restricted to these actions when you use Embark! You can call literally any command that it makes sense to with its keybinding or with M-x after running embark-act. 
-->

Embark使えばアクションに制限がないことを忘れないでほしい。Helmとは違うのだ! embark-actを実行した後にキーバインディングやM-xで必要に応じてコマンドそのものを呼び出すことができるのだ。

# 33%

<!--
That’s fifteen useful Embark thingamajigs, and I didn’t get to mention embark-become. Or embark-prefix-help-map, embark-which-key-prompter, or Embark’s targets and target cycling, or half a dozen more thoughtful features and niceties about Embark. Maybe next time. 
-->

これが"Embarkの例のやつ"と評される便利な15の機能だが、embark-becomeについてはまだ触れていない。embark-prefix-help-map、embark-which-key-prompter、Embarkのターゲットやターゲット巡回、他にも半ダースに及ぶEmbarkの気の利いた機能や魅力についても同様。でもそれは次の機会にでも。

<!--
I’ll conclude instead by mentioning the main packages I used in the above demos:
-->

かわりといってはなんだが、デモで使用した主なパッケージに触れて結びとさせてもらおう。

<!--
embark by Omar Antolin Camarena, who’s been a pleasure to interact with and pester with my requests for features. To add custom actions to the embark keymaps or otherwise customize Embark, I suggest perusing the README. It’s as readable and informative as they come.
-->
- Omar Antolin Camarenaの[embark](https://github.com/oantolin/embark)。彼と交流するのは楽しかったし、機能にたいするわたしのリクエストにもこころよく応えてくれた。embarkキーマップへのアクション追加やEmbarkをカスタマイズするなら、READMEはとても読みやすく非常に有益なので、熟読することをお勧めする。

<!--
consult for its various enhancements to Emacs’ builtins. consult-locate and consult-find (actually consult-fd) to find files, consult-imenu for a colorful imenu with grouping and consult-ripgrep to grep across a directory. 
-->
- Emacsビルトインにたいするさまざまな機能強化については[こちら](https://github.com/minad/consult)をconsult(参照)してほしい。ファイルのオープンにはconsult-locateconsult-find(実際はconsult-fd)、グルーピングつきのカラフルなimenuにはconsult-imenu、ディレクトリーを横断するgrepにはconsult-ripgrepがある。

<!--
marginalia for the annotations in the minibuffer. Co-maintained by 
-->
ミニバッファーの注釈にはmarginaliaを使用した。Omar AntolinとDaniel Mendlerにより共同で保守されている。

<!--
vertico as the minibuffer completion interface. Consult, Vertico and Marginalia are all authored by Daniel Mendler, who I’m convinced never sleeps. I didn’t even mention Corfu. 
-->
- ミニバッファーの補完インターフェイスとしては[vertico](https://github.com/minad/vertico)を使用した Consult、Vertico、MarginaliaはすべてDaniel Mendlerによって開発された。リストには含めなかったCorfu以外にもこれだけのパッケージだ、彼はきっと眠らないに違いない。

<!--
The orderless completion style, also by Omar Antolin, to match pieces of text against minibuffer candidates independently. Together these five packages form the MOVEC pentagram, a composable enhancement suite that integrates Emacs’ loosely bound libraries into a modern and cohesive whole. 
-->
- 補完スタイルは[orderless](https://github.com/oantolin/orderless)を使用した。これも開発者はOmar Antolinで、テキストをミニバッファーの補完候補と個別にマッチするために使用している。これら5つのパッケージを組み合わせることによって形成されるMOVECペンタグラム(訳注:MOVECはMarginalia、Orderless、Vertico、、Embark、Consultの頭文字、ペンタグラムは五芒星で魔術的な意味をもつ5つの頂点をもつ星印)は、緩やかにバインドされたEmacsパッケージ全体を、現代的で密に結合するための構成可能な拡張パッケージコレクションである。

<!--
consult-dir to switch directories quickly. I used this multiple times above to navigate to distant directories when in the minibuffer prompt. 
-->
- 素早いディレクトリー移動には[consult-dir](https://github.com/karthink/consult-dir)を使用した。上述した例において、ミニバッファーの中から遠くのディレクトリーに移動する際に何度か使用している。

<!--
popper to make embark-collect, help and other ephemeral buffers behave when they appear on screen.
-->
- [popper](https://github.com/karthink/popper)はembark-collectやヘルプ、その他の一時的なバッファーがスクリーン上に表示される際に使用している(訳注: 開発は著者さん)。

<!--
ace-window by abo-abo, whose dispatch-keys idea in Ace-Window and Avy I promptly ripped off for Popper. If I understand correctly his Ivy-Occur was an early influence on what became Embark-Collect as well. 
-->

- abo-aboが開発した[ace-window](https://github.com/abo-abo/ace-window)を使用している。Ace-WindowとAvyのdispatch-keyのアイデアについては、速攻でpopperにパクらせていただいた。わたしの理解が正しければ、彼のIvy-Occurは初期のEmbark-Collectにも影響を与えているはずだ。

<!--
0x0 by William Vaughn. I use this far more often than I thought I would. 
-->
- William Vaughnの[0x0](https://git.sr.ht/~willvaughn/emacs-0x0)は、自分で思った以上に頻繁に使用している。

<!--
Finally a quick note for Doom Emacs users: Doom ships with Embark out of the box (as of Sep 2021), you don’t need to do anything besides looking up the keys for embark-act and embark-collect. 
-->
最後にDoom Emacsユーザー向け注意事項を簡潔に記す: Doomには素の状態でEmbarkが同梱されている(2019年9月現在)ので、embark-actとembark-collectのキーを調べる以外何も行う必要はない。

<!--
Despite what these examples suggest, I estimate that I use less than a third of what Embark provides. Even so, in allowing me to change or chain actions at any time, it lets me pilot Emacs by the seat of my pants. A second, unforeseen benefit is that it makes commands and listings that I would never use available in a frictionless way: commands like transpose-regions and apply-macro-to-region-lines, or custom dired, ibuffer and package-menu listings that are interactively inaccessible otherwise[^4]. The ability to quickly whip up such buffers makes knowhing how to use dired or ibuffer pay off several fold.  In composing such features seamlessly with minibuffer interaction or with text-regions, Embark acts as a lever to amplify the power of Emacs’ myriad built in commands and libraries.
-->
これらの例が示唆する事実にも関わらず、おそらくわたしはEmbarkの能力の3分の1も使用できていないのだろう。たとえそうであったとしても、いつでもアクションの変更したり連鎖させられるので、経験と勘によるEmacsの操縦を可能にしてくれるのだ。2つ目の予期せぬ利点として、わたしが決して使うことがなかったであろうコマンドやリストを、抵抗なく利用できるようになることだ。transpose-regionsやapply-macro-to-region-linesのようなコマンド、あるいはdired、ibuffer、package-menuのような機能を使わなければアクセスできなかったカスタマイズされたリストをスムーズに利用できるようになった[^4]。このようなバッファーを素早く作成できるので、diredやibufferを使う方法を知ることによって、数倍の利益が得られるようになる。これらのような機能とミニバッファーとの対話やテキストリージョンをシームレスに構成する際に、Emacsに組み込まれた無数のコマンドやライブラリーのパワーを増幅するレバーとして振る舞うのがEmbarkなのである。

# 脚注

<!-- Although of course, Helm and Embark both do a good job with their presets. -->
[^1]: もちろんHelmとEmbarkはどちらも自身の役割を良好にこなすよう事前に調整されている訳だが。

<!-- To match the inverse of an input string with !, I used a feature of theの orderless package for Emacs.  -->
[^2]: 入力文字列以外と!をマッチさせるために、Emacs 向けパッケージorderlessを使用した。

<!-- Yes, it’s not fully Helm-style since it still uses the minibuffer instead of a buffer to show the candidates/actions. You could use vertico-buffer if that’s a sticking point.  -->
[^3]: その通り。これは完全なHelmスタイルではない。なぜなら候補/アクションの表示にバッファーではなくミニバッファーを使うからだ。そこが躊躇する原因ならばvertico-bufferを使えばよい。

<!--
[^4]: Technically custom package-menu listings are accessible. From the full package listing (M-x list-packages), you can filter package names by regexp with / n. 
-->

[^4]: カスタマイズされたpackage-menuリストへのアクセスは技術的には可能である。完全なパッケージリスト(M-x list-packages)から、/ nでパッケージ名にたいしてregexpでフィルタリングできる。

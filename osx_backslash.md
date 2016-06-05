---
layout: page
title: "Macでの\\(バックスラッシュ)入力について"
description: ""
---
{% include JB/setup %}

Mac OS X と日本語設定の環境では、`delete` の左側のキーで `\` (バックスラッシュ) ではなく `¥` (円記号) が入力されます。

通常は `alt option` と書かれたキー (以下 `option`)と `¥` を同時に押すことで `\` を入力することができますが、Emacs の初期設定では `option` は `Meta` キーの代用として設定されています。

`\` が入力できないとコーディングに支障が出るので、何らかの手段で `\` を入力できるようにする必要があります。

ターミナルで使用する場合
------------------------

ターミナル(Terminal.app) や [iTerm2](http://www.iterm2.com/#/section/home) で Emacs を起動する場合 (`emacs -nw`) は、通常そのままで `\` (バックスラッシュ) を入力することができます。

Metaキーの割り当てを変える
--------------------------

`Meta` を `⌘(Command)` キーに割り当て、`option` を Mac 本来の挙動に割り当てることで `option + ¥` で `\` を入力することができます。

```cl
;; command key as meta key
(setq ns-command-modifier 'meta)
;; option + yen(jis keyboard) => backslash
(setq ns-alternate-modifier 'option)
```

IMEパッチを使用している場合
---------------------------

[http://sakito.jp/emacs/emacs24.html#ime](http://sakito.jp/emacs/emacs24.html#ime) などを参考に IME パッチを当ててビルドした場合は、`.emacs` に次の一行を加えることで `\` を入力できるようになります。

```cl
(mac-translate-from-yen-to-backslash)
```

global-set-keyで文字を挿入する
------------------------------

`.emacs` に以下の設定を追加することで、 `option + ¥` で `\` を入力できるようになります。

```cl
(global-set-key (kbd "M-¥") (lambda () (interactive) (insert "\\")))
```

また、次のように設定することで単独で `\` 、 `option` と同時に押すことで `¥` を入力できるようにできます。

```cl
(global-set-key (kbd "¥") (lambda () (interactive) (insert "\\")))
(global-set-key (kbd "M-¥") (lambda () (interactive) (insert "¥")))
```

KeyRemap4MacBookでキーボードをリマップする
------------------------------------------

[KeyRemap4MacBook](http://pqrs.org/macosx/keyremap4macbook/index.html.ja) は Mac 用のキーボードマップ割り当てツールで、非常に柔軟にカスタマイズすることができます。

ただし影響範囲が大きく Emacs カスタマイズの範疇からも逸脱するため、この記事では詳細に解説しません。

この記事について
----------------

この記事は [Macでの\\(バックスラッシュ)入力について · Issue #22 · emacs-jp/emacs-jp.github.com](https://github.com/emacs-jp/emacs-jp.github.com/issues/22) に寄せられた質問と回答をまとめたものです。

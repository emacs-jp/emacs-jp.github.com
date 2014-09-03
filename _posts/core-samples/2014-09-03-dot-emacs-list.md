---
layout: post
title: issue#15に投稿されたinit.elのリストをまとめました 
tags: [init.el, dot-emacs]
---

{% include JB/setup %}

# [issue\#15 .emacs 晒してる方](https://github.com/emacs-jp/emacs-jp.github.com/issues/15)に投稿されたinit.elのリストをまとめました。

\#15で投稿されたinit.elのリストをまとめました。ただまとめるのも大雑把すぎるので、それぞれどんなパッケージ管理をしているかなどを書いてみました。

[nanasess/dot.emacs ? GitHub](https://github.com/nanasess/dot.emacs)

- el-get

[dot_files/emacs at master ? syohex/dot_files ? GitHub](https://github.com/syohex/dot_files/tree/master/emacs/)

- Cask
- セットアップ処理が書かれている

[eiel/.emacs.d ? GitHub](https://github.com/eiel/.emacs.d)

- init-loaderと似た設定読み込みシステム

[byplayer/dot.emacs.d ? GitHub](https://github.com/byplayer/dot.emacs.d)

- package.el

[muratayusuke/dot.emacs.d ? GitHub](https://github.com/muratayusuke/dot.emacs.d)

- package.el

[shibayu36/emacs ? GitHub](https://github.com/shibayu36/emacs)

- pymacsを使っている
- Cask

[dotfiles/.emacs.d at master ? tarao/dotfiles ? GitHub](https://github.com/tarao/dotfiles/tree/master/.emacs.d)

- el-get + init-loader

[gongo/elfactory ? GitHub](https://github.com/gongo/elfactory)

- Cask、shell scripでインストールやデプロイができる環境を作ってる

[shishi/.emacs.d ? GitHub](https://github.com/shishi/.emacs.d)

- package.el + init-loader.el

[emacs.d/init.el at master ? takaxp/emacs.d ? GitHub](https://github.com/takaxp/emacs.d/blob/master/init.el)

- org-babel? org-modeを駆使して独自のビルド環境を作ってる
- 参考リンク : [Configurations for GNU Emacs - PASTELWIKI](http://pastelwill.jp/wiki/doku.php?id=emacs:init.el)

[ogatomo/emacs ? GitHub](https://github.com/ogatomo/emacs)

- el-get

[uwabami/emacs-config ? GitHub](https://github.com/uwabami/dot.emacs.d)

- el-get、org-babel を使用

[pogin503/dot-emacs ? GitHub](https://github.com/pogin503/dot-emacs)

- Cask + init-loader

[handlename/dot-emacs ? GitHub](https://github.com/handlename/dot-emacs)

- package.el + init-loader

[murasesyuka/dotemacs ? GitHub](https://github.com/murasesyuka/dotemacs)

- init-*.el を loadしていく方式

[yewton/dot-emacs ? GitHub](https://github.com/yewton/dot-emacs)

- el-get + org-babel + package.el

[posaunehm/.emacs.d ? GitHub](https://github.com/posaunehm/.emacs.d)

- init-loader

[niku/.emacs.d ? GitHub](https://github.com/niku/.emacs.d)

- init-loader + Cask

[dotfiles/init.el at master ? zk-phi/dotfiles ? GitHub](https://github.com/zk-phi/dotfiles/blob/master/emacs/init.el)

- 独自ビルド
- 遅延読み込みをして、init.elの読み込みを早くしているみたい

[sakito / dot.emacs.d / wiki / Home — Bitbucket](https://bitbucket.org/sakito/dot.emacs.d/)

- init-*.el を require していく方式
- 参考リンク : [紹介マニアMoinMoin:dot.emacs.d](http://sakito.jp/moin/moin.cgi/dot.emacs.d)

[masutaka/.emacs ? Gists](https://gist.github.com/masutaka/8177244)

- package.el 

漏れがだいぶありそうなのでまた調べたら追記します。

init.el勉強会の参考になればと思います。

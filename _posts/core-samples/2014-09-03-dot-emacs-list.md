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
- make で *.elファイルをコンパイルできるものを作っている。
    - [dot.emacs/Makefile at master · nanasess/dot.emacs](https://github.com/nanasess/dot.emacs/blob/master/Makefile)

[dot_files/emacs at master ? syohex/dot_files ? GitHub](https://github.com/syohex/dot_files/tree/master/emacs/)

- Cask + init-loader
- 大体どんな環境でも、設定が完了するようなセットアップ処理が書かれている

[eiel/.emacs.d ? GitHub](https://github.com/eiel/.emacs.d)

- init-loaderと似た設定読み込みシステム
- bundle/ に拡張elispを submodule として管理するようにしている

[byplayer/dot.emacs.d ? GitHub](https://github.com/byplayer/dot.emacs.d)

- package.el + init-loader
- dash.el, f.el ライブラリを使用している

[muratayusuke/dot.emacs.d ? GitHub](https://github.com/muratayusuke/dot.emacs.d)

- package.el + init-loader
- dash.el, f.el ライブラリを使用している

[shibayu36/emacs ? GitHub](https://github.com/shibayu36/emacs)

- Cask + package.el + init-loader
- pymacs を使っている
- pysmell というのを使っている
    - [PySmellでTextMateにPythonコードの補完機能を付与する．](http://d.hatena.ne.jp/moch-lite/20090419/p1)
- [abbrev](http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/abbrev.html)を使いこなしてそう

[dotfiles/.emacs.d at master ? tarao/dotfiles ? GitHub](https://github.com/tarao/dotfiles/tree/master/.emacs.d)

- el-get + init-loader
- [bundle.el](https://github.com/tarao/bundle-el) というel-getラッパーを使っている
    - パッケージのインストール((el-get 'sync 'package-name)相当のこと)と同じところにそのパッケージの設定を書ける
-  「[typesterさんの記事](http://unknownplace.org/memo/2013/01/21/1/)にあるようにemacs -q -l init.elで誰でもこの設定を試せる(既存の設定は汚さない)」 とのこと
    - 実際便利っぼい

[gongo/elfactory ? GitHub](https://github.com/gongo/elfactory)

- Cask + init-loader
- shell scriptでインストールやデプロイができる環境を作ってる

[shishi/.emacs.d ? GitHub](https://github.com/shishi/.emacs.d)

- package.el + init-loader.el
- [abbrev](http://www.math.s.chiba-u.ac.jp/~matsu/emacs/emacs21/abbrev.html)を使いこなしてそう

[emacs.d/init.el at master ? takaxp/emacs.d ? GitHub](https://github.com/takaxp/emacs.d/blob/master/init.el)

- org-babel? org-modeを駆使して独自のビルド環境を作ってる
- 参考リンク : [Configurations for GNU Emacs - PASTELWIKI](http://pastelwill.jp/wiki/doku.php?id=emacs:init.el)

[sakito / dot.emacs.d / wiki / Home — Bitbucket](https://bitbucket.org/sakito/dot.emacs.d/)

- init-*.el を require していく方式
- 参考リンク : [紹介マニアMoinMoin:dot.emacs.d](http://sakito.jp/moin/moin.cgi/dot.emacs.d)

[ogatomo/emacs ? GitHub](https://github.com/ogatomo/emacs)

- el-get
- init-*.el をloadしていく方式

[uwabami/emacs-config ? GitHub](https://github.com/uwabami/dot.emacs.d)

- el-get + org-babel

[pogin503/dot-emacs ? GitHub](https://github.com/pogin503/dot-emacs)

- Cask + init-loader

[handlename/dot-emacs ? GitHub](https://github.com/handlename/dot-emacs)

- package.el + init-loader
- [quelpa](https://github.com/quelpa/quelpa)を導入された模様
    - 参考リンク : [emacs quelpa : 【本邦初公開】MELPAを改善した新しいパッケージ管理システム - るびきち×Emacs](http://rubikitch.com/2014/09/01/quelpa/)

[murasesyuka/dotemacs ? GitHub](https://github.com/murasesyuka/dotemacs)

- auto-install + package.el
- init-*.el を loadしていく方式

[yewton/dot-emacs ? GitHub](https://github.com/yewton/dot-emacs)

- el-get + org-babel + package.el + init-loader

[posaunehm/.emacs.d ? GitHub](https://github.com/posaunehm/.emacs.d)

- package.el + init-loader

[niku/.emacs.d ? GitHub](https://github.com/niku/.emacs.d)

- Cask + init-loader

[dotfiles/init.el at master ? zk-phi/dotfiles ? GitHub](https://github.com/zk-phi/dotfiles/blob/master/emacs/init.el)

- setup.el の自作読み込みの関数でロードしている
    - setup.el はいったいどこに?
- 遅延読み込みをして、init.elの読み込みを早くしているみたい

[masutaka/.emacs ? Gists](https://gist.github.com/masutaka/8177244)

- el-get
- 単一ファイルで見やすい

[.emacs.d/init.el at master ? yoshitia/.emacs.d ? GitHub](https://github.com/yoshitia/.emacs.d/blob/master/init.el)

- auto-install + package.el
- 最近Emacsにデビューしたニューカマー

[sugyan/dotfiles ? GitHub](https://github.com/sugyan/dotfiles)

- package.el + init-loader
- emacs関連は.emacs.dのほうにまとまってある

[supermomonga/dot-emacs ? GitHub](https://github.com/supermomonga/dot-emacs)

- el-get + マクロ用いて設定を読み込んでいる
- evilはemacs-stateを全く使わないというコンセプトで設定をしているらしい

[dotfiles/share/dot.emacs.d at master · fjyuu/dotfiles](https://github.com/fjyuu/dotfiles/tree/master/share/dot.emacs.d)

- package.el + el-get
- 動作環境はLinux or OS X
- 基本的にはpackage.elで管理して、リポジトリにないものをel-getで管理している

<br>
init.el勉強会の参考になればと思います。

## 個人的な感想

- package.el、el-get、Cask、逐次読み込み、org-babel、自作の関数での読み込み、多種多様なinit.elが出来上がってる。
- init-loaderがかなり使われている
- だれも[Plelude](https://github.com/bbatsov/prelude)とか、[Emacs-live](https://github.com/overtone/emacs-live)、[Emacs24 Starter Kit](https://github.com/eschulte/emacs24-starter-kit)、[Oh-My-Emacs](https://github.com/xiaohanyu/oh-my-emacs)などのスターターキットを使っていないというのがわかった。
あそこら辺は便利そうなのでいつか使ってみたいと思っている。Emacs初心者の人に対してきっとおすすめできるはずなんだけれど使用者がいないからどうしたものか。
ちなみに↓に良さげなStarter Kit一覧があります。
[Starter Kit - emacs-tw/awesome-emacs](https://github.com/emacs-tw/awesome-emacs#starter-kit)

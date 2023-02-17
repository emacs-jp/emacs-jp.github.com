---
layout: page
title: "Emacs for Beginners"
description: "Emacsビギナーのためのページ"
redirect_to: /tips/emacs-in-2020
---
{% include JB/setup %}

Emacs を使ってみたい、と思っても何をどうしていいのかわからない、そんな方のために次の内容を説明します。

* Emacs のインストール方法
* Emacs のチュートリアル
* Emacs についての情報収集の方法


## Emacsのインストール

各環境ごとに代表的なEmacsのインストール方法を簡単に述べます。

### Windows

GNU公式などいろいろなバイナリパッケージが存在していますが、日本語環境ではgnupackで入れる方法が便利です。

* [Gnupack Users Guide](http://gnupack.sourceforge.jp/docs/latest/UsersGuide.html)

### MacOSX

最新バイナリはこちらからインストールできます。

* [Emacs for MacOSX](http://www.emacsformacosx.com/)

ports、homebrew、その他コンパイルなどの情報は以下を参照してください。

* [Emacs24 のインストールと新機能](http://sakito.jp/emacs/emacs24.html)

### Linuxディストリビューション

RedHat、Fedora、CentOS、Ubuntu、Debian など、大抵のOSではパッケージでインストールすることができます。

#### RedHat, CentOS, Fedora

```
sudo yum install emacs
```

####  Ubuntu, Debian

```
sudo apt-get install emacs
```

独自ビルドする場合は、ソースコードの中の INSTALL ファイルを参照してください。

### その他ビルド情報

* [MinGWを使ってのビルド方法](https://gist.github.com/nakinor/5187609) (2013/03/20)
* [Ubuntu Weekly Recipe 第235回　Ubuntu 12.04でEmacs 24.1を使う](http://gihyo.jp/admin/serial/01/ubuntu-recipe/0235) (2012/08/08)


## ドキュメント

### チュートリアル

まったくEmacs初めての人向けの情報。じっくり取り組むには書籍がいいかもしれません。

* [Emacs 初心者のための最低限のコマンド表](http://chalow.net/2007-01-11-1.html)
* [Emacs チュートリアルのすすめ](http://at-aka.blogspot.jp/2007/01/emacs_19.html)

### 書籍

比較的最近出版された本としては以下のものがあります。

#### Emacs実践入門 (大竹智也著)

* Emacsを日常のテキスト編集やプログラム開発で、快適に利用するためのノウハウを解説した書籍です。最初に読む本としてお勧めです。
* [Amazonサイト](http://www.amazon.co.jp/dp/4774150029)

####  Emacsテクニックバイブル　~作業効率をカイゼンする200の技~ (るびきち著)

* Emacsの改善ネタがたくさん集まっています。
* [Amazonサイト](http://www.amazon.co.jp/dp/4774143278)

#### Emacs Lispテクニックバイブル (るびきち著)

* Emacsを本格的に利用するために必要な Emacs Lisp の情報がまとまっています。
* [Amazonサイト](http://www.amazon.co.jp/dp/4774148970)

#### やさしいEmacs-Lisp講座 (広瀬雄二著)

* Emacs Lispで本格的に拡張を行う方法をコンパクトにまとめた本です。
* [Amazonサイト](http://www.amazon.co.jp/dp/4877832718)


### リファレンス

#### `M-x info`

Emacsに標準で付属しているInfoが公式な一次情報です。

#### Infoの Web版

[Emacs Manuals](http://www.gnu.org/software/emacs/#Manuals)

日本語の情報が欲しい場合、古いですが以下の情報が参考になります。

* [GNU Emacsマニュアル (21.3版)](http://www.bookshelf.jp/texi/emacs-man/21-3/jp/emacs.html) (2005年頃)
* [GNU Emacs Lispリファレンスマニュアル (21.3版)](http://www.bookshelf.jp/texi/elisp-manual/21-2-8/jp/elisp.html)

## ML、オンラインコミュニケーション

### Slack emacs-jp

* https://emacs-jp.slack.com
* 参加したい方はこちらの[招待リンク](https://join.slack.com/t/emacs-jp/shared_invite/zt-1id7hvbxh-~n_wSBrdrHMk8~Ge8Fp3IQ)からサインアップできます

### twitter

* ハッシュタグ [#emacs](https://twitter.com/hashtag/emacs?src=hash), [#emacsjp](https://twitter.com/hashtag/emacsjp?src=hash)
* [今すぐフォローすべき Emacs界のスーパーエンジニア(2011年版)](http://d.hatena.ne.jp/syohex/20111012/1318429372)

### GitHub

- [emacs-jp](https://github.com/emacs-jp)

### [ELIPS](http://www.jpl.org/elips/ELIPS-ML.html)

Emacsに関する何でもありのMLです。最近は流量が少ないです。

### [EmacsWiki](http://www.emacswiki.org/)

Emacsに関する総合情報サイトです。英語情報が中心で、古い情報も混じっています。

### [Lingr Emacs](http://lingr.com/room/emacs)

Emacsのチャットです。たまに盛り上がります。([過去ログ](http://lingr.com/room/emacs/archives))


英語ですが、最新を追うには以下の情報元が参考になります。

### [Emacs Google+](https://plus.google.com/communities/114815898697665598016)

Emacsに関する話題いろいろ

### [emacs関連本家メーリングリスト](http://savannah.gnu.org/mail/?group_id=40)

* help-gnu-emacs(一般的質問など)
* emacs-devel(Emacsの開発の中心)
* bug-gnu-emacs(バグ報告など)

## 地域コミュニティ

お近くのコミュニティへ是非ご参加ください。

### 関東Emacs
* [(kantou-emacs #x01) #関東Emacs](https://atnd.org/events/54734)
* [(kantou-emacs #x02) #関東Emacs](https://atnd.org/events/63979)
* [(kantou-emacs #x03) 第3回関東Emacs勉強会](https://kantou-emacs.doorkeeper.jp/events/42422)

### 東京Emacs勉強会
* 2012/08/24 [Emacs勉強会](http://shibuya.doorkeeper.jp/events/1615)

### 関西Emacs
* 関西Emacs勉強会 まとめ(http://peccu.sytes.net/ke/)
* 半年おきに定期的に開催

### 福岡Emacs
* 2011/09/23 [fukuoka_emacs #0x03](http://atnd.org/events/19653)
* 2012/08/25 [Emacs温泉](http://d.hatena.ne.jp/kiwanami/20120827/1346092543)
* 2015/06/05 [福岡Emacs 0x04](http://fukuoka-emacs.connpass.com/event/15117/)

<!--
This file has been left for redirection.
Please do not add any content.
Redirect to /tips/emacs-in-2020.
This file will be deleted after 6 month (2020/03/01).

;; Local Variables:
;; buffer-read-only: t
;; End:
-->

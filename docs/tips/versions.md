---
layout: page
author: zonuexe
title: "Emacsのバージョン"
description: "今日にEmacsと呼ばれるGNU Emacsのバージョン表記とリリースの種類、過去の更新履歴についてまとめます。"
date: 2019-01-13
last_modified: 2022-09-12
---
{% include JB/setup %}

## 最新バージョンについて

歴史的には多様なEmacsがありますが、今日においてEmacsと呼ばれるのは、もっぱら**[GNU Emacs]**です。

<ins datetime="2022-09-12T20:00:00+0900">**GNU Emacs**の最新安定版は **`28.2`** (<time datetime="2022-09-12T06:13:13-0400">2022年9月12日</time>リリース)です。<br>
<!-- 次期安定版のリリース候補版として<time datetime="2020-07-28T22:30:24+0200">2020年7月28日</time>に**`27.1-rc1`**がリリースされました。<br> -->
<!-- GNU Emacsの新しい安定版である**`27.1`**が<time datetime="2020-08-06">2020年8月6日</time>にリリースされることが予告されています。<br> -->
masterブランチでは次のメジャーバージョンとなる **`29.0`** 系統の開発が進行しています。</ins>

[GNU Emacs]: https://ja.wikipedia.org/wiki/GNU_Emacs

## バージョン表記

GNU Emacsのバージョン表記は<strong><code><var>x</var>.<var>y</var></code></strong>や<strong><code><var>x</var>.<var>y</var>.<var>z</var></code></strong>または<strong><code><var>x</var>.<var>y</var>-rc</code></strong>の形式です。

ここでは**Emacs 27**を例に紹介します。

### 開発版 (master)

 * 正式にリリースされていない開発中のバージョンです
 * 便宜上、**`27.0.50`**のような番号がつけられています
 * EmacsのGitリポジトリのmasterブランチからソースコードをチェックアウトした場合は通常このバージョンです
 * 日常的に変更されるため、**自分で問題解決できるユーザー**以外には向きません

### プレテスト版 (pretest)
 * 次の安定版に向けて開発が進むと段階的にリリースされるバージョンです
 * 通例、**`27.0.90`**から始まり、**`27.0.91`**, **`27.0.92`**... のように進んでいきます
 * 一般的なソフトウェアの**β版**に相当します
 * **新しいものをいち早く試したい熟練ユーザー**はこの時点で試してもよいでしょう

### リリース候補版 (<abbr title="Release Candidate">RC</abbr>)

 * さらに開発が進み、安定版としてリリースするための候補バージョンです
 * **`27.1-rc1`**, **`27.1-rc2`**, **`27.2-rc1`**のように、バージョン番号の最後に`-rc`がつきます
 * **Lispパッケージの開発者**はこの段階でテストしておくことが望ましいです

### 安定版 (stable)

 * 品質が安定し、正式にリリースされたバージョンです
 * **`27.1`**, **`27.2`** のような2桁の表記です
 * **特別な理由がない限りはこのバージョンの利用を推奨します**

##  各OSのEmacsバージョン

### GNU/Linux

**GNU Emacs**は多くのGNU/Linuxシステムのパッケージマネージャからインストールできます。ただしEmacsのリリースサイクルとOS(ディストリビューション)のリリースサイクルは異なっており、最新のEmacsが取得できるわけではない場合があることに気をつけてください。

### macOS

GNU EmacsはmacOS向けにCocoa APIによるGUIをサポートしています。macOSのサポート状況およびシステム固有のカスタマイズについては[Mac OS / GNUstep (GNU Emacs Manual (Japanese Translation))](https://ayatakesi.github.io/emacs/27.2/html/Mac-OS-_002f-GNUstep.html)を参照してください。

macOSでのEmacsのインストール方法には、ソースコードからビルドする以外に複数の選択肢があります。おそらく[Homebrew](https://brew.sh/index_ja)を使うのがもっとも簡単でしょう。`emacs`以外は[Tap](https://docs.brew.sh/Taps)として提供されています。

 * [Emacs Mac Port](https://bitbucket.org/mituharu/emacs-mac/src/master/) (`emacs-mac`)
   * 山本光晴氏によるEmacs 27.2の派生であり、ネイティブGUIを実装することでmacOSとの親和性を強化したバージョンです ([変更点についての説明](https://bitbucket.org/mituharu/emacs-mac/raw/master/README-mac))
   * HomebrewのTapは[railwaycat/homebrew-emacsmacport](https://github.com/railwaycat/homebrew-emacsmacport)で提供されています
   * `brew install`時のオプションでアプリのアイコンを変更できます
 * [Emacs Plus](https://github.com/d12frosted/homebrew-emacs-plus) (`emacs-plus`)
   * d12frosted氏によるHomebrewのTapで、通常のEmacsにパッチを追加しデフォルトのビルドオプションを変更したものです
   * `brew install`時のオプションでアプリのアイコンを変更できます
 * [Emacs](https://formulae.brew.sh/formula/emacs#default) `emacs`
   * `brew install emacs`でインストールできるGUIなしのパッケージです
   * ターミナルでのみEmacsを利用し、GUIサポートが一切不要の場合に利用できます
 * [GNU Emacs For Mac OS X] (`--cask emacs`)
   * `brew install --cask emacs`でインストールできるビルド済みのEmacsパッケージです
   * Doom Emacsを利用する場合は[互換性に問題があるため利用しない](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org)ことを推奨しています

| パッケージ名 | 最新安定版(27.2) | ビルド済み安定版(cask)    | 開発版(28.50) | window-system   |
|--------------|:----------------:|:-------------------------:|:-------------:|-----------------|
| emacs-mac    | ○                | [homebrew-emacsmacport]   | ×             | `'mac`          |
| emacs-plus   | ○                | ×                         | ○             | `'ns`           |
| emacs        | CLIのみ          | ×                         | ○             |                 |
| emacs (cask) | ○                | [GNU Emacs For Mac OS X]  | ×             | `'ns`           |

[homebrew-emacsmacport]: https://github.com/railwaycat/homebrew-emacsmacport/releases
[GNU Emacs For Mac OS X]: https://emacsformacosx.com/

**macOS 10.14 Mojave以前**に添付されていたのは2007年にリリースされた**Emacs 22.1**という古いバージョンなので、利用しないことを**強く推奨**します。

### Microsoft Windows

WindowsでのEmacsについては[GNU Emacs for Windows再入門](/tips/emacs-for-windows)および[GNU Emacs download - GNU Project](https://www.gnu.org/software/emacs/download.html)などを参考にしてください。

日本語入力について[trueroad/tr-emacs-ime-module: Emulator of GNU Emacs IME patch for Windows (tr-ime)](https://github.com/trueroad/tr-emacs-ime-module#readme)を参考にMELPAから`tr-ime`パッケージをインストールし有効化することでIMEの挙動を改善できます。

<p class="alert alert-warning" role="alert">プレテスト版および開発版のコンパイル済みバイナリは<a href="https://alpha.gnu.org/gnu/emacs/pretest/windows/">alpha.gnu.org</a>からダウンロードもできます。ただし、正式リリースされたものではなく、最新版ではないものが含まれていることを注意してください。技術的な問題を自己解決できない場合は安定版のバージョンを利用することを強く推奨します。</p>

## リリース履歴

古いEmacs 22から23, 24の頃のEmacsの日本語情報はsakitoさんの記事に詳しいです。

 * [Emacs23 (Cocoa Emacs) 入門から中毒まで : 紹介マニア](http://sakito.jp/emacs/emacs23.html)
 * [Emacs24 のインストールと新機能 : 紹介マニア](http://sakito.jp/emacs/emacs24.html)

Emacsマニュアルでは「アンチニュース」の形式でまとめられています。（時間を遡るユーザーにダウングレード情報を提供するというジョーク体裁で書かれた逆ニュースなので、機能は「単純化」「削除されました」は実際には<u>機能追加を意味する</u>ことに気をつけてください）

 * [Emacs 23 アンチニュース](https://ayatakesi.github.io/emacs/24.5/Antinews.html)
 * [Emacs 24 アンチニュース](https://ayatakesi.github.io/emacs/25.2/html/Antinews.html)
 * [Emacs 25 アンチニュース](https://ayatakesi.github.io/emacs/26.1/html/Antinews.html)
 * [Emacs 26 アンチニュース](https://ayatakesi.github.io/emacs/27.1/html/Antinews.html)


以下の表は2018年(Emacs 26)以降のGNU Emacsのリリースカレンダーです。

<div style="width: 100%; overflow-y: scroll;">
<table class="table table-hover" style="text-align:center">
<thead><tr><th>年／月</th>
<th scope="col">1</th><th scope="col">2</th><th scope="col">3</th><th scope="col">4</th><th scope="col">5</th><th scope="col">6</th><th scope="col">7</th><th scope="col">8</th><th scope="col">9</th><th scope="col">10</th><th scope="col">11</th><th scope="col">12</th></tr></thead>
<tbody>
<tr><th scope="row">2018</th>
  <td><span class="badge badge-pill badge-secondary">26.0.91</span></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-info">26.1-rc1</span></td>
  <td><span class="badge badge-pill badge-primary">26.1</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-secondary">26.1.90</span></td>
  <td></td>
</tr>
<tr><th scope="row">2019</th>
  <td><span class="badge badge-pill badge-secondary">26.1.91</span></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-primary">26.2</span></td>
  <td></td>
  <td><span class="badge badge-pill badge-secondary">26.2.90</span></td>
  <td></td>
  <td><span class="badge badge-pill badge-primary">26.3</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
</tr>
<tr><th scope="row">2020</th>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-secondary">27.0.90</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-info">27.1-rc1</span></td>
  <td><span class="badge badge-pill badge-primary">27.1</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
</tr>
<tr><th scope="row">2021</th>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-primary">27.2</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-secondary">28.0.90</span></td>
</tr>
<tr><th scope="row">2022</th>
  <td><span class="badge badge-pill badge-secondary">28.0.91</span></td>
  <td></td>
  <td><span class="badge badge-pill badge-secondary">28.0.92</span></td>
  <td><span class="badge badge-pill badge-primary">28.1</span></td>
  <td></td>
  <td></td>
  <td></td>
  <td></td>
  <td><span class="badge badge-pill badge-primary">28.2</span></td>
  <td></td>
  <td></td>
  <td></td>
</tr>
</tbody></table>
</div>

公式サイトの[GNU Emacs Release History]には安定版のリリース履歴がリストアップされています。

[GNU Emacs Release History]: https://www.gnu.org/software/emacs/history.html

<table class="table"><thead><tr><th scope="col">バージョン</th><th scope="col">リリース日</th></tr></thead>
<tbody>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2022-09/msg00730.html">Emacs 28.1</a></th><td><time datetime="2022-09-12">2022年9月12日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2022-04/msg00093.html">Emacs 28.1</a></th><td><time datetime="2022-04-04">2022年4月4日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/info-gnu/2021-03/msg00008.html">Emacs 27.2</a></th><td><time datetime="2021-03-25">2021年3月25日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2020-08/msg00237.html">Emacs 27.1</a></th><td><time datetime="2020-08-10">2020年8月10日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2019-08/msg00577.html">Emacs 26.3</a></th><td><time datetime="2019-04-12">2019年8月28日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2019-04/msg00503.html">Emacs 26.2</a></th><td><time datetime="2019-04-12">2019年4月12日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2018-05/msg00765.html">Emacs 26.1</a></th><td><time datetime="2018-05-28">2018年5月28日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu/2017-09/msg00006.html">Emacs 25.3</a></th><td><time datetime="2017-09-11">2017年9月11日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2017-04/msg00000.html">Emacs 25.2</a></th><td><time datetime="2017-04-21">2017年4月21日</time></td></tr>
<tr><th scope="row"><a href="https://lists.gnu.org/archive/html/emacs-devel/2016-09/msg00451.html">Emacs 25.1</a></th><td><time datetime="2016-09-17">2016年9月17日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2015-04/msg00002.html">Emacs 24.5</a></th><td><time datetime="2015-04-10">2015年4月10日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2014-10/msg00002.html">Emacs 24.4</a></th><td><time datetime="2014-10-20">2014年10月20日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2013-03/msg00001.html">Emacs 24.3</a></th><td><time datetime="2013-03-11">2013年3月11日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2012-08/msg00000.html">Emacs 24.2</a></th><td><time datetime="2012-08-27">2012年8月27日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2012-06/msg00000.html">Emacs 24.1</a></th><td><time datetime="2012-06-10">2012年6月10日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2012-01/msg00000.html">Emacs 23.4</a></th><td><time datetime="2012-01-29">2012年1月29日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2011-03/msg00000.html">Emacs 23.3</a></th><td><time datetime="2011-03-10">2011年3月10日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2010-05/msg00000.html">Emacs 23.2</a></th><td><time datetime="2010-05-08">2010年5月8日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2009-07/msg00000.html">Emacs 23.1</a></th><td><time datetime="2009-07-29">2009年7月29日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2008-09/msg00000.html">Emacs 22.3</a></th><td><time datetime="2008-09-05">2008年9月5日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2008-03/msg00000.html">Emacs 22.2</a></th><td><time datetime="2008-03-26">2008年3月26日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2007-06/msg00000.html">Emacs 22.1</a></th><td><time datetime="2007-06-02">2007年6月2日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2005-02/msg00000.html">Emacs 21.4</a></th><td><time datetime="2005-02-06">2005年2月6日</time></td></tr>
<tr><th scope="row"><a href="http://mail.gnu.org/archive/html/info-gnu-emacs/2003-03/msg00000.html">Emacs 21.3</a></th><td><time datetime="2003-03-24">2003年3月24日</time></td></tr>
<tr><th scope="row"><a href="http://mail.gnu.org/archive/html/info-gnu-emacs/2002-03/msg00000.html">Emacs 21.2</a></th><td><time datetime="2002-03-18">2002年3月18日</time></td></tr>
<tr><th scope="row"><a href="http://lists.gnu.org/archive/html/info-gnu-emacs/2001-10/msg00009.html">Emacs 21.1</a></th><td><time datetime="2001-10-28">2001年10月28日</time></td></tr>
</tbody></table>

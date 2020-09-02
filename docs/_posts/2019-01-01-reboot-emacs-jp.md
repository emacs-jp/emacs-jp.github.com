---
layout: post
author: zonuexe
title: 2019年Emacs JPを再始動します
tags: [announcement]
date: 2019-01-01
last_modified: 2019-01-01
---
{% include JB/setup %}

新年あけましておめでとうございます。

さて、2018年はどんな年でしたでしょうか。みなさんには昨今のEmacsの様子は、どう映ったでしょうか。**「安定」**でしょうか。正直な感想として**「停滞」**でしょうか。あるいは、*特に何も映らなかった*かもしれません。

昨年2018年5月には、Emacs本体の最新メジャーバージョンである**[GNU Emacs] 26.1**がリリースされました。

[MELPA]にも日本からの開発者が投稿したものもいくつも新たに収録されているほか、[@10sr]さんの[EditorConfig Emacs]や、[nim-mode]のYuta Yamadaさん([@yuutayamada])、僭越ながら筆者([@zonuexe])の[Emacs-PHP]など、世界中から利用されているLispパッケージには現在も日本人が主体となって継続的に開発されているパッケージがいくつもあります。

2016年以降はAyanokoji Takesiさん([@ayatakesi])の非常に大きな尽力により[Emacs日本語マニュアル]が提供され現在まで更新されています[^1]。

オンラインのコミュニティとしては[init.el読書会]が2014年から現在まで、ほぼ毎週欠かさず開催されています。

毎年の恒例行事ですが[Emacs Advent Calendar 2018]にも数多くの記事が投稿されました。

[![Emacs Advent Calendar 2018](/images/advent-calendar-2018.png)][Emacs Advent Calendar 2018]

特に、[use-package]の代替実装を一から独自実装している大学生のConaoさん([@conao3])の活動([use-packageからの移行のすゝめ - leaf.elでバージョン安全なinit.elを書く])は目をみはるものがあります。

話題としては[Language Server Protocol]への関心も高まっています。まだ言語ごとに実装のばらつきがあり、快適で安定した開発環境が確実に手に入るというには遠い状況もありますが、これから徐々に発展していくと思います。

--------

このように2018年もEmacs本体、世界、日本国内のコミュニティとしても、目立たずとも着実に進歩を遂げてきた年でした。

もしかすると2017年後半以降はるびきちさんによる[るびきち「新生日刊Emacs」]の更新が途絶えたことでEmacsについての新しい情報が日本語で提供されなくなったことで話題が減り、Emacsコミュニティの動きが見えにくくなったことはあるかもしれません。

従来オフラインでのミーティングとして継続開催されていた[関西Emacs勉強会]や[関東Emacs勉強会]は、2016年頃から途絶えてしまっていました。

しかしながら2017年に開催した[Emacs実践入門の出版記念イベント]や[2018年末に突発で開催した、いくつかのEmacs的なイベント][2018年末のEmacs的なイベント]で、コミュニティ活動への強い需要・要望があることは改めて確認できました。

## 2019年のEmacs JP

[Emacs JP]はGitHub Organizationとして2013年に発足して、日本国内のEmacs関係者の寄合や[メンテナ不在のパッケージをホスティングする場][Emacs JPで管理しているパッケージ]としては機能してきましたが、Emacsユーザー同士のコミュニティのハブや情報提供の主体としては機能していませんでした。

このEmacs JPのサイトに掲載されているページも2013〜2014年に更新されたきりで、近年は更新もありませんでしたが、今後は有用なコンテンツを継続的に更新していく予定です。

また、東京近郊でのリアルなイベントとしては2019年は[Shibuyaku-elisp]として、何らかのミートアップを毎月開催していく予定です。

### 皆さまへのお願い

日本語でのオンラインでのコミュニティとして、現在はSlackでの交流が活発に行われています。Slackは登録が必要ですが、 <https://slack-emacs-jp.herokuapp.com/> からどなたでも登録いただけます。

Emacsについて知りたいことや困りごとがあれば、お気軽にこちらのSlack teamで尋ねるか、[emacs-jp/issues]に日本語でissueを作成してと思います。ベストエフォートで対応いたしますし、必要であれば上流への報告や修正を取り次ぎます。

最後になりますが、コミュニティは所詮は人の集合に過ぎないので、誰かがやろうと言い出さなければ誰も動かず、事態は何も変りません。自分でやれることはないと思っても、その存在を伝えるだけでも、それを見た誰かが解決できることもあるかもしれません。

-----

#### 脚注

[^1]: GNU Emacs Lisp Reference Manualの翻訳も作業中です。

[@10sr]: https://github.com/10sr
[@ayatakesi]: https://github.com/ayatakesi
[@conao3]: https://github.com/conao3
[@rubikitch]: https://github.com/rubikitch
[@yuutayamada]: https://github.com/yuutayamada
[@zonuexe]: https://github.com/zonuexe
[EditorConfig Emacs]: https://github.com/editorconfig/editorconfig-emacs
[Emacs Advent Calendar 2018]: https://qiita.com/advent-calendar/2018/emacs
[Emacs JP]: https://github.com/emacs-jp
[emacs-jp/issues]: https://github.com/emacs-jp/issues/issues
[Emacs-PHP]: https://github.com/emacs-php
[Emacs日本語マニュアル]: https://ayatakesi.github.io/
[GNU Emacs]: https://www.gnu.org/software/emacs/
[Language Server Protocol]: https://microsoft.github.io/language-server-protocol/
[MELPA]: https://melpa.org/#/
[nim-mode]: https://github.com/nim-lang/nim-mode
[Shibuyaku-elisp]: https://shibuya-el.connpass.com/
[use-package]: https://github.com/jwiegley/use-package
[use-packageからの移行のすゝめ - leaf.elでバージョン安全なinit.elを書く]: https://qiita.com/conao3/items/82abfea7a4c81f946e60
[るびきち「新生日刊Emacs」]: http://emacs.rubikitch.com/
[関西Emacs勉強会]: https://atnd.org/events/49196
[関東Emacs勉強会]: https://kantou-emacs.doorkeeper.jp/events/42422
[Emacs実践入門の出版記念イベント]: https://medium.com/@tadsan/%E6%94%B9%E8%A8%82%E6%96%B0%E7%89%88-emacs%E5%AE%9F%E8%B7%B5%E5%85%A5%E9%96%80%E3%81%AE%E5%87%BA%E7%89%88%E8%A8%98%E5%BF%B5%E3%82%A4%E3%83%99%E3%83%B3%E3%83%88%E3%82%92%E9%96%8B%E5%82%AC%E3%81%97%E3%81%9F-8d9300225298
[2018年末のEmacs的なイベント]: https://medium.com/@tadsan/2018%E5%B9%B4%E6%9C%AB%E3%81%AEemacs%E7%9A%84%E3%81%AA%E3%82%A4%E3%83%99%E3%83%B3%E3%83%88-820e65ff76f
[Emacs JPで管理しているパッケージ]: https://emacs-jp.github.io/maintenances/
[init.el読書会]: http://emacs-jp.github.io/reading-init.el/

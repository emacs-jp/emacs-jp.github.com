---
layout: page
title: "rainbow-mode"
description: "色名, カラーコードの視覚化"
category: "face"
tags: ["face"]
date: 2013-05-06
last_modified: 2013-05-06
---
{% include JB/setup %}

## 概要

`rainbow-mode.el`は *red*, *green*などの色名や `#aabbcc`といったカラーコードから実際の
色を表示するマイナーモードです. 色名やカラーコードの背景がその色に設定されます.


Emacsには色一覧の表示を行う `list-colors-display`がありますが,
色名ベースになっているため細かい調整の際にはあまり向いていません. rainbow-modeは任意の
カラーコードについて色を表示してくれるので, 細かい faceの調整に向いています. faceの調整
以外にも HTML, CSSの作成時にも役立てることができます.


## スクリーンショット

![ranbow-mode](/images/face/rainbow-mode/rainbow-mode.png)


## インストール方法

rainbow-modeは MELPAに登録されているので, package.elを使ってインストールすることができます.


## 各種コマンド

#### rainbow-mode

rainbow-modeの有効, 無効を切り替える.

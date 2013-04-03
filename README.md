# Emacs User JP

## このサイトについて
Emacs User JP は Emacs と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。

[http://emacs-jp.github.com/](http://emacs-jp.github.com/)


## ページ作成者向け情報

### 準備

#### Python関連

```
% pip install pygments
```

#### Ruby関連

```
% git clone git@github.com:emacs-jp/emacs-jp.github.com.git
% cd emacs-jp.github.com
% bundle install
```

#### Python3を利用する場合

pygments.rbに含まれる `mentos.py`が Python3に対応していないので,
`python`コマンドが python3である場合, 正しくページを生成できない.
そのため Python2を使うよう切り替えるか, `mentos.py`の shebangを
書き換える必要がある.


### ページ生成
`--auto`オプションつきで起動(デフォルトで有効)した場合, ページ生成時に
エラーがあっても特に何も表示されない. もし作成したページがうまく表示されない
場合は, `--no-auto`オプションをつけてエラーなくページが生成されるかを
確認するとよい.

```
% jekyll --no-auto
```

### 動作確認
サーバを起動し, [http://localhost:4000](http://localhost:4000)で確認.
ポート番号を変える場合 `--server`オプションのあとにポート番号を指定する

```
% jekyll --server # デフォルトで --autoが有効
```

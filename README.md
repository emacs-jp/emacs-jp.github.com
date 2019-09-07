# Emacs JP

## このサイトについて

**Emacs JP**は[GNU Emacs]と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。

<http://emacs-jp.github.io/>

[GNU Emacs]: https://www.gnu.org/software/emacs/

## Slack - <https://emacs-jp.slack.com>

参加したい方は https://slack-emacs-jp.herokuapp.com/ からサインアップできます。

## ページ作成者向け情報

### 準備

RubyおよびBundlerが必要です。詳細は[Using Jekyll as a static site generator with GitHub Pages - GitHub Help]をお読みください。

[Using Jekyll as a static site generator with GitHub Pages - GitHub Help]: https://help.github.com/en/articles/using-jekyll-as-a-static-site-generator-with-github-pages

```
% git clone git@github.com:emacs-jp/emacs-jp.github.com.git
% cd emacs-jp.github.com
% bundle install
```

### 動作確認

サーバーを起動し、 <http://localhost:4000> で確認。
ポート番号を変える場合 `--port` オプションのあとにポート番号を指定してください。

```
% bundle exec jekyll serve
```

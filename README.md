# Emacs JP

## このサイトについて

**Emacs JP**は[GNU Emacs]と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。

<http://emacs-jp.github.io/>

[GNU Emacs]: https://www.gnu.org/software/emacs/

## Slack - <https://emacs-jp.slack.com>

参加したい方は https://slack-emacs-jp.herokuapp.com/ からサインアップできます。

## ページ作成者向け情報

### Docker

[docker](https://www.docker.com/)及び[docker-compose](https://docs.docker.com/compose/)を利用してローカルの環境に影響を与えずに、簡単にローカルサーバーを立てることができます。
```sh
# ローカルサーバーを立てる (以下のいずれかを選んで実行する)
make up            # localhost:4000 でローカルサーバーを立てる
make serve         # make upのエイリアス
PORT=3000 make up  # localhost:3000 でローカルサーバーを立てる

# ビルドログを見る
make log

# 後片付け
make down
```

### システムのRuby

RubyおよびBundlerが必要です。詳細は[Using Jekyll as a static site generator with GitHub Pages - GitHub Help](https://help.github.com/en/articles/using-jekyll-as-a-static-site-generator-with-github-pages)をお読みください。

```
git clone git@github.com:emacs-jp/emacs-jp.github.com.git
cd emacs-jp.github.com
bundle install
bundle exec jekyll serve         # localhost:4000 でローカルサーバーを立てる
```

ポート番号を変える場合 `--port` オプションのあとにポート番号を指定してください。

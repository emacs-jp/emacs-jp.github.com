# Emacs JP

## このサイトについて

**Emacs JP**は[GNU Emacs]と日本語に関わるあらゆるリソースを集約することを目的としたコミュニティサイトです。

<http://emacs-jp.github.io/>

[GNU Emacs]: https://www.gnu.org/software/emacs/

## Slack - <https://emacs-jp.slack.com>

参加したい方はこちらの [招待リンク](https://join.slack.com/t/emacs-jp/shared_invite/zt-1id7hvbxh-~n_wSBrdrHMk8~Ge8Fp3IQ) からサインアップできます。

## ページ作成者向け情報

### Docker

[docker](https://www.docker.com/)及び[docker-compose](https://docs.docker.com/compose/)を利用してローカルの環境に影響を与えずに、簡単にローカルサーバーを立てることができます。
```sh
# ローカルサーバーを立てる (以下のいずれかを選んで実行する)
make up                # localhost:4000 でローカルサーバーを立てる
make serve             # make upのエイリアス
PORT=3000 make up      # localhost:3000 でローカルサーバーを立てる

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

cd docs
bundle exec jekyll serve         # localhost:4000 でローカルサーバーを立てる
```

ポート番号を変える場合 `--port` オプションのあとにポート番号を指定してください。

### コンテンツ作成のルールとtips

記事はブログポストとページの2種類があります。
- ブログポストは `/docs/_posts` 以下に `%Y-%m-%d-<name>.md` というファイル名で作成します。
- ページは以下のルールで作成します
  - `docs/env/<lang>.md`: コンピューター言語 *lang* 向けの環境構築について
  - `docs/packages/<pkg>.md`: パッケージ *pkg* の紹介及び設定について
  - `docs/tips/<name>.md`: 雑多な話題について

なにか疑問や提案があればissueで相談して頂ければと思います。


### org-mode

[org-mode](https://orgmode.org/)を用いてブログを生成できます。

1. `docs/org/config.el` を `load`
2. `docs/org/` 以下に作りたい階層と同じ構造でorgファイルを作成、編集

   新規ファイル作成後、 `M-x my/insert-emacs-jp-template` でテンプレートを挿入できます。
3. `M-x my/publish-emacs-jp` で `docs/org/` 以下のorgをmdに一括変換

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
# コンテナを取得・更新する
make pull

# ローカルサーバーを立てる (以下のいずれかを選んで実行する)
make up                # localhost:4000 でローカルサーバーを立てる
make serve             # make upのエイリアス
PORT=3000 make up      # localhost:3000 でローカルサーバーを立てる

# ビルドログを見る
make log

# 公開用の設定でビルドを検証する（成果物は終了時に削除される）
make build

# Ruby と読み込まれた gem の版を確認する
make versions

# 後片付け
make down
```

Docker と CI は、GitHub 管理の公開 Action が参照する Pages コンテナを使用します。
`make build` は公開 Action と同じコンテナ内の `github-pages build` を実行します。
Docker のプレビューも同じ gem と Pages の設定を使用し、`JEKYLL_ENV=production` で動作します。
このプレビューでは Pages の safe モードにより、`docs/_plugins/debug.rb` を含む独自プラグインは読み込まれません。
プレビューと検証用ビルドの出力は別々のコンテナ内に置くため、同時に実行できます。

Docker と CI の gem の版は、GitHub が管理するコンテナに従います。
CI は `actions/jekyll-build-pages@v1` が参照するイメージと Compose の指定を比較します。
不一致になった場合、公開ビルドのログも確認して `docker-compose.yml` のイメージを更新してください。
取得済みイメージを更新するには `make pull` を実行してください。
コンテナは公開環境に合わせて `linux/amd64` を指定するため、ARM マシンでは Docker のエミュレーションが必要です。

ローカルには公開時の認証情報はありません。
GitHub API の認証が必要な場合は、`JEKYLL_GITHUB_TOKEN` 環境変数を指定してください。
公開時点のコンテナ更新や GitHub API の応答まで固定する構成ではありません。

### システムの Ruby

システムの Ruby と Bundler でも記事をプレビューできます。
導入方法は[GitHub Pages サイトを Jekyll でローカルにテストする公式手順](https://docs.github.com/ja/pages/setting-up-a-github-pages-site-with-jekyll/testing-your-github-pages-site-locally-with-jekyll)を参照してください。

```sh
git clone git@github.com:emacs-jp/emacs-jp.github.com.git
cd emacs-jp.github.com
bundle install

cd docs
bundle exec jekyll serve
```

ポート番号を変える場合は `--port` オプションを指定してください。
`bundle install` が作るルートの `Gemfile.lock` は Git に追加しません。
この手順で使う gem の版は公開環境と異なる場合があるため、公開用のビルドは CI または `make build` で確認してください。

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

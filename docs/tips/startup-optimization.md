---
layout: page
author: zk-phi
title: 'Emacs の起動時間を""詰める""'
date: 2020-09-07
last_modified: 2020-09-07
---
{% include JB/setup %}

Emacs はプラグインを増やしていくと起動に何秒もかかって重い、という話をみることがあります。

しかし、考えてみれば Emacs には 1000 以上の Emacs Lisp ファイルが初めから同梱されているわけで、そこに数十のプラグインを足しただけで爆裂に遅くなるのは、なにか設定にも問題がある気がします。

この記事では、 Emacs の起動時間を詰めるために今までに試してきた、小技や大技たちを紹介します。

自分用にメンテしているフレームワーク [setup.el](https://github.com/zk-phi/setup) で活用しているテクニックが主なので、そちらを試してみて欲しい気持ちもありますが、それぞれの Tips 単体でも価値があると思うので記事にもまとめてみることにしました。

----

参考までに、私の設定ファイルは [こちら](https://github.com/zk-phi/dotfiles) にあります。
`init.el` 本体だけで 4500 行くらい (コマンド類は別ファイルで定義) ありますが、 Mac 上で GUI の Emacs を立ち上げた時の起動時間 (`emacs-init-time`) は速い時で 0.38 秒でした。

## テクニック紹介の前に
### emacsclient について

Emacs を起動しっぱなしにして、そもそも何度も起動しないようにするという手法 (emacsclient) もあります。

起動自体を速くすることとは別路線の手法になるので、この記事ではスコープ外としますが、試してみる価値はあると思います。

### init.el のプロファイリングについて

チューニングを行う際は、何がボトルネックになっているのかを調べるのが大事です。

`init.el` の先頭に

``` emacs-lisp
(require 'profiler)
(profiler-start 'cpu)
```

末尾に

``` emacs-lisp
(profiler-report)
(profiler-stop)
```

などと書いておくことで、 `init.el` のプロファイルを取ることができます。

どのパッケージのロードが遅いのか、どの関数の処理が遅いのかを調べながらチューニングしていくことで、より効率よく起動時間を短縮してゆくことができます。

行き詰まった時は活用してみてください。

では、いよいよ本題に入っていきます。

## バイトコンパイル

全人類がやるべき設定その１です。

`init.el` や各種パッケージのソースコードを、 Emacs が処理しやすい形 (`.elc`) にあらかじめ変換しておきます。

`M-x byte-compile-file` でコンパイルできるので、片っ端からコンパイルしましょう。コンパイルされたファイルがあれば Emacs が勝手にそちらを優先してロードします。

dired を扱える方は、 `dired-do-byte-compile` という一括コンパイルコマンドがあるので、手動で入れているパッケージがたくさんある場合は便利です。

ソースコードを編集した時は再度バイトコンパイルするのを忘れないようにしましょう (自動化も手だと思います)。

## autoload と with-eval-after-load

全人類がやるべき設定その２です。

### 正統派な使い方

たとえば Perl 言語の設定は Perl のファイルを開くまで必要ないですし、Web ブラウザ eww の設定は eww を起動するまで必要ありません。それらを一度も使うことなく Emacs を閉じることがあれば、設定にかかった時間はまるまる無駄になってしまいます。

これを防ぐのが `autoload` と `with-eval-after-load` です。

例を挙げます：

``` emacs-lisp
;; hoge パッケージを読み込む
(require 'hoge)

;; hoge パッケージの初期設定をする
(hoge-initialize)

;; hoge パッケージのコマンドにキーを割り当てる
(global-set-key (kbd "C-x h") 'hoge-run)
```

このようなコード片は、たいていの場合、次のように書き換えることができます。

``` emacs-lisp
;; hoge-run コマンドが実行されそうになったら慌てて hoge パッケージを読み込む
(autoload 'hoge-run "hoge")

;; hoge パッケージが読み込まれたらすぐに初期設定をする
(with-eval-after-load 'hoge
  (hoge-initialize))

(global-set-key (kbd "C-x h") 'hoge-run)
```

書き換え後のコード片は、キーバインドの設定だけを起動時に行い、パッケージの読み込みは `hoge-run` コマンドが実行されるまで遅延します。パッケージが読み込まれると、 `with-eval-after-load` が `hoge-run` の処理に移る前に割り込んで初期設定を実行するので、ほとんどの場合これで問題ありません。

この書き換えによって、 `hoge-run` コマンドがそもそも使われなければ `hoge` パッケージの設定はそもそも行われないし、仮に使うとしても起動時にすべてのパッケージの設定を行うよりはずっと起動時間が短くなります。

特定の言語だけで使用するパッケージなども同様に、トリガーだけを設定しておいて読み込みを遅延することができます。

``` emacs-lisp
(autoload 'sugoi-python-minor-mode "sugoi-python")
(add-hook 'python-mode-hook 'sugoi-python-minor-mode)
```

「起動時に必要とは限らないな」というパッケージに片っ端からこれを適用しましょう。突き詰めると、フォントやカラースキームの設定、ごく基本的なパッケージの読み込み（「かっこを光らせる」など）くらいしか、起動時に必須な設定はないことに気づくと思います。となれば当然、起動はかなり速くなります。

----

ところで自分はずっと `with-eval-after-load` (`eval-after-load`) の第一引数をファイル名の文字列にして使っていたのですが、

``` emacs-lisp
(with-eval-after-load "hoge"
  '(hoge-initialize))
```

シンボル (`'hoge`) にしておいた方がわずかに速いことに最近気づきました。前者はパッケージが読み込まれたかのチェックに正規表現を使うのに対して、後者はたんに `eq` で比較されるためです。オーダーは変わらないですが定数倍が良いです。

### autoload を使った小技

ある自分で実装した関数があって、これがあるパッケージ `foo` を利用している場合、次のように書くことで軽率に遅延することができます。

``` emacs-lisp
(autoload 'my-special-foo-command "foo")

(with-eval-after-load 'foo
  (defun my-special-foo-command ()
    ...))
```

こう書いても微々たる差かもですが…。

``` emacs-lisp
(defun my-special-foo-command ()
  (require 'foo)
  ...)
```

まとまった数の自作コマンドがある場合は、それらをまとめて別ファイルに移動してしまい、まるっと `autoload` するのも手です。

## タイマーを使った擬似非同期

起動時に必要なパッケージ以外を片っ端から `autoload` してもまだ起動に時間がかかる (起動時に必要なパッケージだけでもそれなりにある) 場合は、それらを「少しずつ」セットアップしていくと快適です。

すなわち、全てのセットアップが完了してから初めてユーザーの入力を受け付けるのではなく、ユーザーの入力も受けつつ裏で並行してセットアップを進めることで、体感の起動速度＝待たされ感が改善します。

最近入ったスレッド機能を使うことも考えられますが、まだ不安定そうな雰囲気もあるので、私はタイマーを使っています。スレッドを使っても現状は結局ノンプリエンプティブなので、ユーザーの入力を少しずつブロックしながらやっていくことには変わりなくて、タイマーに対してさほど優位点がないように思います。

タイマーを使う場合は、以下のように `after-init-hook` あたりでタイマーをスタートさせて、各設定項目を細切れに処理してゆけば ok です。

``` emacs-lisp
;; 非同期に行う設定のリスト
(defvar my-delayed-configurations nil)

;; 0.1 秒ずつ間隔を開けながら消化
(defvar my-delayed-configuration-timer nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq my-delayed-configuration-timer
                  (run-with-timer
                    0.1 0.1 ; 0.1 秒ごとに
                    (lambda ()
                      (if my-delayed-configurations ; まだやることがあれば
                          (eval (pop my-delayed-configurations)) ; 一個やる
                        (cancel-timer my-delayed-configuration-timer)))))))
```

タイマーで少しずつ間隔を開けながら処理することで、ユーザーからの入力を (Emacs が) 処理する余地が生まれるので、体感の待たされ感が軽減します。

「非同期に実行する設定のリスト」に要素を追加するためのマクロを用意しておくと便利です。

``` emacs-lisp
(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(push ',(cons 'progn body) my-delayed-configurations))

(with-delayed-execution
  (require 'foo)
  (foo-mode 1))
```

## 便利ライブラリへの依存を見直す

Emacs Lisp には `cl-lib` をはじめ、便利関数をまとめたライブラリが色々あります。これらを使うことで設定ファイルをスマートに書くことができますが、ロードはもちろん無料ではありません。

これらの便利ライブラリを使うとき、「使うのはマクロだけにとどめる」ことを意識すると起動の高速化になります。たとえば `cl-lib` で定義されている `cl-case` は関数ではなくマクロですが、一方 `cl-every` は関数です。マクロはコンパイルの時にマクロを使わない形の式に展開されるので、一度コンパイルしてしまえば、起動時にそのマクロが定義されている必要はありません。

依存をマクロだけにすることができると、

``` emacs-lisp
(require 'cl-lib)
```

を以下のように書き換えて、

``` emacs-lisp
(eval-when-compile
  (require 'cl-lib))
```

ロードをコンパイル時に限ることができます。これによって便利マクロたちを実質無料で使用することができ、起動は当然速くなります。

あるシンボルが関数なのかマクロなのか調べたい場合は、 `M-x describe-function` が便利です。

### パッケージがロードされていないことの確認

「起動時に絶対ロードしたいパッケージ」の中に `cl-lib` (など) の関数に依存しているものがあれば、せっかく頑張って `init.el` から消したところで結局ロードされてしまいます。

パッケージがロードされているかどうかは `featurep` 関数で調べることができるので、もし Emacs 起動直後に `(featurep <パッケージ>)` が `t` を返すようであれば、何か別のパッケージが `require` してしまっています。

そのようなパッケージは上の擬似非同期ロードのテクニックで遅延してしまうのがおすすめです。

もし犯人が見つからない場合は、次のように `load-history` から犯人を探すこともできます。

``` emacs-lisp
(let (packages)
  (dolist (row load-history)
    (dolist (elm row)
      (when (and (consp elm)
                 (eq (car elm) 'require)
                 (eq (cdr elm) 'cl-lib))
        (push (car row) packages))))
  (print packages))
```

## コンパイル時計算
### 単純なコンパイル時計算

`init.el` 内に登場する純粋な (副作用のない) 計算はコンパイル時に行ってしまうことでわずかですが高速化できます。

たとえば重い (しかしキャッシュしても問題ない) 計算 `omoi-keisan` によって定数 `my-super-constant` の値が決定される場合：

``` emacs-lisp
(defconst my-super-constant (omoi-keisan))
```

これをコンパイル時計算することで起動を高速化できます。

``` emacs-lisp
(defconst my-super-constant
  (eval-when-compile (omoi-keisan)))
```

長いので適当な短い別名を割り当てておくと軽率に使えて良いと思います。起動時に計算し直す必要がないものには片っ端からつけましょう。

``` emacs-lisp
(defalias '! 'eval-when-compile)
(defconst my-super-constant (! (omoi-keisan)))
```

### コンパイル時ループアンローリング

`init.el` の中に `dotimes`, `dolist` など典型的な形のループ処理があって、かつループの範囲が静的に決まっている場合は、アンローリングしてしまった方が変数束縛などのコストがない分、効率がいいです。

``` diff
-(dolist (cmd '(narrow-to-region
-               dired-find-alternate-file
-               upcase-region
-               downcase-region))
-  (put cmd 'disabled nil))
+(put 'narrow-to-region 'disabled nil)
+(put 'dired-find-alternate-file 'disabled nil)
+(put 'upcase-region 'disabled nil)
+(put 'downcase-region 'disabled nil)
```

とはいえループの範囲が広い場合や、ループの中で行う処理が複数行にわたるような場合、やっぱり同じコードをたくさんコピペするのは気が引けます。そこで、この変換をコンパイル中にしてしまうマクロを用意しておくと便利です。変数名を指定できる必要はあまりないので、私はアナフォリックマクロ風に、 `,it` で参照できるようにしています。

``` emacs-lisp
;; setup.el より
(defmacro !foreach (list &rest body)
  "Eval BODY for each elements in LIST. The current element can
be referred with `,it'."
  (declare (indent 1))
  `(progn ,@(mapcar
             (lambda (elem)
               (macroexpand-all
                (if (cadr body) `(progn ,@body) (car body))
                `((,'\, . (lambda (&rest body) `',(funcall `(lambda (it) ,@body) ',elem))))))
             (eval list))))

(!foreach '(narrow-to-region
            dired-find-alternate-file
            upcase-region
            downcase-region)
  (put ,it 'disabled nil))
```

---

実装が回りくどく見えますが、単純な `it` への参照だけでなく「`it` を含む式」もコンパイル時計算で展開できるようになっているためです。

``` emacs-lisp
(!foreach '(narrow-to-region
            dired-find-alternate-file
            upcase-region
            downcase-region)
  (message ,(symbol-name it)))
```

`macroexpand-all` の第二引数を利用すると、このように一時的にマクロ定義を `flet` するような使い方ができてごく稀に便利です。ごく稀ですが…。

## 環境依存バイトコンパイル

コンパイル後のファイル `init.elc` のポータビリティを諦めれば、すなわち使うマシンごとにいちいちコンパイルすることにすれば、手間と引き換えにさらなるチューニングができるようになります。

### コンパイル時 load-path 解決

通常、 `require` や `load` は変数 `load-path` に登録されているディレクトリを探索して目当てのパッケージを探します。しかし頻繁にパッケージの置き場を変えることがないのであれば、毎回この探索をするのは無駄です。

一応これらの関数はパッケージの場所をフルパスで指定することもできるのですが、とはいえベタ書きは避けたい気持ちもあります。

そこで、コンパイル時にそのマシンの `load-path` からパッケージを探索して、結果をキャッシュしておくようなオレオレ `load` マクロを定義しておくと便利です。

``` emacs-lisp
(defmacro my-load (library &rest args)
  (let ((abs (locate-library library)))
    `(load ,abs ,@args)))
```

マシンごとにコンパイルする必要はありますが、一度コンパイルしてしまえばバイトコードにフルパスが埋め込まれるので、起動時の `load-path` の探索は不要になります。

### コンパイル時条件分岐

同様にバイトコードのポータビリティを諦めることでできるもう一つの最適化は、 OS ごとの設定などの「条件分岐」のコンパイル時計算です。

たとえば次のように OS ごとに設定をディスパッチするようなコード片があったとき：

``` emacs-lisp
(if (eq system-type 'windows-nt)
    ...
  ...)
```

もしどうせコンパイルした時と同じマシンで使うとわかっているなら、起動時に毎回この条件のチェックを行うのは無駄です。

そこで、コンパイル時に中身を展開してしまうような条件分岐マクロを用意しておくと便利です。

``` emacs-lisp
(defmacro !if (test then &rest else)
  (declare (indent 2))
  (if (eval test) then `(progn ,@else)))
```

私は「フォントのインストール状況によってよしなにフォントを選ぶ」設定や、「ファイルが存在する場合だけ読み込む」設定などもこれで書いています。意外と活用できるところがあると思います。

## ちょっとしたハックで不要な処理を省く
### message を一時的に無効にする

IO は重い処理なので、もしセットアップ時に重要ではないメッセージをやたら表示するパッケージがある場合は、黙らせておくことで若干起動を高速化できます。

以前は `message` 関数を `flet` などで乗っ取る実装をしていましたが、現代の Emacs には `inhibit-message` という便利変数があるのでこれを活用します。

``` emacs-lisp
(let ((inhibit-message t))
  (require 'foo)
  (foo-initialize))
```

### Magic File Name を一時的に無効にする

Emacs にはファイル名に応じて IO に独自の処理を挟む機構 (Magic File Name) があります。これはリモートのファイルにシームレスにアクセスするためなどに使われますが、起動処理の途中で欲しくなることはまずありません。しかし使わない場合でもファイル名のチェックは走ってしまうので、パッケージをロードする際などにはオーバーヘッドが発生します。

起動処理の間だけこれを無効にしておくことで、わずかに起動を速くすることができます。

そのためには、 `init.el` の頭で

``` emacs-lisp
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
```

`init.el` のおわりで

``` emacs-lisp
(setq file-name-handler-alist my-saved-file-name-handler-alist)
```

のように設定しておけば ok です。

設定内容を妥協しなくても、ただ書いておくだけでちょっと速くなるのでおすすめです。

### GC を減らす

Emacs Lisp ではメモリの free を人間が指示しないので、代わりにいらなくなったゴミは GC が集めます。しかし起動処理の途中でこれが走ってしまうと余計なオーバーヘッドになります。

そこで、起動の間だけ GC が走らないようにしておき、あとでメモリが必要になった時にまとめてお掃除してもらう方法があります。

Magic File Name と同様に、 `init.el` の頭で

``` emacs-lisp
(setq gc-cons-threshold most-positive-fixnum)
```

などとして GC の閾値を闇雲にでかい値にしておくことで GC を実質止めることができます。

もちろんこのままでは無限にメモリを食いつぶしてしまい、それはそれでパフォーマンスにも悪影響なので、 `init.el` のおわりでそれらしい値に戻しておく必要があります。

``` emacs-lisp
(setq gc-cons-threshold 16777216) ; 16mb
```

これもとりあえず書いておけば速くなる系なのでおすすめです。

### early-init.el

Emacs 27 からは `init.el` の他に `early-init.el` というファイルも持てるようになりました。これは Emacs が起動した直後、 GUI の構築やパッケージのロードなどが行われるよりも前の、かなり初期の段階でロードされる設定ファイルです。

もともとパッケージシステム自体の設定などを行う用途で導入されましたが、 GUI の基本的な設定もここに入れておくと若干の効率化になります。

たとえばメニューバーやツールバーがいらない場合、 `init.el` で次のように設定するのが一般的だと思います。

``` emacs-lisp
(menu-bar-mode -1)
(tool-bar-mode -1)
```

しかしこれでは一度バー類が構築されてからまたすぐに消すという処理になってしまうので、無駄な計算が生じます。

`early-init.el` の中で、そもそもこれらのバー類はデフォルトで無効なものなのだと定義してしまえば：

``` emacs-lisp
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
```

この無駄な処理を省くことができます。

また、もし Emacs をいつも決まったフレームサイズで起動する場合 (フルスクリーンなど)、以下の行も `early-init.el` に加えておくと

``` emacs-lisp
(setq frame-inhibit-implied-resize t)
```

フォントが読み込まれたタイミングなどでフレームサイズの再計算が入ってチカチカする現象を防げます。

## Emacs Lisp の細かな最適化
### 安全な関数を諦める

Emacs の設定を書いていると、 `add-hook` や `add-to-list` などの関数によく出会うと思います。これらの主な機能はリストに値を追加することですが、このときリストの要素に重複が出ないようにチェックもしてくれます。

一見便利なように思えますが、重複チェックはリストの全要素との比較が必要なのであまり軽い計算とはいえず、しかもあらかじめ重複しないとわかっている場合は無駄になってしまいます。

そこで、私は明らかに重複しないとわかっている（あるいは重複しても問題にならない）場合は `add-to-list` の代わりに `push` を使うようにしています。

``` emacs-lisp
(push '("\\.scad$" . scad-mode) auto-mode-alist)
```

`add-hook` には重複チェックに加えて、「リストじゃない場合はリストに変換する」などの機能も備わっているので、次のようなオレオレ雑 `add-hook` を用意しておくと便利です。

``` emacs-lisp
(defun my-function-list-p (val)
  (or (null val) (and (consp val) (not (eq (car val) 'lambda)))))

;; 重複チェックしない add-hook
(defun my-add-global-hook (hook fn)
  (let ((oldvalue (when (default-boundp hook) (default-value hook))))
    (if (my-function-list-p oldvalue)
        (set-default hook (cons fn oldvalue))
      (set-default hook (list fn oldvalue)))))
```

### defsubst

Emacs Lisp にも関数のインライン展開があります。

`defun` と同じ要領で `defsubst` を使って定義でき、コンパイラーに対してインライン化してもいいよという指示を出せます。小さな関数に適用していくことで、関数呼び出し分のコストを削減することができます。

### lexical-binding

Emacs Lisp は現時点ではデフォルトで動的束縛です。

``` emacs-lisp
(defvar fn
  (let ((a 1))
    (lambda (x) (+ x 1))))

(funcall fn 3) ;; => 4

(let ((a 2))
  (funcall fn 3)) ;; => 5
```

が、最近はローカル変数をまとめて静的束縛にすることができる `lexical-binding` オプションが実装されているので、静的束縛を利用することもできます。

``` emacs-lisp
;;; -*- lexical-binding: t -*-

(defvar fn
  (let ((a 1))
    (lambda (x) (+ x 1))))

(funcall fn 3) ;; => 4

;; 関数が定義された時の a を参照し続ける
(let ((a 2))
  (funcall fn 3)) ;; => 4
```

変数が静的に束縛されるとわかっていると、たとえば [`let` がただの stack push / pop で済むなど](https://emacs.stackexchange.com/questions/2129/why-is-let-faster-with-lexical-scope)、最適化の余地が広がるので効率よく実行できます。

動的束縛でないと困るという場面はさほど多くないので、「俺、いま動的束縛活用してんなー」という自覚なしに書いている `init.el` はおそらく `lexical-binding: t` にしても問題なく動くと思います (動かない場合、たいてい警告が出るので従いましょう)。

---

自分はずっと、動的束縛こそ Emacs Lisp の味だぜと思って `lexical-binding` を使ってこなかったのですが、静的束縛になるのは変数束縛だけだと知って宗旨替えしました。

advice や関数再定義こそ Emacs Lisp の真髄だなあと思うことがあります。

### オブジェクトの実体を意識する

Emacs Lisp には似たような機能の関数が複数あることがあります。それぞれの実装を理解して使い分けることで、より効率の良い設定ファイルを書くことができます。

#### 等値比較

`eq`, `eql`, `equal` はどれも等値比較の関数ですが、

- `eq` ... オブジェクトの実体が同じ
- `eql` ... オブジェクトの実体が同じか、数値として等しい
- `equal` ... オブジェクトの実体が同じか、数値として等しいか、文字列として等しいか、リストや配列の要素がすべて等しい

のように判定の緩さと実行効率に違いがあります。特に `equal` については、コレクションの全ての要素を確認するので他の比較に比べて顕著に遅いです。判定の目的に応じて適切に使い分けましょう。

比較の対象が数値や文字列とあらかじめわかっている場合は、 `=`, `string=` などの専用の関数もあります。

要素が配列に含まれているかを検査する関数など、内部的に等値比較をおこなう関数群にも同様に `memq`, `memql`, `member` などのバリエーションがあるので、使い分けましょう。

#### コンスセルを理解する

LISP のリストは、空リスト (`nil`) に「コンスセル」（２要素タプル）を被せていくことで作られます：

``` emacs-lisp
(cons 1 (cons 2 (cons 3 nil))) ;; = '(1 2 3)
```

要はリンクトリストです。

たとえば二つのリストを連結したいとき、 Emacs Lisp では `concat` または `nconc` を使うことができますが、それぞれ次のような違いがあります。

- `(concat A B)` ... `A`, `B` の要素をすべて並べた新しいリストを作る
- `(nconc A B)` ... リスト `A` の末尾を `nil` からリスト `B` に置き換える

後者は新しいコンスセルをアロケートしないので効率が良いです。

ただし、元のリスト `A` は破壊されてしまいます。また `B` も破壊こそされませんが、一方でコピーもされません。たとえば以下のようなコードを実行すると：

``` emacs-lisp
(defconst a '(1 2 3))
(defconst b (nconc '(1 2 3) a)) ;; => '(1 2 3 1 2 3)
(defconst c (nconc '(3 2 1) a)) ;; => '(3 2 1 1 2 3)
```

メモリ上には木構造のようなものができあがります。

``` text
(b) 1 - 2 - 3
             \
              +- (a) 1 - 2 - 3
             /
(c) 3 - 2 - 1
```

ここでたとえばリスト `a` の先頭 `1` を破壊的に書き換えると：

``` emacs-lisp
(setcar a 9)
(print a) ;; => '(9 2 3)
```

残りのリストたちも書き換わります。

``` emacs-lisp
(print b) ;; => '(1 2 3 9 2 3)
(print c) ;; => '(3 2 1 9 2 3)
```

``` text
(b) 1 - 2 - 3
             \
              +- (a) 9 - 2 - 3
             /
(c) 3 - 2 - 1
```

コンスセルやリストはいたるところで使う LISP の基本オブジェクトなので、このような仕組みを理解した上で上手に活用していくと、よりパフォーマンスの出る Emacs Lisp を書くことができます。

## おまけ：飛び道具たち

本文では紹介しなかった離れ技たちのアイデアだけ、最後に紹介しておきます。

難しいしちゃんと動く保証もないのでオススメはしません。

### コンパイル時パッケージ読み込み

「起動時に必須なパッケージ」をせめて効率よくロードするために試したテクニックです。

ロード対象のパッケージのコードをコンパイル中に読み込んでしまって、コンパイル結果の `init.elc` に直接埋め込んでしまいます。

これによりファイルを開きにいくコストが削減できると思いました。

効果はわりとシステムによってまちまちのようで、 Windows ではかなり効果がでたものの、他はイマイチでした。 Windows 機を使うことが減ったのと、いかんせんかなりワイルドなテクニックで動作も不安定なところがあったので使うのはやめました。

一応 `setup.el` に名残のコードがあるので、興味のある方は読んだり使ったりしてみてください。自前で `load-history` をメンテしたり、なかなか過激なコードになっているので面白いとは思います。

### ポータブルダンパー

Emacs 27 から unexec に代わって使われるようになった pdump を悪用するアイデアです。

「起動時に必須なパッケージ」のロードが終わった状態の Emacs を作って、それを pdump します。

そもそも「カスタマイズ後の Emacs を正しく pdump することは目標としていない」と明言されているので、かなり工夫して使う必要はありそうですが、もしうまくいけば相当な効率化が期待できます。

やってみたいなとは思いつつまだできていません。

## まとめ

Emacs の起動時間を詰めるために使える小技や大技たちを紹介しました。

なにか他にも面白いテクニックや話題を知っている方は、ぜひコメントや記事を書いて教えてもらえると嬉しいです。

よき Emacs ライフを！

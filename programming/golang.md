---
layout: page
title: "Goプログラミングの環境構築"
description: "Goプログラミングの環境構築"
category: "programming"
tags: ["go", "golang"]
---
{% include JB/setup %}

## 概要

Emacsでの Go言語をプログラミングする際の環境構築について示す.

## 事前準備
コード補完のために `gocode`, 定義元へのジャンプのために `godef`をインストールしておく

```
 % go get -u github.com/nsf/gocode
 % go get -u code.google.com/p/rog-go/exp/cmd/godef
```

## 推奨パッケージ

* [go-mode](https://code.google.com/p/go/)
* [go-autocomplete](https://github.com/nsf/gocode)
* [go-eldoc](https://github.com/syohex/emacs-go-eldoc)

これらのパッケージはすべて [MELPA](http://melpa.milkbox.net/packages/)からインストールできる


### NOTE

* [改良版 go-mode](https://github.com/dominikh/go-mode.el)


## go-modeの各種コマンド(利用頻度が高いもの)

### go-import-add (Default:  `C-c C-a`)

指定したパッケージの import文を追加する.

### godef-jump (Default: `C-c C-j`)

関数, インタフェース, 変数等の定義元にジャンプする. コードリーティング等,
コードの流れを把握したいときに非常に有用となる. `godef`コマンドが必要に
なるので, 事前にインストールしておく. `M-,`で元の場所に戻れる.

#### MEMO

`M-.`に bindすると, 他のモードと同じになって, 使いやすいかもしれない.


### godef-describe(Default: `C-c C-d`)

後述の `go-eldoc`を使うことで, 自動的に情報を表示できる.

### gofmt

`gofmt`コマンドを現在のバッファに適用する.

保存時自動的にこのコマンドを実行したい場合は 以下のように `gofmt-before-save`を保存のための
hookに登録すればよい.

```
(add-hook 'before-save-hook 'gofmt-before-save).
```


## [go-autocomplete](https://github.com/nsf/gocode)

gocodeを利用した. [autocomplete](https://github.com/auto-complete/auto-complete)の Go拡張. 高速であり,
精度の高い補完能力を持つ.

![go-autocomplete-screenshot](http://farm4.staticflickr.com/3797/9001480371_d3a0ef1da4_o.png)



## [go-eldoc](https://github.com/syohex/emacs-go-eldoc)

Go用の `eldoc`. カーソル下の関数のシグネチャ, typeされた型の情報を minibufferに表示する.

![go-eldoc-screenshot](https://github.com/syohex/emacs-go-eldoc/raw/master/image/go-eldoc1.png)


## その他パッケージ

### [flycheck](https://github.com/flycheck/flycheck)

flycheckは Goが標準でサポートしている.

### [flymake-go](https://github.com/robert-zaremba/flymake-go)

Go用 flymake

### [go-errcheck](https://github.com/dominikh/go-errcheck.el)

`compilation`インタフェースでエラーを指摘するツール. `next-error`コマンドなどで
エラー箇所に移動できる. Goは指摘されるエラーが多いので, flymakeの類だと, 頻繁に
エラーが指摘されてしまう. 一気にコードを書きって後でエラーをまとめて修正したい
というような方はこちらのパッケージの方が良いと思われる.


### [go-direx](https://github.com/syohex/emacs-go-direx)

`direx`を使った Goのツリービューア


## helm-tips

```common-lisp
(defvar my/helm-go-source
  '((name . "Helm Go")
    (candidates . (lambda ()
                    (cons "builtin" (go-packages))))
    (action . (("Show document" . godoc)
               ("Import package" . my/helm-go-import-add)))))

(defun my/helm-go-import-add (candidate)
  (dolist (package (helm-marked-candidates))
    (go-import-add current-prefix-arg package)))

(defun my/helm-go ()
  (interactive)
  (helm :sources '(my/helm-go-source) :buffer "*helm go*"))
```

### Screenshot

<hr />

![go-helm-util1](/images/programming/golang/go-helm-util1.png)

<hr />

![go-helm-util2](/images/programming/golang/go-helm-util2.png)


## 設定例

```common-lisp
(eval-after-load "go-mode"
  '(progn
     (require 'go-autocomplete)
     (add-hook 'go-mode-hook 'go-eldoc-setup)

     ;; key bindings
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     (define-key go-mode-map (kbd "M-,") 'pop-tag-mark)))
```


## 参考
* [EmacsでのGo言語編集環境 by typesterさん](http://unknownplace.org/archives/golang-editing-with-emacs.html)
* [Emacsを使ったGo言語開発手法(2013.07版) by ymotongpooさん](http://ymotongpoo.hatenablog.com/entry/2013/07/06/154448)
* [Writing Go in Emacs by Dominik Honnefさん](http://www.honnef.co/posts/2013/08/writing_go_in_emacs__cont__/)

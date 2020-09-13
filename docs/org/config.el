;;; config.el --- emacs-jp blog config  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Keywords: convenience

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; emacs-jp blog config.

;;   1. load this file.
;;   2. M-x org-publish emacs-jp

;;; Code:

;; require conao3 forked ox-gfm
(setf (alist-get "celpa" package-archives nil nil #'string=)
      "https://celpa.conao3.com/packages/")

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(leaf ox-gfm
  :ensure t
  :require org ox-publish t
  :defun my/f-parent org-publish
  :defvar org-publish-project-alist
  :preface
  (defun my/f-parent (path)
    "Return the parent directory to PATH."
    (let ((parent (file-name-directory
                   (directory-file-name (expand-file-name path default-directory)))))
      (directory-file-name parent)))

  (defvar my/emacs-jp-org-dir (my/f-parent
                               (or load-file-name
                                   (bound-and-true-p byte-compile-current-file)
                                   buffer-file-name)))

  (defun my/publish-emacs-jp ()
    "Publish emacs-jp blog files."
    (interactive)
    (org-publish "emacs-jp"))

  (defun my/insert-emacs-jp-template ()
    "Insert emacs-jp template."
    (interactive)
    (insert (format "\
#+title: {{ todo: title }}
#+author: {{ todo: author }}
#+date: %s
#+last_modified: %s
#+options: ^:{} toc:nil

#+link: images file+sys:../images/
#+link: files file+sys:../files/

#+gfm_layout: page
#+gfm_tags: {{ todo: tags }}
#+gfm_headline_offset: 1
#+gfm_preamble: {%% include JB/setup %%}
#+gfm_custom_front_matter: :org t

* 概要
* 使用方法
* まとめ"
                    (format-time-string "<%Y-%m-%d %a>")
                    (format-time-string "<%Y-%m-%d %a>"))))

  :config
  (setf (alist-get "emacs-jp" org-publish-project-alist nil nil #'string=)
        (list
         :base-directory (expand-file-name "" my/emacs-jp-org-dir)
         :base-extension "org"
         :publishing-directory (expand-file-name "../" my/emacs-jp-org-dir)
         :recursive t
         :publishing-function 'org-gfm-publish-to-gfm)))


;; (provide 'config)
;;; config.el ends here

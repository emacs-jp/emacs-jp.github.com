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

(leaf ox-gfm
  :ensure t
  :require org ox-publish t
  :custom ((org-gfm-layout . "page")
           (org-gfm-preamble . "{% include JB/setup %}\n"))
  :defun my/f-parent
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

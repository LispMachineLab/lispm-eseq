;;; lispm-eseq-lisp.el --- Editing ESeq's Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-eseq
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ESeq's files of Lisp, its eseqlisp modules and the DGenLisp of its
;; instruments and effects, open in `lispm-eseq-lisp-mode': Lisp's
;; editing, ESeq's forms shown as definitions, and its keys:
;;
;;   C-c C-c  check what the file belongs to with ESeq's own check: an
;;            instrument or effect compiled, played and its panel drawn,
;;            a sequencer loaded and its panel drawn
;;   C-c C-v  view the panel the last check drew
;;   C-c C-o  open ESeq
;;   C-c C-d  open ESeq's guide to making instruments, effects and
;;            sequencers
;;
;; ESeq loads a Local module again once its file is saved, and an
;; instrument's ui.lisp; a dsp.lisp already on a track is loaded again
;; from ESeq's Edit menu, Reload Instrument From Disk.

;;; Code:

(require 'lisp-mode)
(require 'lispm-eseq)

(defconst lispm-eseq-lisp-font-lock-keywords
  `((,(concat "(\\(" (regexp-opt '("def" "defmacro" "defwidget" "def-kind" "defsynth-ui" "defeffect-ui"
                                   "defparam" "definstrument" "defeffect" "defscene" "define-agent-tool")
                                 t)
              "\\)\\_>[ \t\n]*\\([^ \t\n()]+\\)?")
     (1 font-lock-keyword-face)
     (3 font-lock-function-name-face nil t))
    (,(concat "(" (regexp-opt '("module" "import" "export" "load" "let" "if" "when" "unless" "cond"
                                "lambda" "each" "do" "set!" "and" "or" "not")
                              t)
              "\\_>")
     (1 font-lock-keyword-face))
    ("\\_<self\\.[^ \t\n()]+" . font-lock-variable-name-face)
    ("\\_<:[^ \t\n()]+" . font-lock-builtin-face)
    ("\\_<\\(true\\|false\\|nil\\)\\_>" . font-lock-constant-face))
  "What `lispm-eseq-lisp-mode' shows: ESeq's definitions, its forms,
fields of self, keywords.")

(defun lispm-eseq-lisp--target ()
  "Return what ESeq's check checks for this buffer's file, or say why not."
  (or (and buffer-file-name (lispm-eseq-file-target buffer-file-name))
      (user-error "This file is not one of the instruments, effects or Local modules made for ESeq")))

(defun lispm-eseq-lisp-check (&optional no-render)
  "Save this buffer and check what it belongs to with ESeq's own check;
with a prefix argument, NO-RENDER, without drawing its panel.  What it
says shows in a buffer; the panel it drew is a \\[lispm-eseq-lisp-view-panel] away."
  (interactive "P")
  (pcase-let ((`(,kind . ,target) (lispm-eseq-lisp--target)))
    (save-buffer)
    (display-buffer (get-buffer-create lispm-eseq-check-buffer-name))
    (lispm-eseq-check kind target
                      (lambda (result)
                        (message "%s %s" (if (plist-get result :passed) "Passed:" "Failed:")
                                 (or (plist-get result :failure) target)))
                      no-render)
    (message "Checking %s…" (if (eq kind 'sequencer) target (file-name-nondirectory target)))))

(defun lispm-eseq-lisp-view-panel ()
  "View the panel the last check of this file's instrument, effect or
sequencer drew."
  (interactive)
  (pcase-let* ((`(,kind . ,target) (lispm-eseq-lisp--target))
               (png (plist-get (lispm-eseq-last-check kind target) :png)))
    (if (and png (file-readable-p png))
        (view-file-other-window png)
      (user-error "No panel drawn yet: C-c C-c checks it"))))

(defun lispm-eseq-lisp-guide ()
  "Open ESeq's guide to making instruments, effects and sequencers."
  (interactive)
  (if-let* ((guide (lispm-eseq-guide)))
      (view-file-other-window guide)
    (user-error "ESeq has written no guide: it writes one once installed")))

(defvar lispm-eseq-lisp-mode-syntax-table
  (let ((table (make-syntax-table lisp-data-mode-syntax-table)))
    ;; |i|, as in (each LIST |i| ...), names a variable, not a symbol quoted.
    (modify-syntax-entry ?| "_" table)
    table)
  "Syntax table of `lispm-eseq-lisp-mode'.")

(defvar-keymap lispm-eseq-lisp-mode-map
  :doc "Keys of `lispm-eseq-lisp-mode'."
  :parent lisp-mode-shared-map
  "C-c C-c" #'lispm-eseq-lisp-check
  "C-c C-v" #'lispm-eseq-lisp-view-panel
  "C-c C-o" #'lispm-eseq-start
  "C-c C-d" #'lispm-eseq-lisp-guide)

(easy-menu-define lispm-eseq-lisp-menu lispm-eseq-lisp-mode-map
  "Menu of ESeq's Lisp."
  '("ESeq"
    ["Check" lispm-eseq-lisp-check :active (lispm-eseq-installed-p)]
    ["Check Without Panel" (lispm-eseq-lisp-check t) :active (lispm-eseq-installed-p)]
    ["View Panel" lispm-eseq-lisp-view-panel]
    "---"
    ["Open ESeq" lispm-eseq-start :active (lispm-eseq-installed-p)]
    ["Guide to Making Things" lispm-eseq-lisp-guide]))

;;;###autoload
(define-derived-mode lispm-eseq-lisp-mode lisp-data-mode "ESeq Lisp"
  "Mode of ESeq's Lisp: its eseqlisp modules, and the DGenLisp of its
instruments and effects.  ESeq loads a file again once it is saved.
\\{lispm-eseq-lisp-mode-map}"
  :syntax-table lispm-eseq-lisp-mode-syntax-table
  (font-lock-add-keywords nil lispm-eseq-lisp-font-lock-keywords)
  (setq-local comment-start ";; "))

;;;###autoload
(add-to-list 'auto-mode-alist '("/\\.eseq\\.d/.*\\.lisp\\'" . lispm-eseq-lisp-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/com\\.universalsequences\\.eseq/.*\\.lisp\\'" . lispm-eseq-lisp-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("/ESeq\\.app/Contents/Resources/.*\\.lisp\\'" . lispm-eseq-lisp-mode))

(provide 'lispm-eseq-lisp)
;;; lispm-eseq-lisp.el ends here

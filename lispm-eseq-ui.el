;;; lispm-eseq-ui.el --- ESeq's window  -*- lexical-binding: t; -*-

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

;; M-x lispm-eseq opens ESeq's window: ESeq, running or not, and what was
;; made for it, instruments, effects, sequencers and packages, each with
;; how its last check went.  Its keys:
;;
;;   RET  on ESeq, open it; on a heading, make one; on a line, edit it
;;   o    open ESeq
;;   e    edit the line's instrument, effect or sequencer
;;   u    edit the panel, ui.lisp, of the line's instrument or effect
;;   c    check the line's instrument, effect or sequencer with ESeq's
;;        own check; C, without drawing its panel
;;   v    view the panel the last check drew
;;   l    show what the checks said
;;   +    make an instrument or effect from one of ESeq's own, or a
;;        sequencer, a pair of Local modules that passes the check
;;   i    install a package from its git repository
;;   I    import a package from a folder or an archive
;;   d    show the line's folder
;;   w    copy the line's folder or file
;;   G    open ESeq's guide to making instruments, effects and sequencers
;;   g    read it all again
;;   n p  the next and the previous line

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'lispm-eseq)
(require 'lispm-eseq-lisp)

(defface lispm-eseq-heading '((t :inherit bold))
  "Face of the headings of ESeq's window."
  :group 'lispm-eseq)

(defface lispm-eseq-good '((t :inherit success))
  "Face of what is well: ESeq running, a check passed."
  :group 'lispm-eseq)

(defface lispm-eseq-bad '((t :inherit error))
  "Face of what is wrong: a check failed."
  :group 'lispm-eseq)

(defface lispm-eseq-dim '((t :inherit shadow))
  "Face of what is not there: ESeq not running, a thing not checked."
  :group 'lispm-eseq)

(defconst lispm-eseq-buffer-name "*ESeq*"
  "Name of the buffer of ESeq's window.")

(defconst lispm-eseq-column 30
  "Column the second column of ESeq's window starts at.")

(defconst lispm-eseq-third-column 48
  "Column the third column of ESeq's window starts at.")

(defconst lispm-eseq-headings
  '((instrument . "Instruments") (effect . "Effects") (sequencer . "Sequencers") (package . "Packages"))
  "The headings of ESeq's window, by kind.")

(defvar lispm-eseq-message nil
  "What went wrong last, said at the top of ESeq's window, or nil.")

;;;; What the line is about

(defun lispm-eseq-line ()
  "Return what the line at point is about: an item as `lispm-eseq-items'
gives it, (heading . KIND), (eseq), or nil."
  (get-text-property (line-beginning-position) 'lispm-eseq-line))

(defun lispm-eseq--item ()
  "Return the item of the line at point, or nil."
  (let ((line (lispm-eseq-line)))
    (and (plist-get line :kind) line)))

(defun lispm-eseq--kind-here ()
  "Return the kind of the line at point, or of its heading, or nil."
  (let ((line (lispm-eseq-line)))
    (cond ((plist-get line :kind))
          ((eq (car-safe line) 'heading) (cdr line)))))

(defun lispm-eseq-next-line (&optional n)
  "Go to the Nth next line about something."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (let ((start (point)))
        (forward-line (if (< n 0) -1 1))
        (while (and (not (lispm-eseq-line)) (not (if (< n 0) (bobp) (eobp))))
          (forward-line (if (< n 0) -1 1)))
        (unless (lispm-eseq-line)
          (goto-char start))))))

(defun lispm-eseq-previous-line (&optional n)
  "Go to the Nth previous line about something."
  (interactive "p")
  (lispm-eseq-next-line (- (or n 1))))

;;;; Drawing

(defun lispm-eseq--column (column)
  "Return a space reaching COLUMN."
  (propertize " " 'display `(space :align-to ,column)))

(defun lispm-eseq--line (about first &rest more)
  "Insert a line ABOUT something: FIRST, then MORE, each in its column."
  (let ((start (point)))
    (insert "  " first)
    (when (car more)
      (insert (lispm-eseq--column lispm-eseq-column) (car more)))
    (when (cadr more)
      (insert (lispm-eseq--column lispm-eseq-third-column) (cadr more)))
    (insert "\n")
    (when about
      (put-text-property start (point) 'lispm-eseq-line about))))

(defun lispm-eseq--mark (good)
  "Return the mark of a line: filled when GOOD, else hollow."
  (if good (propertize "●" 'face 'lispm-eseq-good) (propertize "○" 'face 'lispm-eseq-dim)))

(defun lispm-eseq--check-text (item)
  "Return how ITEM's last check went, as the window says it."
  (let* ((key (lispm-eseq-item-key item))
         (result (and key (lispm-eseq-last-check (car key) (cdr key)))))
    (pcase (lispm-eseq-check-state item)
      ('checking (propertize "Checking…" 'face 'lispm-eseq-good))
      ('passed (concat (propertize "Passed" 'face 'lispm-eseq-good)
                       (format-time-string " at %H:%M" (plist-get result :time))
                       (when (plist-get result :warnings)
                         (format ", %d warning%s" (length (plist-get result :warnings))
                                 (if (cdr (plist-get result :warnings)) "s" "")))))
      ('failed (propertize (format "Failed: %s" (or (plist-get result :stage) "see l"))
                           'face 'lispm-eseq-bad))
      (_ (if key (propertize "Not checked: c checks it" 'face 'lispm-eseq-dim) "")))))

(defun lispm-eseq--second-column (item)
  "Return what ITEM's line says in its second column."
  (pcase (plist-get item :kind)
    ('sequencer (if (plist-get item :kinds)
                    (concat "kind " (string-join (plist-get item :kinds) ", "))
                  (propertize "module" 'face 'lispm-eseq-dim)))
    ('package (string-join (delq nil (list (plist-get item :version)
                                           (when (plist-get item :kinds)
                                             (concat "kinds " (string-join (plist-get item :kinds) ", ")))))
                           "  "))
    (_ "")))

(defun lispm-eseq--insert-missing ()
  "Say that ESeq is not installed, and where it comes from."
  (insert (propertize "ESeq is not installed" 'face 'lispm-eseq-heading) "\n\n"
          "ESeq is a live sequencer and audio engine for the Mac, made after Emacs:\n"
          "its interface and sequencers are written in its own Lisp, and its\n"
          "instruments and effects in DGenLisp, compiled while it runs.\n\n"
          "ESeq.app comes from its releases, " lispm-eseq-releases-url ",\n"
          "and goes into the Applications folder.  Then g reads it again.\n"))

(defun lispm-eseq--insert-section (kind)
  "Insert the heading of KIND and what was made of it."
  (let ((items (lispm-eseq-items kind))
        (start (point)))
    (insert "\n" (propertize (alist-get kind lispm-eseq-headings) 'face 'lispm-eseq-heading))
    (insert (propertize (pcase kind
                          ('package "  i installs one, I imports one")
                          (_ "  + makes one"))
                        'face 'lispm-eseq-dim)
            "\n")
    (put-text-property (1+ start) (point) 'lispm-eseq-line (cons 'heading kind))
    (if (null items)
        (insert (propertize "  None yet\n" 'face 'lispm-eseq-dim))
      (dolist (item items)
        (lispm-eseq--line item (plist-get item :name) (lispm-eseq--second-column item)
                          (lispm-eseq--check-text item))))))

(defun lispm-eseq--print ()
  "Show ESeq and what was made for it in the current buffer, point kept
on its line."
  (let ((line (lispm-eseq-line))
        (column (current-column))
        (inhibit-read-only t))
    (erase-buffer)
    (if (not (lispm-eseq-installed-p))
        (lispm-eseq--insert-missing)
      (insert (propertize (concat "ESeq " (or (lispm-eseq-version) "")) 'face 'lispm-eseq-heading) "\n")
      (when lispm-eseq-message
        (insert (propertize lispm-eseq-message 'face 'lispm-eseq-bad) "\n"))
      (insert "\n")
      (let ((running (lispm-eseq-running-p)))
        (lispm-eseq--line '(eseq)
                          (concat (lispm-eseq--mark running) (if running " Running" " Not running"))
                          (propertize (if running "o brings it to the front" "o opens it") 'face 'lispm-eseq-dim)))
      (mapc #'lispm-eseq--insert-section lispm-eseq-kinds))
    (goto-char (point-min))
    (if-let* ((match (and line (text-property-search-forward 'lispm-eseq-line line #'equal))))
        (progn (goto-char (prop-match-beginning match))
               (move-to-column column))
      (goto-char (point-min))
      (lispm-eseq-next-line))))

(defun lispm-eseq--refresh ()
  "Show what is known of ESeq now in its window.
This is one of `lispm-eseq-changed-hook'."
  (when-let* ((buffer (get-buffer lispm-eseq-buffer-name)))
    (with-current-buffer buffer
      (let ((windows (get-buffer-window-list buffer nil t)))
        (lispm-eseq--print)
        (dolist (window windows)
          (set-window-point window (point)))))))

(add-hook 'lispm-eseq-changed-hook #'lispm-eseq--refresh)

;;;; Commands

(defun lispm-eseq-reload (&rest _)
  "Read ESeq and what was made for it again."
  (interactive)
  (lispm-eseq-forget)
  (setq lispm-eseq-message nil)
  (lispm-eseq--refresh))

(defun lispm-eseq--item-or-error ()
  "Return the item of the line at point, or say there is none."
  (or (lispm-eseq--item) (user-error "Not on an instrument, effect, sequencer or package")))

(defun lispm-eseq-edit (&optional panel)
  "Edit the line's instrument or effect, its dsp.lisp, or with PANEL its
ui.lisp; its sequencer's file; or show its package's folder."
  (interactive)
  (let ((item (lispm-eseq--item-or-error)))
    (pcase (plist-get item :kind)
      ((or 'instrument 'effect)
       (find-file-other-window (expand-file-name (if panel "ui.lisp" "dsp.lisp") (plist-get item :folder))))
      ('sequencer (find-file-other-window (plist-get item :file)))
      ('package (dired-other-window (plist-get item :folder))))))

(defun lispm-eseq-edit-panel ()
  "Edit the panel, ui.lisp, of the line's instrument or effect."
  (interactive)
  (lispm-eseq-edit t))

(defun lispm-eseq-check-here (&optional no-render)
  "Check the line's instrument, effect or sequencer with ESeq's own check;
with NO-RENDER, without drawing its panel."
  (interactive)
  (let* ((item (lispm-eseq--item-or-error))
         (key (or (lispm-eseq-item-key item) (user-error "A package is not checked as a whole"))))
    (lispm-eseq-check (car key) (cdr key)
                      (lambda (result)
                        (message "%s %s" (if (plist-get result :passed) "Passed:" "Failed:")
                                 (or (plist-get result :failure) (plist-get item :name))))
                      no-render)))

(defun lispm-eseq-check-without-panel ()
  "Check the line's instrument, effect or sequencer without drawing its
panel."
  (interactive)
  (lispm-eseq-check-here t))

(defun lispm-eseq-view-panel ()
  "View the panel the last check of the line's item drew."
  (interactive)
  (let* ((key (lispm-eseq-item-key (lispm-eseq--item-or-error)))
         (png (and key (plist-get (lispm-eseq-last-check (car key) (cdr key)) :png))))
    (if (and png (file-readable-p png))
        (view-file-other-window png)
      (user-error "No panel drawn yet: c checks it"))))

(defun lispm-eseq-show-checks ()
  "Show what ESeq's checks said."
  (interactive)
  (if-let* ((buffer (get-buffer lispm-eseq-check-buffer-name)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (display-buffer buffer))
    (user-error "Nothing was checked yet")))

(defun lispm-eseq-new (kind)
  "Make an instrument, an effect or a sequencer, as KIND is: an instrument
or effect from one of ESeq's own, or a sequencer that passes the check;
then edit it."
  (interactive
   (list (let ((here (lispm-eseq--kind-here)))
           (if (memq here '(instrument effect sequencer))
               here
             (intern (completing-read "Make: " '("instrument" "effect" "sequencer") nil t))))))
  (unless (lispm-eseq-installed-p)
    (user-error "ESeq is not installed: ESeq.app comes from %s" lispm-eseq-releases-url))
  (if (eq kind 'sequencer)
      (find-file-other-window (lispm-eseq-new-sequencer (read-string "Sequencer's name (lower-case): ")))
    (let* ((examples (or (lispm-eseq-examples kind) (user-error "ESeq has no %ss of its own" kind)))
           (example (completing-read (format "Start from ESeq's %s: " kind) examples nil t))
           (name (read-string (format "Name of the new %s: " kind) (file-name-nondirectory example))))
      (find-file-other-window (expand-file-name "dsp.lisp" (lispm-eseq-copy-example kind example name))))))

(defun lispm-eseq-install (identity url)
  "Install the package IDENTITY, AUTHOR/NAME, from the git repository URL."
  (interactive (list (read-string "Package (author/name): ")
                     (read-string "From the git repository: ")))
  (message "Installing %s…" identity)
  (lispm-eseq-install-package identity url
                              (lambda (error)
                                (setq lispm-eseq-message (and error (format "%s was not installed: %s" identity error)))
                                (lispm-eseq--refresh)
                                (message (if error "%s was not installed" "Installed %s") identity))))

(defun lispm-eseq-import (file)
  "Import the package FILE, a folder or an archive."
  (interactive (list (read-file-name "Import the package: ")))
  (lispm-eseq-import-package file
                             (lambda (error)
                               (setq lispm-eseq-message (and error (format "Not imported: %s" error)))
                               (lispm-eseq--refresh)
                               (message (if error "%s was not imported" "Imported %s")
                                        (file-name-nondirectory (directory-file-name file))))))

(defun lispm-eseq--file-here ()
  "Return the folder or file of the line's item."
  (let ((item (lispm-eseq--item-or-error)))
    (or (plist-get item :folder) (plist-get item :file))))

(defun lispm-eseq-show-folder ()
  "Show the folder of the line's item."
  (interactive)
  (let ((file (lispm-eseq--file-here)))
    (dired-other-window (if (file-directory-p file) file (file-name-directory file)))))

(defun lispm-eseq-copy ()
  "Copy the folder or file of the line's item."
  (interactive)
  (let ((file (lispm-eseq--file-here)))
    (kill-new file)
    (message "Copied %s" (abbreviate-file-name file))))

(defun lispm-eseq-act ()
  "Do what the line at point is for: open ESeq, make one, edit it."
  (interactive)
  (pcase (lispm-eseq-line)
    ('(eseq) (lispm-eseq-start))
    ('(heading . package) (call-interactively #'lispm-eseq-install))
    (`(heading . ,kind) (lispm-eseq-new kind))
    ((guard (lispm-eseq--item)) (lispm-eseq-edit))
    (_ (user-error "Nothing to do on this line"))))

;;;; The window

(defvar-keymap lispm-eseq-mode-map
  :doc "Keys of ESeq's window."
  "RET" #'lispm-eseq-act
  "o" #'lispm-eseq-start
  "e" #'lispm-eseq-edit
  "u" #'lispm-eseq-edit-panel
  "c" #'lispm-eseq-check-here
  "C" #'lispm-eseq-check-without-panel
  "v" #'lispm-eseq-view-panel
  "l" #'lispm-eseq-show-checks
  "+" #'lispm-eseq-new
  "i" #'lispm-eseq-install
  "I" #'lispm-eseq-import
  "d" #'lispm-eseq-show-folder
  "w" #'lispm-eseq-copy
  "G" #'lispm-eseq-lisp-guide
  "g" #'lispm-eseq-reload
  "n" #'lispm-eseq-next-line
  "p" #'lispm-eseq-previous-line)

(defun lispm-eseq--checkable-p ()
  "Return non-nil when the line's item is checked."
  (and (lispm-eseq-installed-p) (lispm-eseq-item-key (lispm-eseq--item))))

(easy-menu-define lispm-eseq-menu lispm-eseq-mode-map
  "ESeq's menu."
  '("ESeq"
    ["Open ESeq" lispm-eseq-start :active (lispm-eseq-installed-p)]
    "---"
    ["New Instrument…" (lispm-eseq-new 'instrument) :active (lispm-eseq-installed-p)]
    ["New Effect…" (lispm-eseq-new 'effect) :active (lispm-eseq-installed-p)]
    ["New Sequencer…" (lispm-eseq-new 'sequencer) :active (lispm-eseq-installed-p)]
    "---"
    ["Edit" lispm-eseq-edit :active (lispm-eseq--item)]
    ["Edit Panel" lispm-eseq-edit-panel :active (memq (plist-get (lispm-eseq--item) :kind) '(instrument effect))]
    ["Check" lispm-eseq-check-here :active (lispm-eseq--checkable-p)]
    ["Check Without Panel" lispm-eseq-check-without-panel :active (lispm-eseq--checkable-p)]
    ["View Panel" lispm-eseq-view-panel :active (lispm-eseq--item)]
    ["Show Checks" lispm-eseq-show-checks :active (get-buffer lispm-eseq-check-buffer-name)]
    "---"
    ["Install Package…" lispm-eseq-install :active (lispm-eseq-installed-p)]
    ["Import Package…" lispm-eseq-import :active (lispm-eseq-installed-p)]
    "---"
    ["Show Folder" lispm-eseq-show-folder :active (lispm-eseq--item)]
    ["Copy Path" lispm-eseq-copy :active (lispm-eseq--item)]
    ["Guide to Making Things" lispm-eseq-lisp-guide :active (lispm-eseq-installed-p)]
    ["Read Again" lispm-eseq-reload]))

(define-derived-mode lispm-eseq-mode special-mode "ESeq"
  "ESeq's window: ESeq, running or not, and the instruments, effects,
sequencers and packages made for it.
\\{lispm-eseq-mode-map}"
  (setq truncate-lines t
        buffer-read-only t)
  ;; A line cut at the window's edge ends in an ellipsis, not a dollar.
  (setq buffer-display-table (or (copy-sequence buffer-display-table) (make-display-table)))
  (set-display-table-slot buffer-display-table 'truncation ?…)
  (setq-local revert-buffer-function #'lispm-eseq-reload))

;;;###autoload
(defun lispm-eseq ()
  "Open ESeq's window: ESeq, and what was made for it."
  (interactive)
  (let ((buffer (get-buffer-create lispm-eseq-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lispm-eseq-mode)
        (lispm-eseq-mode))
      (lispm-eseq-forget)
      (lispm-eseq--print))
    (pop-to-buffer-same-window buffer)))

(provide 'lispm-eseq-ui)
;;; lispm-eseq-ui.el ends here

;;; lispm-eseq.el --- ESeq, a live sequencer extended in Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-eseq
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: multimedia, lisp, tools
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

;; ESeq (https://github.com/universalsequences/eseq) is a live sequencer
;; and audio engine for macOS, an application of its own, ESeq.app, made
;; after Emacs: its interface and its sequencers are written in a Lisp of
;; its own, eseqlisp, which it loads again as their files change, and its
;; instruments and effects in another, DGenLisp, which it compiles to
;; native code while it runs.  What is made for it lives in two places:
;;
;;   ~/Library/Application Support/com.universalsequences.eseq/
;;       instruments/NAME/   an instrument: dsp.lisp, ui.lisp
;;       effects/NAME/       an effect, the same
;;   ~/.eseq.d/packages/
;;       local/FOLDER/*.lisp sequencers: modules of eseqlisp, FOLDER.core
;;       AUTHOR.NAME/        packages installed, each with a manifest.json
;;
;; This package is ESeq's side in Emacs.  M-x lispm-eseq opens its window:
;; ESeq, running or not, and what was made for it.  From there ESeq is
;; opened; instruments, effects and sequencers are made from examples,
;; edited in Emacs, where ESeq reloads what is saved, and checked with
;; ESeq's own check, which compiles them, plays a test note and draws
;; their panel; packages are installed from git.
;;
;; This file finds ESeq.app and runs its command-line program, eseq, at
;; ESeq.app/Contents/MacOS/eseq; lispm-eseq-lisp.el is the mode of ESeq's
;; Lisp files, lispm-eseq-ui.el the window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup lispm-eseq nil
  "ESeq, a live sequencer extended in Lisp."
  :group 'applications
  :prefix "lispm-eseq-")

(defcustom lispm-eseq-application nil
  "ESeq's application, ESeq.app: nil to look for it in the Applications
folders, then with Spotlight."
  :type '(choice (const :tag "Look for it" nil) directory))

(defconst lispm-eseq-bundle-identifier "com.universalsequences.eseq"
  "ESeq's bundle identifier.")

(defconst lispm-eseq-releases-url "https://github.com/universalsequences/eseq/releases"
  "Where ESeq's releases are, which ESeq.app comes from.")

(defconst lispm-eseq-kinds '(instrument effect sequencer package)
  "The kinds of what is made for ESeq, as this package lists them.")

(defvar lispm-eseq-changed-hook nil
  "Hook run once what is known of ESeq changed: a check ended, something
was made or installed.")

;;;; ESeq.app

(defvar lispm-eseq--found nil
  "ESeq.app as last looked for: (TIME . DIRECTORY-OR-NIL).")

(defun lispm-eseq--spotlight ()
  "Return ESeq.app as Spotlight finds it by its bundle identifier, or nil."
  (when (executable-find "mdfind")
    (seq-find (lambda (file) (string-suffix-p ".app" file))
              (process-lines-ignore-status
               "mdfind" (format "kMDItemCFBundleIdentifier == '%s'" lispm-eseq-bundle-identifier)))))

(defun lispm-eseq-application ()
  "Return ESeq.app, a directory, or nil when it is not installed.  Looks
are kept a few seconds, as the window asks often."
  (if lispm-eseq-application
      (and (file-directory-p lispm-eseq-application)
           (directory-file-name (expand-file-name lispm-eseq-application)))
    (if (and lispm-eseq--found (< (- (float-time) (car lispm-eseq--found)) 5))
        (cdr lispm-eseq--found)
      (cdr (setq lispm-eseq--found
                 (cons (float-time)
                       (or (seq-find #'file-directory-p
                                     (mapcar (lambda (directory) (expand-file-name "ESeq.app" directory))
                                             '("/Applications" "~/Applications")))
                           (lispm-eseq--spotlight))))))))

(defun lispm-eseq-forget ()
  "Look for ESeq.app again next time, as once it was installed."
  (setq lispm-eseq--found nil))

(defun lispm-eseq-program ()
  "Return eseq, ESeq's command-line program, or nil."
  (when-let* ((application (lispm-eseq-application))
              (program (expand-file-name "Contents/MacOS/eseq" application))
              ((file-executable-p program)))
    program))

(defun lispm-eseq-installed-p ()
  "Return non-nil when ESeq is installed, its program with it."
  (and (lispm-eseq-program) t))

(defun lispm-eseq-version ()
  "Return ESeq's version as its Info.plist says it, \"0.1.0 (31e088f8)\",
or nil."
  (when-let* ((application (lispm-eseq-application))
              (file (expand-file-name "Contents/Info.plist" application))
              ((file-readable-p file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((value (lambda (key)
                     (goto-char (point-min))
                     (and (re-search-forward
                           (format "<key>%s</key>[ \t\n]*<string>\\([^<]*\\)</string>" (regexp-quote key))
                           nil t)
                          (match-string 1)))))
        (when-let* ((version (funcall value "CFBundleShortVersionString")))
          (if-let* ((build (funcall value "CFBundleVersion")))
              (format "%s (%s)" version build)
            version))))))

(defun lispm-eseq--extended-regexp (text)
  "Return a POSIX extended regular expression matching TEXT, as pgrep reads
one."
  (replace-regexp-in-string "[][.()*+?{}|^$\\\\]" "\\\\\\&" text))

(defun lispm-eseq-running-p ()
  "Return non-nil while ESeq runs: its window, not a check drawing a panel
out of sight, which runs the same program with arguments."
  (when-let* ((application (lispm-eseq-application)))
    (eq 0 (ignore-errors
            (call-process "pgrep" nil nil nil "-fx"
                          (lispm-eseq--extended-regexp
                           (expand-file-name "Contents/MacOS/metal_seq" application)))))))

;;;###autoload
(defun lispm-eseq-start ()
  "Open ESeq, or bring it to the front."
  (interactive)
  (let ((application (lispm-eseq-application)))
    (unless application
      (user-error "ESeq is not installed: ESeq.app comes from %s" lispm-eseq-releases-url))
    (unless (eq 0 (call-process "open" nil nil nil "-a" application))
      (error "ESeq did not open"))
    (run-at-time 2 nil #'run-hooks 'lispm-eseq-changed-hook)))

;;;; Where things are

(defvar lispm-eseq--paths nil
  "What eseq paths said, (PROGRAM . ALIST), ALIST of (KEY . DIRECTORY).")

(defun lispm-eseq--default-paths ()
  "Return where ESeq keeps things when it says nothing, as ESeq.app does."
  (let ((root (expand-file-name "~/Library/Application Support/com.universalsequences.eseq"))
        (lisp (expand-file-name "~/.eseq.d")))
    `(("user-root" . ,root)
      ("user-instruments" . ,(expand-file-name "instruments" root))
      ("user-effects" . ,(expand-file-name "effects" root))
      ("packages" . ,(expand-file-name "packages" lisp))
      ("local-packages" . ,(expand-file-name "packages/local" lisp)))))

(defun lispm-eseq-parse-paths (text)
  "Return the paths TEXT, what eseq paths printed, says: (KEY . DIRECTORY)."
  (delq nil (mapcar (lambda (line)
                      (when (string-match "\\`\\([a-z][a-z-]*\\)[ \t]+\\(/.*?\\)[ \t]*\\'" line)
                        (cons (match-string 1 line) (match-string 2 line))))
                    (split-string text "\n" t))))

(defun lispm-eseq-paths ()
  "Return where ESeq keeps things, as (KEY . DIRECTORY): user-root,
user-instruments, user-effects, factory-instruments, factory-effects,
packages, local-packages, authoring.  ESeq says it once per program;
while it is not installed, these are where it would."
  (let ((program (lispm-eseq-program)))
    (cond
     ((null program) (lispm-eseq--default-paths))
     ((equal (car lispm-eseq--paths) program) (cdr lispm-eseq--paths))
     (t (let ((paths (with-temp-buffer
                       (and (eq 0 (ignore-errors (call-process program nil '(t nil) nil "paths")))
                            (lispm-eseq-parse-paths (buffer-string))))))
          (if paths
              (cdr (setq lispm-eseq--paths (cons program paths)))
            (lispm-eseq--default-paths)))))))

(defun lispm-eseq-path (key)
  "Return the directory of KEY, a string as `lispm-eseq-paths' gives, or nil."
  (cdr (assoc key (lispm-eseq-paths))))

(defun lispm-eseq-guide ()
  "Return ESeq's guide to making instruments, effects and sequencers,
AGENTS.md in its folder of what users make, written first if need be,
or nil."
  (when-let* ((root (lispm-eseq-path "user-root")))
    (let ((file (expand-file-name "AGENTS.md" root)))
      (unless (file-exists-p file)
        (when-let* ((program (lispm-eseq-program)))
          (ignore-errors (call-process program nil nil nil "authoring" "seed"))))
      (and (file-readable-p file) file))))

;;;; What was made for it

(defun lispm-eseq--sources (directory &optional depth)
  "Return the folders under DIRECTORY holding a dsp.lisp, DEPTH folders
deep at most, as (NAME . FOLDER), NAME its path under DIRECTORY."
  (let ((depth (or depth 3))
        (found nil))
    (when (and (> depth 0) (file-directory-p directory))
      (dolist (folder (directory-files directory t "\\`[^.]"))
        (when (and (file-directory-p folder) (not (string-suffix-p ".presets" folder)))
          (if (file-exists-p (expand-file-name "dsp.lisp" folder))
              (push (cons (file-name-nondirectory folder) folder) found)
            (dolist (inner (lispm-eseq--sources folder (1- depth)))
              (push (cons (concat (file-name-nondirectory folder) "/" (car inner)) (cdr inner)) found))))))
    (sort found (lambda (a b) (string-lessp (downcase (car a)) (downcase (car b)))))))

(defun lispm-eseq-module-name (file)
  "Return the name of the module of FILE, a Local file of eseqlisp, as ESeq
names it, \"pulse.core\" for local/pulse/core.lisp; nil for another file."
  (when-let* ((local (lispm-eseq-path "local-packages"))
              (local (file-name-as-directory (expand-file-name local)))
              (file (expand-file-name file))
              ((string-prefix-p local file))
              ((string-suffix-p ".lisp" file)))
    (string-replace "/" "." (file-name-sans-extension (substring file (length local))))))

(defun lispm-eseq--kinds-in (file)
  "Return the names of the kinds FILE declares with def-kind."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((kinds nil))
      (while (re-search-forward "^[ \t]*(def-kind[ \t\n]+\\([^ \t\n()]+\\)" nil t)
        (push (match-string 1) kinds))
      (nreverse kinds))))

(defun lispm-eseq--manifest (folder)
  "Return what FOLDER's manifest.json says, an alist, or nil."
  (let ((file (expand-file-name "manifest.json" folder)))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer :object-type 'alist :array-type 'list))))))

(defun lispm-eseq-items (kind)
  "Return what was made for ESeq of KIND, one of `lispm-eseq-kinds', as
property lists: :kind, :name, and :folder for instruments and effects;
:file, :module and :kinds, those its def-kind declares, for sequencers;
:folder, :version and :kinds for packages."
  (pcase kind
    ((or 'instrument 'effect)
     (mapcar (pcase-lambda (`(,name . ,folder)) (list :kind kind :name name :folder folder))
             (lispm-eseq--sources (lispm-eseq-path (if (eq kind 'instrument) "user-instruments" "user-effects")))))
    ('sequencer
     (when-let* ((local (lispm-eseq-path "local-packages"))
                 ((file-directory-p local)))
       (mapcar (lambda (file)
                 (list :kind 'sequencer :name (lispm-eseq-module-name file) :module (lispm-eseq-module-name file)
                       :file file :kinds (lispm-eseq--kinds-in file)))
               (sort (directory-files-recursively local "\\`[^.].*\\.lisp\\'") #'string-lessp))))
    ('package
     (when-let* ((packages (lispm-eseq-path "packages"))
                 ((file-directory-p packages)))
       (delq nil (mapcar (lambda (folder)
                           (when-let* (((file-directory-p folder))
                                       (manifest (lispm-eseq--manifest folder)))
                             (list :kind 'package :folder folder
                                   :name (or (alist-get 'name manifest) (file-name-nondirectory folder))
                                   :version (alist-get 'version manifest)
                                   :kinds (mapcar (lambda (kind) (alist-get 'name kind))
                                                  (alist-get 'kinds manifest)))))
                         (directory-files packages t "\\`[^.]")))))))

(defun lispm-eseq-examples (kind)
  "Return ESeq's own instruments or effects, as KIND is, to start from:
\(NAME . FOLDER), NAME as \"Synths/Revsynt\"."
  (lispm-eseq--sources (lispm-eseq-path (if (eq kind 'instrument) "factory-instruments" "factory-effects"))))

(defun lispm-eseq-file-target (file)
  "Return what ESeq's check checks for FILE, as (KIND . TARGET): an
instrument's or effect's folder, or a sequencer's module, the one in the
same folder declaring its kind when FILE does not; nil when FILE is none
of them."
  (let ((file (expand-file-name file)))
    (cl-flet ((under (key)
                (when-let* ((directory (lispm-eseq-path key)))
                  (string-prefix-p (file-name-as-directory (expand-file-name directory)) file))))
      (cond
       ((or (under "user-instruments") (under "user-effects"))
        (let ((folder (locate-dominating-file file "dsp.lisp")))
          (when folder
            (cons (if (under "user-instruments") 'instrument 'effect) (directory-file-name folder)))))
       ((lispm-eseq-module-name file)
        (let ((viewer (if (and (file-exists-p file) (lispm-eseq--kinds-in file))
                          file
                        (seq-find #'lispm-eseq--kinds-in
                                  (directory-files (file-name-directory file) t "\\`[^.].*\\.lisp\\'")))))
          (cons 'sequencer (lispm-eseq-module-name (or viewer file)))))))))

;;;; Making

(defun lispm-eseq--name-p (name)
  "Return non-nil when NAME can name a folder of what is made for ESeq, in
a folder or not, as \"Bass/Acid Mono\"."
  (and (stringp name) (string-match-p "\\`[^/.][^/]*\\(?:/[^/.][^/]*\\)*\\'" name)))

(defun lispm-eseq-copy-example (kind example name)
  "Make a new instrument or effect, as KIND is, named NAME, from EXAMPLE,
one of ESeq's own as `lispm-eseq-examples' names them: its Lisp files are
copied into a new folder.  Return that folder."
  (let* ((source (cdr (assoc example (lispm-eseq-examples kind))))
         (target (expand-file-name name (lispm-eseq-path (if (eq kind 'instrument) "user-instruments" "user-effects")))))
    (cond ((not source) (user-error "ESeq has no %s %s" kind example))
          ((not (lispm-eseq--name-p name)) (user-error "Not a name for a folder: %s" name))
          ((file-exists-p target) (user-error "%s exists already" (abbreviate-file-name target))))
    (make-directory target t)
    (dolist (file (directory-files source t "\\.lisp\\'"))
      (copy-file file (expand-file-name (file-name-nondirectory file) target)))
    (run-hooks 'lispm-eseq-changed-hook)
    target))

(defconst lispm-eseq-sequencer-core
  ";; %s's tick: what ESeq's scheduler runs on every sixteenth while it plays.
;; It holds no widgets, as the scheduler imports only this module for it.

(module %s.core)

(export tick)

;; Play TRACK on every step whose cell is 1.
(def tick (cells track)
  (if (= (nth cells (mod (gen-tick) (len cells))) 1)
      (seq-emit :track track :at :now :note 0 :vel 0.8)
      nil))
"
  "A new sequencer's core module: its name, twice.")

(defconst lispm-eseq-sequencer-view
  ";; %s's panel and its kind, which ESeq offers as \"New %s\": sixteen
;; steps, each on or off, played on one track.

(module %s.view)

(import %s.core)

(export %s-panel)

;; CELLS with the one at INDEX turned over.
(def %s-flip (cells index)
  (map (lambda (j) (if (= j index) (- 1 (nth cells j)) (nth cells j)))
       (range 0 (len cells))))

(def %s-panel (self)
  (box :padding 1
    (h-stack :gap 0.2 :align :center
      (each (range 0 (len self.cells)) |i|
        (button (if (= (nth self.cells i) 1) \"x\" \".\")
          :key (str \"%s-cell-\" i)
          :width 2 :height 1
          :on-click (lambda (event) (set! self.cells (%s-flip self.cells i))))))))

(def-kind %s
  :generator (:resolution :16
              :requires (%s.core)
              :tick (%s.core/tick self.cells self.track))
  :document ((cells (list 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0))
             (track 0))
  :view %s-panel)
"
  "A new sequencer's view module: its name, thirteen times.")

(defun lispm-eseq-new-sequencer (name)
  "Make a new sequencer named NAME, lower-case letters, digits and hyphens:
a folder of Local modules, NAME.core with its tick and NAME.view with its
panel and its kind, NAME, which passes ESeq's check.  Return the view's
file."
  (unless (and (stringp name) (string-match-p "\\`[a-z][a-z0-9-]*\\'" name))
    (user-error "A sequencer's name is lower-case letters, digits and hyphens: %s" name))
  (let ((folder (expand-file-name name (lispm-eseq-path "local-packages"))))
    (when (file-exists-p folder)
      (user-error "%s exists already" (abbreviate-file-name folder)))
    (make-directory folder t)
    (with-temp-file (expand-file-name "core.lisp" folder)
      (insert (format lispm-eseq-sequencer-core name name)))
    (with-temp-file (expand-file-name "view.lisp" folder)
      (insert (apply #'format lispm-eseq-sequencer-view (make-list 13 name))))
    (run-hooks 'lispm-eseq-changed-hook)
    (expand-file-name "view.lisp" folder)))

;;;; Running eseq

(defun lispm-eseq--run (arguments buffer then)
  "Run eseq with ARGUMENTS, what it says going to BUFFER, if non-nil; then
call THEN with its exit status and what it said."
  (let ((program (or (lispm-eseq-program)
                     (user-error "ESeq is not installed: ESeq.app comes from %s" lispm-eseq-releases-url)))
        (output nil))
    (make-process
     :name "lispm-eseq" :noquery t :connection-type 'pipe
     :command (cons program arguments)
     :filter (lambda (_process text)
               (push text output)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (save-excursion
                       (goto-char (point-max))
                       (insert text))))))
     :sentinel (lambda (process _event)
                 (unless (process-live-p process)
                   (funcall then (process-exit-status process) (apply #'concat (nreverse output))))))))

;;;; Checking

(defvar lispm-eseq-checks (make-hash-table :test #'equal)
  "The last check of each thing checked, by (KIND . TARGET): a property
list as `lispm-eseq-parse-check' returns it, with :time.")

(defvar lispm-eseq--checking nil
  "What is being checked: (KIND . TARGET), each.")

(defconst lispm-eseq-check-buffer-name "*ESeq Check*"
  "Name of the buffer showing what ESeq's check says.")

(defun lispm-eseq-parse-check (status text)
  "Return what a check that exited with STATUS saying TEXT found, as
:passed, :stage and :error when it failed, :png, the picture of the panel
it drew, :warnings, and :lines, its lines of findings."
  (let* ((lines (split-string text "\n" t "[ \t\r]+"))
         (failure (cl-position-if (lambda (line) (string-match-p "\\`\\(?:eseq: \\)?FAIL " line)) lines))
         (png (seq-some (lambda (line)
                          (and (string-match "\\`ok +panel +\\(?:[^ :]+: \\)?\\(/.*\\.png\\)\\'" line)
                               (match-string 1 line)))
                        lines)))
    (list :passed (and (eq status 0) (seq-some (lambda (line) (string-prefix-p "PASS " line)) lines) t)
          :stage (and failure (string-match "(\\([^()]*\\))\\'" (nth failure lines)) (match-string 1 (nth failure lines)))
          :error (and failure (string-join (seq-take (nthcdr (1+ failure) lines) 12) "\n"))
          :failure (and failure (nth failure lines))
          :png png
          :warnings (seq-filter (lambda (line) (string-prefix-p "warn " line)) lines)
          :lines (seq-filter (lambda (line) (string-match-p "\\`\\(?:ok\\|warn\\|--\\|PASS\\) " line)) lines))))

(defun lispm-eseq-check (kind target &optional then no-render)
  "Have ESeq check TARGET, of KIND: an instrument's or an effect's folder,
compiled, played and its panel drawn; or a sequencer's module, read,
loaded and its panel drawn.  NO-RENDER leaves the panel undrawn.  What
it says goes to `lispm-eseq-check-buffer-name'.  THEN, if given, is
called with what it found, see `lispm-eseq-parse-check'."
  (unless (lispm-eseq-program)
    (user-error "ESeq is not installed: ESeq.app comes from %s" lispm-eseq-releases-url))
  (let ((key (cons kind target))
        (buffer (get-buffer-create lispm-eseq-check-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'special-mode) (special-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n%s %s, %s\n" (capitalize (symbol-name kind)) (if (eq kind 'sequencer) target (abbreviate-file-name target))
                        (format-time-string "%T")))))
    (push key lispm-eseq--checking)
    (run-hooks 'lispm-eseq-changed-hook)
    (lispm-eseq--run `(,(symbol-name kind) "check" ,target ,@(and no-render '("--no-render")))
                     buffer
                     (lambda (status text)
                       (let ((result (append (lispm-eseq-parse-check status text)
                                             (list :kind kind :target target :time (float-time)))))
                         (setq lispm-eseq--checking (delete key lispm-eseq--checking))
                         (puthash key result lispm-eseq-checks)
                         (run-hooks 'lispm-eseq-changed-hook)
                         (when then (funcall then result)))))))

(defun lispm-eseq-target (kind target)
  "Return what ESeq's check is to check for TARGET, of KIND, as an agent or
a user names it: an instrument's or effect's folder, or its name under
the user's own; a sequencer's module, or one of its files.  What is none
of them is given as it is, ESeq knowing names of its own, as
\"Synths/Heat\"."
  (pcase kind
    ((or 'instrument 'effect)
     (let ((mine (and (lispm-eseq--name-p target)
                      (expand-file-name target (lispm-eseq-path (if (eq kind 'instrument)
                                                                     "user-instruments"
                                                                   "user-effects"))))))
       (cond ((and (file-name-absolute-p target) (file-directory-p target))
              (directory-file-name (expand-file-name target)))
             ((and (file-name-absolute-p target) (file-exists-p target))
              (directory-file-name (file-name-directory (expand-file-name target))))
             ((and mine (file-directory-p mine)) mine)
             (t target))))
    ('sequencer
     (or (and (string-suffix-p ".lisp" target) (cdr (lispm-eseq-file-target target)))
         target))))

(defun lispm-eseq-checking-p (kind target)
  "Return non-nil while TARGET, of KIND, is being checked."
  (member (cons kind target) lispm-eseq--checking))

(defun lispm-eseq-last-check (kind target)
  "Return the last check of TARGET, of KIND, or nil."
  (gethash (cons kind target) lispm-eseq-checks))

(defun lispm-eseq-check-text (result)
  "Return RESULT, a check's, as words to read: what passed, or where it
failed and why."
  (if (plist-get result :passed)
      (string-join (append (plist-get result :lines)
                           (when (plist-get result :png)
                             (list (format "The panel ESeq drew: %s" (plist-get result :png)))))
                   "\n")
    (string-join (delq nil (list (or (plist-get result :failure) "FAIL")
                                 (plist-get result :error)))
                 "\n")))

;;;; Packages

(defun lispm-eseq-install-package (identity url &optional then)
  "Install the package IDENTITY, AUTHOR/NAME, from the git repository at
URL, with ESeq's own installer.  THEN, if given, is called with nil once
it is, else with what went wrong."
  (unless (string-match-p "\\`[a-z0-9][a-z0-9-]*/[a-z0-9][a-z0-9-]*\\'" identity)
    (user-error "A package is named AUTHOR/NAME, lower-case: %s" identity))
  (unless (string-match-p "\\`\\(?:https://\\|git@\\)[^ ]+\\'" url)
    (user-error "Not a git repository's address: %s" url))
  (lispm-eseq--run (list "package" "install" identity url) nil
                   (lambda (status text)
                     (run-hooks 'lispm-eseq-changed-hook)
                     (when then
                       (funcall then (unless (eq status 0) (string-trim text)))))))

(defun lispm-eseq-import-package (file &optional then)
  "Import the package FILE, a folder or an archive, with ESeq's own
importer.  THEN, if given, is called with nil once it is, else with what
went wrong."
  (lispm-eseq--run (list "package" "import" (expand-file-name file)) nil
                   (lambda (status text)
                     (run-hooks 'lispm-eseq-changed-hook)
                     (when then
                       (funcall then (unless (eq status 0) (string-trim text)))))))

;;;; Said in words

(defun lispm-eseq-report ()
  "Return where ESeq stands, as words to read: installed and running or
not, where what is made for it goes, and what was, with its last checks."
  (if (not (lispm-eseq-installed-p))
      (format "ESeq is not installed.  ESeq.app comes from %s." lispm-eseq-releases-url)
    (concat
     (format "ESeq %s, at %s, %s.\n" (or (lispm-eseq-version) "")
             (abbreviate-file-name (lispm-eseq-application))
             (if (lispm-eseq-running-p) "running" "not running"))
     (format "Its guide to making things: %s\n" (or (lispm-eseq-guide) "none"))
     (mapconcat (pcase-lambda (`(,key . ,label))
                  (format "%s: %s\n" label (or (lispm-eseq-path key) "unknown")))
                '(("user-instruments" . "Instruments go in")
                  ("user-effects" . "Effects go in")
                  ("local-packages" . "Sequencers go in")
                  ("factory-instruments" . "ESeq's own instruments, to read")
                  ("factory-effects" . "ESeq's own effects, to read"))
                "")
     (mapconcat
      (lambda (kind)
        (let ((items (lispm-eseq-items kind)))
          (concat (format "\n%s %s: %d\n" (capitalize (format "%ss" kind))
                          (if (eq kind 'package) "installed" "made")
                          (length items))
                  (mapconcat (lambda (item) (concat "- " (lispm-eseq-item-text item) "\n")) items ""))))
      lispm-eseq-kinds ""))))

(defun lispm-eseq-item-key (item)
  "Return what ITEM is checked as, (KIND . TARGET), or nil for a package.
A sequencer's module declaring no kind is checked with the one beside it
that does, which imports it."
  (pcase (plist-get item :kind)
    ((and kind (or 'instrument 'effect)) (cons kind (plist-get item :folder)))
    ('sequencer (if (plist-get item :kinds)
                    (cons 'sequencer (plist-get item :module))
                  (lispm-eseq-file-target (plist-get item :file))))))

(defun lispm-eseq-failure-text (result)
  "Return where RESULT, a check that failed, failed and why, in a line."
  (let ((error (seq-find (lambda (line) (not (string-suffix-p ":" line)))
                         (split-string (or (plist-get result :error) "") "\n" t))))
    (string-join (delq nil (list (plist-get result :stage) error)) ": ")))

(defun lispm-eseq-check-state (item)
  "Return how ITEM's last check went: `passed', `failed', `checking', or
nil when it was not checked."
  (when-let* ((key (lispm-eseq-item-key item)))
    (cond ((lispm-eseq-checking-p (car key) (cdr key)) 'checking)
          ((lispm-eseq-last-check (car key) (cdr key))
           (if (plist-get (lispm-eseq-last-check (car key) (cdr key)) :passed) 'passed 'failed)))))

(defun lispm-eseq-item-text (item)
  "Return ITEM, as `lispm-eseq-items' gives it, as a line to read."
  (let ((key (lispm-eseq-item-key item)))
    (concat (plist-get item :name)
            (pcase (plist-get item :kind)
              ((or 'instrument 'effect) (format ", in %s" (abbreviate-file-name (plist-get item :folder))))
              ('sequencer (concat (format ", %s" (abbreviate-file-name (plist-get item :file)))
                                  (when (plist-get item :kinds)
                                    (format ", declaring the kind %s" (string-join (plist-get item :kinds) ", ")))))
              ('package (concat (when (plist-get item :version) (format " %s" (plist-get item :version)))
                                (when (plist-get item :kinds)
                                  (format ", kinds %s" (string-join (plist-get item :kinds) ", "))))))
            (pcase (lispm-eseq-check-state item)
              ('passed ", passed its last check")
              ('failed (format ", failed its last check, %s"
                               (lispm-eseq-failure-text (lispm-eseq-last-check (car key) (cdr key)))))
              ('checking ", being checked")))))

(provide 'lispm-eseq)
;;; lispm-eseq.el ends here

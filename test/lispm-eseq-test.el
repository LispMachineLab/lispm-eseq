;;; lispm-eseq-test.el --- Tests of lispm-eseq  -*- lexical-binding: t; -*-

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

;; The tests run test/fake-eseq in a fake ESeq.app, never ESeq: what it
;; keeps goes to a directory of the test's own.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-eseq)
(require 'lispm-eseq-lisp)
(require 'lispm-eseq-ui)

(defconst lispm-eseq-test--fake
  (expand-file-name "fake-eseq" (file-name-directory (or load-file-name buffer-file-name)))
  "The stand-in for ESeq's program.")

(defun lispm-eseq-test--write (file text)
  "Write TEXT into FILE, its directories made."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text)))

(defmacro lispm-eseq-test--with-eseq (&rest body)
  "Run BODY with a fake ESeq.app, APPLICATION, whose program keeps what
users make under ROOT and its own under FACTORY."
  (declare (indent 0))
  `(let* ((root (make-temp-file "lispm-eseq-test" t))
          (factory (expand-file-name "factory" root))
          (application (expand-file-name "Applications/ESeq.app" root))
          (lispm-eseq-application application)
          (lispm-eseq--paths nil)
          (lispm-eseq-checks (make-hash-table :test #'equal))
          (lispm-eseq--checking nil)
          (lispm-eseq-message nil)
          (lispm-eseq-changed-hook (list (function lispm-eseq--refresh)))
          (process-environment (append (list (concat "LISPM_ESEQ_TEST_ROOT=" root)
                                             (concat "LISPM_ESEQ_TEST_FACTORY=" factory))
                                       process-environment)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "Contents/MacOS" application) t)
           (copy-file lispm-eseq-test--fake (expand-file-name "Contents/MacOS/eseq" application))
           (set-file-modes (expand-file-name "Contents/MacOS/eseq" application) #o755)
           (lispm-eseq-test--write (expand-file-name "Contents/Info.plist" application)
                                   "<plist><dict>
  <key>CFBundleShortVersionString</key>
  <string>0.1.0</string>
  <key>CFBundleVersion</key>
  <string>31e088f8</string>
</dict></plist>\n")
           (lispm-eseq-test--write (expand-file-name "instruments/Synths/Revsynt/dsp.lisp" factory) "(dsp)\n")
           (lispm-eseq-test--write (expand-file-name "instruments/Synths/Revsynt/ui.lisp" factory) "(ui)\n")
           (lispm-eseq-test--write (expand-file-name "instruments/Synths/Revsynt/dsp.layout.json" factory) "{}\n")
           (lispm-eseq-test--write (expand-file-name "instruments/Synths/Revsynt.presets/Init.json" factory) "{}\n")
           (lispm-eseq-test--write (expand-file-name "effects/stereo-tremolo/dsp.lisp" factory) "(dsp)\n")
           ,@body)
       (when-let* ((buffer (get-buffer lispm-eseq-buffer-name))) (kill-buffer buffer))
       (when-let* ((buffer (get-buffer lispm-eseq-check-buffer-name))) (kill-buffer buffer))
       (delete-directory root t))))

(defun lispm-eseq-test--check (kind target &optional no-render)
  "Check TARGET, of KIND, and return what the check found."
  (let (result)
    (lispm-eseq-check kind target (lambda (found) (setq result found)) no-render)
    (with-timeout (10 (error "The check did not end"))
      (while (not result) (accept-process-output nil 0.05)))
    result))

(defun lispm-eseq-test--wait (predicate)
  "Wait until PREDICATE returns non-nil, ten seconds at most."
  (with-timeout (10 (error "Waited in vain"))
    (while (not (funcall predicate)) (accept-process-output nil 0.05))))

;;;; ESeq.app

(ert-deftest lispm-eseq-test-application ()
  "ESeq.app is found where it is said to be, its program in it, and its
version read from its Info.plist; without it, nothing is."
  (lispm-eseq-test--with-eseq
    (should (equal (lispm-eseq-application) application))
    (should (equal (lispm-eseq-program) (expand-file-name "Contents/MacOS/eseq" application)))
    (should (lispm-eseq-installed-p))
    (should (equal (lispm-eseq-version) "0.1.0 (31e088f8)"))
    (let ((lispm-eseq-application (expand-file-name "Nowhere/ESeq.app" root)))
      (should-not (lispm-eseq-application))
      (should-not (lispm-eseq-installed-p))
      (should-not (lispm-eseq-version))
      (should (string-prefix-p "ESeq is not installed." (lispm-eseq-report))))))

(ert-deftest lispm-eseq-test-running ()
  "ESeq runs while its window's program does, by its whole command line,
quoted as pgrep reads it; a check's drawing, with arguments, is not it."
  (should (equal (lispm-eseq--extended-regexp "/A (b)/x.app/c+d|e") "/A \\(b\\)/x\\.app/c\\+d\\|e"))
  (lispm-eseq-test--with-eseq
    (let (asked)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (program _infile _buffer _display &rest arguments)
                   (setq asked (cons program arguments))
                   0)))
        (should (lispm-eseq-running-p)))
      (should (equal asked (list "pgrep" "-fx"
                                 (lispm-eseq--extended-regexp
                                  (expand-file-name "Contents/MacOS/metal_seq" application))))))))

(ert-deftest lispm-eseq-test-paths ()
  "Where ESeq keeps things is what its program says, read once, paths
with spaces whole; without it, where ESeq.app would keep them."
  (should (equal (lispm-eseq-parse-paths
                  "user-root           /Users/a/Library/Application Support/com.universalsequences.eseq
local-packages      /Users/a/.eseq.d/packages/local\n")
                 '(("user-root" . "/Users/a/Library/Application Support/com.universalsequences.eseq")
                   ("local-packages" . "/Users/a/.eseq.d/packages/local"))))
  (lispm-eseq-test--with-eseq
    (should (equal (lispm-eseq-path "user-instruments") (expand-file-name "support/instruments" root)))
    (should (equal (lispm-eseq-path "local-packages") (expand-file-name "eseq.d/packages/local" root)))
    (should (equal (lispm-eseq-path "factory-effects") (expand-file-name "effects" factory)))
    (lispm-eseq-path "packages")
    (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "calls" root)) (buffer-string))
                   "paths\n"))
    (let ((lispm-eseq-application (expand-file-name "Nowhere/ESeq.app" root)))
      (should (equal (lispm-eseq-path "local-packages") (expand-file-name "~/.eseq.d/packages/local"))))))

(ert-deftest lispm-eseq-test-guide ()
  "ESeq's guide is written once, by its program, then read where it is."
  (lispm-eseq-test--with-eseq
    (should (equal (lispm-eseq-guide) (expand-file-name "support/AGENTS.md" root)))
    (should (equal (lispm-eseq-guide) (expand-file-name "support/AGENTS.md" root)))
    (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "calls" root)) (buffer-string))
                   "paths\nauthoring seed\n"))))

;;;; What was made for it

(ert-deftest lispm-eseq-test-items ()
  "Instruments and effects are the folders holding a dsp.lisp, those of
folders too; sequencers, the Local modules, named as ESeq names them, with
the kinds they declare; packages, the folders with a manifest."
  (lispm-eseq-test--with-eseq
    (let ((instruments (lispm-eseq-path "user-instruments"))
          (local (lispm-eseq-path "local-packages"))
          (packages (lispm-eseq-path "packages")))
      (lispm-eseq-test--write (expand-file-name "Fold Poly/dsp.lisp" instruments) "(dsp)\n")
      (lispm-eseq-test--write (expand-file-name "Bass/Acid Mono/dsp.lisp" instruments) "(dsp)\n")
      (lispm-eseq-test--write (expand-file-name "Bass/notes.txt" instruments) "\n")
      (lispm-eseq-test--write (expand-file-name "Empty/ui.lisp" instruments) "(ui)\n")
      (lispm-eseq-test--write (expand-file-name "pulse/core.lisp" local) "(module pulse.core)\n")
      (lispm-eseq-test--write (expand-file-name "pulse/view.lisp" local)
                              "(module pulse.view)\n\n(def-kind pulse\n  :view pulse-panel)\n")
      (lispm-eseq-test--write (expand-file-name "alec.euclid/manifest.json" packages)
                              "{\"name\": \"alec/euclid\", \"version\": \"0.1.0\", \"kinds\": [{\"name\": \"euclid\", \"module\": \"alec.euclid.rings\"}]}")
      (should (equal (mapcar (lambda (item) (plist-get item :name)) (lispm-eseq-items 'instrument))
                     '("Bass/Acid Mono" "Fold Poly")))
      (should (equal (plist-get (car (lispm-eseq-items 'instrument)) :folder)
                     (expand-file-name "Bass/Acid Mono" instruments)))
      (should-not (lispm-eseq-items 'effect))
      (should (equal (mapcar (lambda (item) (list (plist-get item :module) (plist-get item :kinds)))
                             (lispm-eseq-items 'sequencer))
                     '(("pulse.core" nil) ("pulse.view" ("pulse")))))
      (should (equal (mapcar (lambda (item) (list (plist-get item :name) (plist-get item :version) (plist-get item :kinds)))
                             (lispm-eseq-items 'package))
                     '(("alec/euclid" "0.1.0" ("euclid")))))
      (should (equal (lispm-eseq-examples 'instrument)
                     (list (cons "Synths/Revsynt" (expand-file-name "instruments/Synths/Revsynt" factory)))))
      (should (string-search "Sequencers made: 2" (lispm-eseq-report)))
      (should (string-search "- pulse.view, " (lispm-eseq-report)))
      (should (string-search "Packages installed: 1\n- alec/euclid 0.1.0, kinds euclid" (lispm-eseq-report))))))

(ert-deftest lispm-eseq-test-file-target ()
  "A file is checked as what it belongs to: its instrument's or effect's
folder, or its sequencer's module declaring the kind, which a core module
without one is checked with."
  (lispm-eseq-test--with-eseq
    (let ((instruments (lispm-eseq-path "user-instruments"))
          (effects (lispm-eseq-path "user-effects"))
          (local (lispm-eseq-path "local-packages")))
      (lispm-eseq-test--write (expand-file-name "Fold Poly/dsp.lisp" instruments) "(dsp)\n")
      (lispm-eseq-test--write (expand-file-name "Echo/dsp.lisp" effects) "(dsp)\n")
      (lispm-eseq-test--write (expand-file-name "pulse/core.lisp" local) "(module pulse.core)\n")
      (lispm-eseq-test--write (expand-file-name "pulse/view.lisp" local) "(module pulse.view)\n(def-kind pulse)\n")
      (should (equal (lispm-eseq-file-target (expand-file-name "Fold Poly/ui.lisp" instruments))
                     (cons 'instrument (expand-file-name "Fold Poly" instruments))))
      (should (equal (lispm-eseq-file-target (expand-file-name "Echo/dsp.lisp" effects))
                     (cons 'effect (expand-file-name "Echo" effects))))
      (should (equal (lispm-eseq-file-target (expand-file-name "pulse/core.lisp" local)) '(sequencer . "pulse.view")))
      (should (equal (lispm-eseq-file-target (expand-file-name "pulse/view.lisp" local)) '(sequencer . "pulse.view")))
      (should (equal (lispm-eseq-module-name (expand-file-name "a/b/c.lisp" local)) "a.b.c"))
      (should-not (lispm-eseq-module-name (expand-file-name "x.lisp" root)))
      (should-not (lispm-eseq-file-target (expand-file-name "notes.lisp" root))))))

;;;; Making

(ert-deftest lispm-eseq-test-copy-example ()
  "An instrument made from one of ESeq's own gets its Lisp, not its
presets nor its layout, in a folder of its own; a name taken or not a
folder's is refused."
  (lispm-eseq-test--with-eseq
    (let ((folder (lispm-eseq-copy-example 'instrument "Synths/Revsynt" "My Rev")))
      (should (equal folder (expand-file-name "My Rev" (lispm-eseq-path "user-instruments"))))
      (should (equal (directory-files folder nil "\\`[^.]") '("dsp.lisp" "ui.lisp")))
      (should-error (lispm-eseq-copy-example 'instrument "Synths/Revsynt" "My Rev") :type 'user-error)
      (should-error (lispm-eseq-copy-example 'instrument "Synths/Revsynt" "../Out") :type 'user-error)
      (should-error (lispm-eseq-copy-example 'instrument "Synths/Nothing" "Mine") :type 'user-error)
      (should (equal (lispm-eseq-copy-example 'effect "stereo-tremolo" "Wobble")
                     (expand-file-name "Wobble" (lispm-eseq-path "user-effects"))))
      (should (equal (lispm-eseq-copy-example 'instrument "Synths/Revsynt" "Bass/Acid Mono")
                     (expand-file-name "Bass/Acid Mono" (lispm-eseq-path "user-instruments"))))
      (should-error (lispm-eseq-copy-example 'instrument "Synths/Revsynt" "Bass/.hidden") :type 'user-error))))

(ert-deftest lispm-eseq-test-target ()
  "What is checked is found as it is named: an instrument by its name under
the user's own, its folder or a file of it; a sequencer by its module or
a file of it; a name ESeq knows itself is left to it."
  (lispm-eseq-test--with-eseq
    (let ((instruments (lispm-eseq-path "user-instruments"))
          (local (lispm-eseq-path "local-packages")))
      (lispm-eseq-test--write (expand-file-name "Bass/Acid/dsp.lisp" instruments) "(dsp)\n")
      (lispm-eseq-new-sequencer "pulse")
      (should (equal (lispm-eseq-target 'instrument "Bass/Acid") (expand-file-name "Bass/Acid" instruments)))
      (should (equal (lispm-eseq-target 'instrument (expand-file-name "Bass/Acid/" instruments))
                     (expand-file-name "Bass/Acid" instruments)))
      (should (equal (lispm-eseq-target 'instrument (expand-file-name "Bass/Acid/dsp.lisp" instruments))
                     (expand-file-name "Bass/Acid" instruments)))
      (should (equal (lispm-eseq-target 'instrument "Synths/Heat") "Synths/Heat"))
      (should (equal (lispm-eseq-target 'sequencer "pulse.view") "pulse.view"))
      (should (equal (lispm-eseq-target 'sequencer (expand-file-name "pulse/core.lisp" local)) "pulse.view")))))

(ert-deftest lispm-eseq-test-new-sequencer ()
  "A new sequencer is a core module with its tick and a view declaring its
kind, all named after it; a name that is not a module's is refused."
  (lispm-eseq-test--with-eseq
    (let* ((view (lispm-eseq-new-sequencer "my-beat"))
           (core (expand-file-name "core.lisp" (file-name-directory view)))
           (text (lambda (file) (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (should (equal view (expand-file-name "my-beat/view.lisp" (lispm-eseq-path "local-packages"))))
      (should (string-search "(module my-beat.core)" (funcall text core)))
      (should (string-search "(module my-beat.view)" (funcall text view)))
      (should (string-search "(import my-beat.core)" (funcall text view)))
      (should (string-search ":tick (my-beat.core/tick self.cells self.track)" (funcall text view)))
      (should (string-search ":view my-beat-panel)" (funcall text view)))
      (should-not (string-search "%" (funcall text view)))
      (should (equal (lispm-eseq--kinds-in view) '("my-beat")))
      (should (equal (lispm-eseq-file-target core) '(sequencer . "my-beat.view")))
      (should-error (lispm-eseq-new-sequencer "my-beat") :type 'user-error)
      (should-error (lispm-eseq-new-sequencer "My Beat") :type 'user-error))))

;;;; Checking

(ert-deftest lispm-eseq-test-parse-check ()
  "A check's lines say whether it passed, the panel it drew, and, failed,
at what and why."
  (let ((passed (lispm-eseq-parse-check
                 0 "ok    compile   15 param(s), 9 input(s), 3 output(s)
warn  a knob has no label
ok    panel     /tmp/eseq-check/user_Rev_Test_.png
PASS /x/instruments/Rev Test\n")))
    (should (plist-get passed :passed))
    (should (equal (plist-get passed :png) "/tmp/eseq-check/user_Rev_Test_.png"))
    (should (equal (plist-get passed :warnings) '("warn  a knob has no label")))
    (should (equal (lispm-eseq-check-text passed)
                   (concat "ok    compile   15 param(s), 9 input(s), 3 output(s)\nwarn  a knob has no label\n"
                           "ok    panel     /tmp/eseq-check/user_Rev_Test_.png\nPASS /x/instruments/Rev Test\n"
                           "The panel ESeq drew: /tmp/eseq-check/user_Rev_Test_.png"))))
  (should (equal (plist-get (lispm-eseq-parse-check 0 "ok    panel     beat: /tmp/beat_view-beat.png\nPASS beat.view\n") :png)
                 "/tmp/beat_view-beat.png"))
  (let ((failed (lispm-eseq-parse-check
                 1 "[dgenlisp compile failed] kind=instrument
source:
(defparam oops
eseq: FAIL /x/instruments/Broken (dsp compile)
compile error:
Error: Parse error: Missing closing parenthesis\n")))
    (should-not (plist-get failed :passed))
    (should (equal (plist-get failed :stage) "dsp compile"))
    (should (equal (plist-get failed :failure) "eseq: FAIL /x/instruments/Broken (dsp compile)"))
    (should (equal (lispm-eseq-failure-text failed) "dsp compile: Error: Parse error: Missing closing parenthesis"))
    (should (equal (lispm-eseq-check-text failed)
                   "eseq: FAIL /x/instruments/Broken (dsp compile)\ncompile error:\nError: Parse error: Missing closing parenthesis")))
  (should-not (plist-get (lispm-eseq-parse-check 1 "PASS x\n") :passed)))

(ert-deftest lispm-eseq-test-check ()
  "A check runs ESeq's own, says it in its buffer, and is kept as the
last of what it checked; while it runs, that is known, and the window
told."
  (lispm-eseq-test--with-eseq
    (let* ((instruments (lispm-eseq-path "user-instruments"))
           (good (expand-file-name "Good" instruments))
           (bad (expand-file-name "Bad" instruments))
           (told 0))
      (lispm-eseq-test--write (expand-file-name "dsp.lisp" good) "(dsp)\n")
      (lispm-eseq-test--write (expand-file-name "dsp.lisp" bad) "(dsp BROKEN\n")
      (add-hook 'lispm-eseq-changed-hook (lambda () (cl-incf told)))
      (let ((result (lispm-eseq-test--check 'instrument good)))
        (should (plist-get result :passed))
        (should (equal (plist-get result :png) (expand-file-name "panel.png" root)))
        (should (eq (lispm-eseq-check-state (list :kind 'instrument :folder good)) 'passed))
        (should (equal (lispm-eseq-last-check 'instrument good) result)))
      (should (= told 2))
      (should-not (plist-get (lispm-eseq-test--check 'instrument good t) :png))
      (let ((result (lispm-eseq-test--check 'instrument bad)))
        (should-not (plist-get result :passed))
        (should (equal (plist-get result :stage) "dsp compile"))
        (should (eq (lispm-eseq-check-state (list :kind 'instrument :folder bad)) 'failed))
        (should (string-search "failed its last check, dsp compile: Error: Parse error"
                               (lispm-eseq-item-text (list :kind 'instrument :name "Bad" :folder bad)))))
      (with-current-buffer lispm-eseq-check-buffer-name
        (should (string-search "PASS " (buffer-string)))
        (should (string-search "FAIL " (buffer-string))))
      (lispm-eseq-check 'sequencer "nothing.view")
      (should (lispm-eseq-checking-p 'sequencer "nothing.view"))
      (should (eq (lispm-eseq-check-state (list :kind 'sequencer :module "nothing.view" :kinds '("nothing")))
                  'checking))
      (lispm-eseq-test--wait (lambda () (not (lispm-eseq-checking-p 'sequencer "nothing.view"))))
      (should (equal (plist-get (lispm-eseq-last-check 'sequencer "nothing.view") :stage) "module"))
      (let ((lispm-eseq-application (expand-file-name "Nowhere/ESeq.app" root)))
        (should-error (lispm-eseq-check 'instrument good) :type 'user-error)
        (should-not lispm-eseq--checking)))))

;;;; Packages

(ert-deftest lispm-eseq-test-install-package ()
  "A package is installed by ESeq's installer from git, named AUTHOR/NAME;
what goes wrong is said."
  (lispm-eseq-test--with-eseq
    (should-error (lispm-eseq-install-package "Alec Euclid" "https://example.org/e.git") :type 'user-error)
    (should-error (lispm-eseq-install-package "alec/euclid" "file:///tmp/e") :type 'user-error)
    (let ((answer 'waiting))
      (lispm-eseq-install-package "alec/euclid" "https://example.org/euclid.git" (lambda (error) (setq answer error)))
      (lispm-eseq-test--wait (lambda () (not (eq answer 'waiting))))
      (should-not answer)
      (should (equal (mapcar (lambda (item) (plist-get item :name)) (lispm-eseq-items 'package)) '("alec/euclid")))
      (setq answer 'waiting)
      (lispm-eseq-install-package "alec/gone" "https://example.org/missing.git" (lambda (error) (setq answer error)))
      (lispm-eseq-test--wait (lambda () (not (eq answer 'waiting))))
      (should (equal answer "eseq: git clone failed: repository not found")))))

;;;; The window

(ert-deftest lispm-eseq-test-window ()
  "The window shows ESeq and what was made for it, under a heading each,
with how each was last checked; RET on a heading makes one, on a line
edits it; without ESeq, it says where it comes from."
  (lispm-eseq-test--with-eseq
    (let ((instruments (lispm-eseq-path "user-instruments")))
      (lispm-eseq-test--write (expand-file-name "Fold Poly/dsp.lisp" instruments) "(dsp)\n")
      (cl-letf (((symbol-function 'lispm-eseq-running-p) #'ignore))
        (lispm-eseq)
        (with-current-buffer lispm-eseq-buffer-name
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-prefix-p "ESeq 0.1.0 (31e088f8)\n" text))
            (should (string-search "○ Not running" text))
            (dolist (heading '("Instruments" "Effects" "Sequencers" "Packages"))
              (should (string-search (concat "\n" heading) text)))
            (should (string-search "Fold Poly" text))
            (should (string-search "Not checked: c checks it" text)))
          (should (equal (lispm-eseq-line) '(eseq)))
          (lispm-eseq-next-line)
          (should (equal (lispm-eseq-line) '(heading . instrument)))
          (lispm-eseq-next-line)
          (should (equal (plist-get (lispm-eseq-line) :name) "Fold Poly"))
          (let (edited)
            (cl-letf (((symbol-function 'find-file-other-window) (lambda (file) (setq edited file))))
              (lispm-eseq-act))
            (should (equal edited (expand-file-name "Fold Poly/dsp.lisp" instruments))))
          (lispm-eseq-check-here)
          (lispm-eseq-test--wait (lambda () (not lispm-eseq--checking)))
          (should (equal (plist-get (lispm-eseq-line) :name) "Fold Poly"))
          (should (string-search "Passed at " (buffer-string)))
          (let (made)
            (cl-letf (((symbol-function 'lispm-eseq-new) (lambda (kind) (setq made kind))))
              (lispm-eseq-previous-line)
              (lispm-eseq-act))
            (should (eq made 'instrument)))))
      (let ((lispm-eseq-application (expand-file-name "Nowhere/ESeq.app" root)))
        (lispm-eseq-reload)
        (with-current-buffer lispm-eseq-buffer-name
          (should (string-prefix-p "ESeq is not installed" (buffer-string)))
          (should (string-search lispm-eseq-releases-url (buffer-string))))))))

;;;; ESeq's Lisp

(ert-deftest lispm-eseq-test-lisp-mode ()
  "ESeq's Lisp files open in its mode, wherever ESeq keeps them, and its
keys check them; |i| names a variable."
  (dolist (file '("/Users/a/.eseq.d/packages/local/pulse/view.lisp"
                  "/Users/a/Library/Application Support/com.universalsequences.eseq/instruments/Bass/dsp.lisp"
                  "/Applications/ESeq.app/Contents/Resources/ui/main.lisp"))
    (should (eq (assoc-default file auto-mode-alist #'string-match) 'lispm-eseq-lisp-mode)))
  (should-not (eq (assoc-default "/Users/a/src/x.lisp" auto-mode-alist #'string-match) 'lispm-eseq-lisp-mode))
  (with-temp-buffer
    (lispm-eseq-lisp-mode)
    (should (eq (key-binding (kbd "C-c C-c")) #'lispm-eseq-lisp-check))
    (insert "(each cells |i| (nth cells i))")
    (goto-char 13)
    (should (equal (thing-at-point 'symbol t) "|i|"))
    (should-error (lispm-eseq-lisp-check) :type 'user-error)))

(provide 'lispm-eseq-test)
;;; lispm-eseq-test.el ends here

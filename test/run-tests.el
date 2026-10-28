;;; run-tests.el --- Batch runner for ox-conf tests -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -L test -l test/run-tests.el

;;; Code:

(defvar ox-conf--test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this runner, e.g. .../ox-conf/test/.")

(defvar ox-conf--root-dir
  (expand-file-name ".." ox-conf--test-dir)
  "Repository root, e.g. .../ox-conf/.")

;; Third-party deps (`request' and friends) live in the user's elpa dir.
(let ((elpa (expand-file-name "~/.emacs.d/elpa")))
  (when (file-directory-p elpa)
    (dolist (d (directory-files elpa))
      (unless (member d '("." ".."))
        (add-to-list 'load-path (expand-file-name d elpa))))))

(add-to-list 'load-path ox-conf--root-dir)

(require 'ert)
(require 'ox-conf)
;; Keep the suite's debug log out of the user's real *ox-conf-log* buffer.
(setq org-conf-log-buffer-name " *ox-conf-test-log*")
(load (expand-file-name "ox-conf-test.el" ox-conf--test-dir) nil t)

(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here

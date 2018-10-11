;;; grep-dired.el --- Grep filename in dired.

;; Filename: grep-dired.el
;; Description: Grep filename in dired.
;;
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;;
;; Created: 2018-10-12 00:06:02
;; Version: 0.1
;; Last-Updated: 2018-10-12 00:06:02
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/grep-dired.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `dired'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Grep filename in dired.
;;
;; This extension is fork find-dired.el
;;

;;; Installation:
;;
;; Put grep-dired.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'grep-dired)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET grep-dired RET
;;

;;; Change log:
;;
;; 2018/10/12
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'dired)

;;; Code:

(defgroup grep-dired nil
  "Run a `find' command and Dired the output."
  :group 'grep-dired)

(defcustom grep-dired-find-exec-terminator
  (if (eq 0
          (ignore-errors
            (process-file find-program nil nil nil
                          null-device "-exec" "echo" "{}" "+")))
      "+"
    (shell-quote-argument ";"))
  "String that terminates \"find -exec COMMAND {} \".
The value should include any needed quoting for the shell.
Common values are \"+\" and \"\\\\;\", with the former more efficient
than the latter."
  :group 'grep-dired
  :type 'string)

(defcustom grep-dired-find-ls-option
  (cons "-ls" "-aluh")
  "A pair of options to produce and parse an `ls -l'-type list from `find'."
  :type '(cons (string :tag "Find Option")
               (string :tag "Ls Switches"))
  :group 'grep-dired)

(defvar grep-dired-find-args nil
  "Last arguments given to `find' by \\[grep-dired].")

(defvar grep-dired-find-args-history nil)

(defvar dired-sort-inhibit)

;;;###autoload
(defun grep-dired-dwim (file)
  (interactive "sGrep file: ")
  (grep-dired default-directory (format "-type f -name '*%s*'" file)))

(defun grep-dired (dir args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    find . \\( ARGS \\) -ls

except that the car of the variable `grep-dired-find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
                     (read-string "Run find (with args): " grep-dired-find-args
                                  '(grep-dired-find-args-history . 1))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "grep-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "Grep Dired"))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
        (if (or (not (eq (process-status find) 'run))
                (yes-or-no-p
                 (format-message "A `find' process is running; kill it? ")))
            (condition-case nil
                (progn
                  (interrupt-process find)
                  (sit-for 1)
                  (delete-process find))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
          grep-dired-find-args args   ; save for next interactive call
          args (concat find-program " . "
                       (if (string= args "")
                           ""
                         (concat
                          (shell-quote-argument "(")
                          " " args " "
                          (shell-quote-argument ")")
                          " "))
                       (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                         (car grep-dired-find-ls-option))
                           (format "%s %s %s"
                                   (match-string 1 (car grep-dired-find-ls-option))
                                   (shell-quote-argument "{}")
                                   grep-dired-find-exec-terminator)
                         (car grep-dired-find-ls-option))))
    ;; Start the find process.
    (shell-command (concat args "&") (current-buffer))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr grep-dired-find-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'grep-dired-kill-find)
      (use-local-map map))
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
         `(lambda (ignore-auto noconfirm)
            (grep-dired ,dir ,grep-dired-find-args)))
    (set (make-local-variable 'dired-subdir-alist)
         (list (cons default-directory (point-min-marker))))
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (let ((point (point)))
      (insert "  " args "\n")
      (dired-insert-set-properties point (point)))
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function grep-dired-filter))
      (set-process-sentinel proc (function grep-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) (point) (current-buffer)))
    ))

(defun grep-dired-kill-find ()
  "Kill the `find' process running in the current buffer."
  (interactive)
  (let ((find (get-buffer-process (current-buffer))))
    (and find (eq (process-status find) 'run)
         (eq (process-filter find) (function grep-dired-filter))
         (condition-case nil
             (delete-process find)
           (error nil)))))

(defun grep-dired-filter (proc string)
  ;; Filter for \\[grep-dired] processes.
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only nil)
                    (beg (point-max))
                    (l-opt (and (consp grep-dired-find-ls-option)
                                (string-match "l" (cdr grep-dired-find-ls-option))))
                    (ls-regexp (concat "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
                                       "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[^[:space:]]+\\)")))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                ;; Convert ` ./FILE' to ` FILE'
                ;; This would lose if the current chunk of output
                ;; starts or ends within the ` ./', so back up a bit:
                (goto-char (- beg 3))   ; no error if < 0
                (while (search-forward " ./" nil t)
                  (delete-region (point) (- (point) 2)))
                ;; Pad the number of links and file size.  This is a
                ;; quick and dirty way of getting the columns to line up
                ;; most of the time, but it's not foolproof.
                (when l-opt
                  (goto-char beg)
                  (goto-char (line-beginning-position))
                  (while (re-search-forward ls-regexp nil t)
                    (replace-match (format "%4s" (match-string 1))
                                   nil nil nil 1)
                    (replace-match (format "%9s" (match-string 2))
                                   nil nil nil 2)
                    (forward-line 1)))
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun grep-dired-sentinel (proc state)
  ;; Sentinel for \\[grep-dired] processes.
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (let ((buffer-read-only nil))
            (save-excursion
              (goto-char (point-max))
              (let ((point (point)))
                (insert "\n")
                (dired-insert-set-properties point (point)))
              (delete-process proc)
              ))
          (message "grep-dired %s finished." (current-buffer))))))

(provide 'grep-dired)

;;; grep-dired.el ends here

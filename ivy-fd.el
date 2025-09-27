;;; ivy-fd.el --- Ivy interface for fd -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-fd
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "27.1") (transient "0.9.3") (ivy "0.14.2"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ivy interface for fd

;;; Code:


(require 'ivy)
(require 'transient)

(defcustom ivy-fd-exec-path (or (executable-find "fdfind")
                                (executable-find "fd"))
  "Path to fd program."
  :group 'ivy-fd
  :type 'string)

(defcustom ivy-fd-resolve-project-root-fn 'ivy-fd-resolve-project-root
  "Function to find project directory."
  :group 'ivy-fd
  :type 'function)

(defvar ivy-fd-boolean-options
  '(:hidden
    :no-ignore
    :no-ignore-vcs
    :full-path
    :follow
    :absolute-path
    :fixed-strings
    :glob
    :ignore-case
    :case-sensitive))

(defun ivy-fd--mark-candidates (candidates)
  "Mark CANDIDATES from ivy collection."
  (dolist (cand (ivy-state-collection
                 ivy-last))
    (when (member cand
                  candidates)
      (let ((marked-cand (concat
                          ivy-mark-prefix
                          cand)))
        (setq ivy--old-cands
              ivy--all-candidates)
        (setcar
         (member cand
                 ivy--all-candidates)
         (setcar
          (member cand
                  ivy--old-cands)
          marked-cand))
        (setq ivy-marked-candidates
              (append
               ivy-marked-candidates
               (list
                marked-cand)))))))

(defun ivy-fd--plist-omit (plist keywords)
  "Omit KEYWORDS with it's values from PLIST."
  (if (seq-find (lambda (it) (memq it plist)) keywords)
      (let ((result))
        (while plist
          (let* ((key (pop plist))
                 (val (pop plist)))
            (unless (memq key keywords)
              (push (list key val) result))))
        (reverse result))
    plist))

(defvar ivy-fd--ivy-read-keywords
  '(:predicate :require-match :initial-input
               :history :preselect
               :def :keymap :update-fn :sort
               :unwind :re-builder :matcher
               :dynamic-collection
               :extra-props
               :action :multi-action))

(defvar ivy-fd--configure-keywords
  '(:parent :initial-input :height :occur
            :update-fn :init-fn :unwind-fn
            :index-fn :sort-fn :sort-matches-fn
            :format-fn :display-fn :display-transformer-fn
            :alt-done-fn :more-chars :grep-p :exit-codes))

(defun ivy-fd--read-multi (prompt collection &rest ivy-args)
  "Read COLLECTION with PROMPT and return list with selected candidates.
IVY-ARGS are combined args both from `ivy-read' and `ivy-configure',
excluding:

- :action
- :multi-action
- :caller

but accepting:

- :persistent-action
- :premarked

Persistent action will be called with current candidate without exiting
completion.

Premarked is candidates from COLLECTION which should be initially marked."
  (interactive)
  (dolist (alist-sym '(ivy--parents-alist
                       ivy-initial-inputs-alist
                       ivy-height-alist
                       ivy-update-fns-alist
                       ivy-unwind-fns-alist
                       ivy-init-fns-alist
                       ivy-index-functions-alist
                       ivy-sort-functions-alist
                       ivy-sort-matches-functions-alist
                       ivy-format-functions-alist
                       ivy-display-functions-alist
                       ivy--display-transformers-alist
                       ivy-alt-done-functions-alist
                       ivy-more-chars-alist))
    (ivy--alist-set alist-sym 'ivy-fd--read-multi nil))
  (when (and (boundp 'counsel--async-exit-code-plist)
             (plist-get counsel--async-exit-code-plist
                        'ivy-fd--read-multi))
    (setq counsel--async-exit-code-plist
          (ivy-fd--plist-omit counsel--async-exit-code-plist
                              '(ivy-fd--read-multi))))
  (let ((marked)
        (persistent-action (plist-get ivy-args :persistent-action))
        (premarked-candidates (plist-get ivy-args :premarked)))
    (let ((args (append
                 (list prompt
                       collection
                       :caller 'ivy-fd--read-multi
                       :action (lambda (item)
                                 (when (and persistent-action
                                            (null ivy-exit))
                                   (funcall persistent-action item))
                                 item)
                       :multi-action (lambda (children)
                                       (setq marked children)))
                 (ivy-fd--plist-pick
                  (seq-difference ivy-fd--ivy-read-keywords
                                  '(:multi-action
                                    :action))
                  ivy-args)))
          (configure-args (ivy-fd--plist-pick
                           ivy-fd--configure-keywords
                           ivy-args))
          (item))
      (when configure-args
        (push 'ivy-fd--read-multi configure-args)
        (apply #'ivy-configure configure-args))
      (setq item (if premarked-candidates
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (when (active-minibuffer-window)
                             (ivy-fd--mark-candidates premarked-candidates)))
                       (apply #'ivy-read args))
                   (apply #'ivy-read args)))
      (or marked
          (when item (list item))))))

(defun ivy-fd--read-multiple (prompt collection &rest ivy-args)
  "Read COLLECTION with PROMPT and return list with selected candidates.


- :action
- :multi-action
- :caller

but accepting:

- :persistent-action
- :premarked

Persistent action will be called with current candidate without exiting
completion.

Premarked is candidates from COLLECTION which should be initially marked."
  (let ((marked)
        (persistent-action (plist-get ivy-args :persistent-action))
        (premarked-candidates (plist-get ivy-args :premarked)))
    (let ((caller (plist-get ivy-args :caller))
          (args (append
                 (list prompt
                       collection
                       :action (lambda (item)
                                 (when (and persistent-action
                                            (null ivy-exit))
                                   (funcall persistent-action item))
                                 item)
                       :multi-action (lambda (children)
                                       (setq marked children)))
                 (ivy-fd--plist-pick
                  (seq-difference ivy-fd--ivy-read-keywords
                                  '(:multi-action
                                    :action))
                  ivy-args)))
          (configure-args (ivy-fd--plist-pick
                           ivy-args
                           ivy-fd--configure-keywords))
          (item))
      (when (and configure-args caller)
        (push caller configure-args)
        (apply #'ivy-configure configure-args))
      (print args)
      (setq item (if premarked-candidates
                     (minibuffer-with-setup-hook
                         (lambda ()
                           (when (active-minibuffer-window)
                             (ivy-fd--mark-candidates premarked-candidates)))
                       (apply #'ivy-read args))
                   (apply #'ivy-read args)))
      (or marked
          (when item (list item))))))


(defmacro ivy-fd--pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro ivy-fd--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(ivy-fd--pipe ,@(reverse functions)))

(defmacro ivy-fd--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defun ivy-fd-keyword-to-option (keyword)
  "Convert KEYWORD to fd flag."
  (funcall (ivy-fd--compose (apply-partially #'concat "--")
                            (ivy-fd--rpartial substring 1)
                            symbol-name)
           keyword))

(defvar ivy-fd-settings-type
  `(plist :options
          (,@(mapcar
              (lambda (it)
                `(,it
                  (const :tag ,(ivy-fd-keyword-to-option it)
                         t)))
              ivy-fd-boolean-options)
           (:exclude
            (repeat (string :tag "Exclude")))
           (:extension (repeat (string :tag "Extensions")))
           (:max-depth (number :tag "Max depth" 1))
           (:type (plist :options
                         ((e (const :tag "empty" t))
                          (x (const :tag "executable" t))
                          (l (const :tag "symlink" t))
                          (f (const :tag "file" t))
                          (d (const :tag "directory" t)))))
           (:size (string :tag "Size"))
           (:changed-within
            (string :tag "--changed-within"))
           (:changed-before
            (string :tag "--changed-before")))))

(defcustom ivy-fd-per-directory-settings nil
  "Settings to apply in directories."
  :group 'ivy-fd
  :type `(alist
          :key-type (directory :tag "Directory")
          :value-type ,ivy-fd-settings-type))

(defun ivy-fd-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

(defun ivy-fd-slash (dir)
  "Add slash to DIR if none."
  (when dir
    (if (string-match-p "/$" dir)
        dir
      (setq dir (concat dir "/")))))

(defun ivy-fd-parent-dir (path)
  "Return the parent directory to PATH with slash."
  (when-let* ((path (ivy-fd-parent path)))
    (ivy-fd-slash path)))

(defvar ivy-fd-async-command nil)

(defun ivy-fd-count-matches-by-re (re str &optional start end)
  "Count occurrences of RE in STR.
START, inclusive, and END, exclusive, delimit the part of s to
match.  START and END are both indexed starting at 1; the initial
character in s is index 1."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (count-matches re (or start 1) (or end (point-max))))))

(defcustom ivy-fd-async-filter-update-time 500000
  "The amount of microseconds to wait until updating `ivy-fd--async-filter'."
  :type 'integer
  :group 'ivy-fd)

(defcustom ivy-fd-async-command-delay 0
  "Number of seconds to wait before spawning another async command."
  :type 'number
  :group 'ivy-fd)

(defvar ivy-fd--async-timer nil
  "Timer used to dispose `ivy-fd--async-command.")

(defvar ivy-fd-async-ignore-re-alist nil
  "An alist of regexp matching candidates to ignore in `ivy-fd--async-filter'.")

(defvar ivy-fd--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar ivy-fd--async-exit-code-plist ()
  "Associate commands with their exit code descriptions.
This plist maps commands to a plist mapping their exit codes to
descriptions.")

(defvar ivy-fd--async-start nil
  "Store the time when a new process was started.")

(defvar ivy-fd--async-duration nil
  "Store the time a process takes to gather all its candidates.
The time is measured in seconds.")

(defvar ivy-fd--async-last-error-string nil
  "When the process returned non-0, store the output here.")

(defvar ivy-fd--async-last-command nil
  "Store the last command ran by `counsel--async-command-1'.")

(defun ivy-fd--sync-sentinel-on-exit (process)
  "Synchronize PROCESS sentinel."
  (if (zerop (process-exit-status process))
      (let ((cur (ivy-state-current ivy-last)))
        (ivy--set-candidates
         (ivy--sort-maybe
          (with-current-buffer (process-buffer process)
            (ivy-fd--split-string))))
        (when ivy-fd--async-start
          (setq ivy-fd--async-duration
                (time-to-seconds (time-since ivy-fd--async-start))))
        (let ((re (ivy-re-to-str ivy-regex)))
          (if ivy--old-cands
              (if (eq (ivy-alist-setting ivy-index-functions-alist)
                      'ivy-recompute-index-zero)
                  (ivy-set-index 0)
                (ivy--recompute-index re ivy--all-candidates))
            (unless (string= cur (nth ivy--index ivy--all-candidates))
              (let ((func (ivy-alist-setting ivy-index-functions-alist)))
                (if func
                    (funcall func re ivy--all-candidates)
                  (ivy--preselect-index
                   (if (> (length re) 0)
                       cur
                     (ivy-state-preselect ivy-last))
                   ivy--all-candidates))))))
        (setq ivy--old-cands ivy--all-candidates)
        (if ivy--all-candidates
            (ivy--exhibit)
          (ivy--insert-minibuffer "")))
    (setq ivy-fd--async-last-error-string
          (with-current-buffer (process-buffer process) (buffer-string)))
    (setq ivy--all-candidates
          (let ((status (process-exit-status process))
                (plist (plist-get ivy-fd--async-exit-code-plist
                                  (ivy-state-caller ivy-last))))
            (list (or (plist-get plist status)
                      (format "error code %d" status)))))
    (setq ivy--old-cands ivy--all-candidates)
    (ivy--exhibit)))

(defun ivy-fd--split-string (&optional str)
  "Split STR or buffer string."
  (split-string
   (or str (buffer-string))
   "\0"
   ;; (ivy-alist-setting ivy-fd-async-split-string-re-alist)
   t))

(defun ivy-fd-delete-process (&optional name)
  "Delete current `ivy-fd' process or that with NAME."
  (let ((process (get-process (or name " *ivy-fd*"))))
    (when process
      (delete-process process))))

(defun ivy-fd--async-sentinel (process _msg)
  "Sentinel function for an asynchronous `ivy-fd' PROCESS."
  (when (eq (process-status process) 'exit)
    (ivy-fd--sync-sentinel-on-exit process)))

(defun ivy-fd--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`ivy-fd-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (list 0 0 ivy-fd-async-filter-update-time)
                     (time-since ivy-fd--async-time))
    (let (numlines)
      (with-current-buffer (process-buffer process)
        (setq numlines (count-lines (point-min) (point-max)))
        (ivy--set-candidates
         (let ((lines (ivy-fd--split-string))
               (ignore-re (ivy-alist-setting ivy-fd-async-ignore-re-alist)))
           (if (stringp ignore-re)
               (seq-remove (lambda (line)
                             (string-match-p ignore-re line))
                           lines)
             lines))))
      (let ((ivy--prompt (format "%d++ %s" numlines
                                 (ivy-state-prompt ivy-last))))
        (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))
      (setq ivy-fd--async-time (current-time)))))

(defun ivy-fd--async-command-1 (cmd &optional sentinel filter name)
  "Start and return new `ivy-fd' process by calling CMD.
CMD can be either a shell command as a string, or a list of the
program name to be called directly, followed by its arguments.
If the default `ivy-fd' process or one with NAME already exists,
kill it and its associated buffer before starting a new one.
Give the process the functions SENTINEL and FILTER, which default
to `ivy-fd--async-sentinel' and `ivy-fd--async-filter',
respectively."
  (ivy-fd-delete-process name)
  (setq name (or name " *ivy-fd*"))
  (when (get-buffer name)
    (kill-buffer name))
  (setq ivy-fd--async-last-command cmd)
  (let* ((buf (get-buffer-create name))
         (proc (if (listp cmd)
                   (apply #'start-file-process name buf cmd)
                 (start-file-process-shell-command name buf cmd))))
    (setq ivy-fd--async-time (current-time))
    (setq ivy-fd--async-start ivy-fd--async-time)
    (set-process-sentinel proc (or sentinel #'ivy-fd--async-sentinel))
    (set-process-filter proc (or filter #'ivy-fd--async-filter))
    proc))

(defun ivy-fd--elisp-to-pcre (regex)
  "Convert REGEX from Elisp format to PCRE format, on best-effort basis.
REGEX may be of any format returned by an Ivy regex function,
namely a string or a list.  The return value is always a string.

Note that incorrect results may be returned for sufficiently
complex regexes."
  (if (consp regex)
      (mapconcat
       (lambda (pair)
         (let ((subexp (ivy-fd--elisp-to-pcre (car pair))))
           (if (string-match-p "|" subexp)
               (format "(?:%s)" subexp)
             subexp)))
       (seq-filter #'cdr regex)
       ".*")
    (replace-regexp-in-string
     "\\\\[(){}|`']\\|[()]"
     (lambda (s)
       (or (cdr (assoc s '(("\\(" . "(")
                           ("\\)" . ")")
                           ("(" . "\\(")
                           (")" . "\\)")
                           ("\\{" . "{")
                           ("\\}" . "}")
                           ("\\|" . "|")
                           ("\\`" . "^")
                           ("\\'" . "$"))))
           (error
            "Unexpected error in `ivy-fd--elisp-to-pcre' (got match %S)" s)))
     regex t t)))

(defun ivy-fd--async-command (&rest args)
  "Like `ivy-fd--async-command-1', with same ARGS, but debounced.
Calls to `ivy-fd--async-command-1' are separated by at least
`ivy-fd-async-command-delay' seconds, so as to avoid issues
caused by spawning too many subprocesses too quickly."
  (if (zerop ivy-fd-async-command-delay)
      (apply #'ivy-fd--async-command-1 args)
    (when ivy-fd--async-timer
      (cancel-timer ivy-fd--async-timer))
    (setq ivy-fd--async-timer
          (apply #'run-with-timer
                 ivy-fd-async-command-delay
                 nil
                 #'ivy-fd--async-command-1
                 args))))

(defun ivy-fd-async-cmd (input)
  "Return a `mdfind' shell command based on INPUT."
  (let* ((regex (shell-quote-argument (ivy-fd--elisp-to-pcre
                                       (ivy--regex input))))
         (cmd (apply #'format ivy-fd-async-command
                     (append
                      (make-vector
                       (ivy-fd-count-matches-by-re "%s"
                                                   ivy-fd-async-command)
                       regex)
                      nil))))
    cmd))

(defvar ivy-fd-async-history nil
  "History for `ivy-fd-async'.")

(defun ivy-fd-async-function (input &rest _)
  "Call a \"locate\" style shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (ivy-fd--async-command
      (funcall #'ivy-fd-async-cmd input))
     '("" "working..."))))

;;;###autoload
(defun ivy-fd-preview-file (file)
  "Momentarily display content of the FILE in popup window.

Display remains until next event is input."
  (interactive "f")
  (if (file-directory-p file)
      (ivy-fd-visit-dir file)
    (when-let* ((filename (and
                          file
                          (file-readable-p file)
                          (file-exists-p file)
                          file))
               (buffer (get-buffer-create
                        "*ivy-fd-preview*")))
      (with-current-buffer buffer
        (with-current-buffer-window
            buffer
            (cons 'display-buffer-in-direction
                  '((window-height . fit-window-to-buffer)))
            (lambda (window _value)
              (with-selected-window window
                (setq buffer-read-only t)
                (let ((inhibit-read-only t))
                  (unwind-protect
                      (read-key-sequence "")
                    (quit-restore-window window 'kill)
                    (setq unread-command-events
                          (append (this-single-command-raw-keys)
                                  unread-command-events))))))
          (insert-file-contents filename)
          (let ((buffer-file-name filename))
            (delay-mode-hooks (set-auto-mode)
                              (font-lock-ensure)))
          (setq header-line-format
                (abbreviate-file-name filename)))))))

(defvar ivy-fd-current-dir nil)
(defvar ivy-fd-last-input nil)
(defvar ivy-fd-args nil)

(defvar ivy-fd-hydra-state '(:hidden nil :no-ignore nil))

(defun ivy-fd-hydra-get (keyword)
  "Get value of KEYWORD from `fd-hydra-state'."
  (plist-get ivy-fd-hydra-state keyword))

(defun ivy-fd-hydra-put (keyword value)
  "Put KEYWORD with VALUE to `fd-hydra-state'."
  (setq ivy-fd-hydra-state
        (plist-put ivy-fd-hydra-state keyword value)))

(defun ivy-fd-maybe-to-number (value)
  "Convert string of VALUE to number or return VALUE.
If value is empty string, return nil."
  (cond ((stringp value)
         (setq value (string-trim value))
         (if (string-empty-p value)
             nil
           (string-to-number value)))
        (t value)))


(defun ivy-fd-fdfind-read-max-depth ()
  "Read and set max-depth option."
  (let* ((value (ivy-fd-hydra-get :max-depth))
         (new-value (read-string "--max-depth " (when (numberp value)
                                                  (number-to-string value)))))
    (ivy-fd-hydra-put :max-depth (ivy-fd-maybe-to-number new-value))))

(defun ivy-fd-read-date (&optional prompt &rest _)
  "Read fdfind date options with PROMPT if provided."
  (let* ((actions '((?h "hours" "%dh")
                    (?m "minutes" "%dmin")
                    (?w "weeks" "%dweeks")
                    (?d "days" "%dd")
                    (?t "time")
                    (?o "other")
                    (?n "none")))
         (answer (read-multiple-choice (or prompt "Date type: ") actions)))
    (if (nth 2 answer)
        (format (nth 2 answer)
                (read-number (format "%s " (nth 1 answer))))
      (pcase (car answer)
        (?t (read-string (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (current-time))))
        (?o (read-string "Value: "))))))

(defun ivy-fd-read-size (&optional prompt &rest _)
  "Read fdfind size options with PROMPT."
  (let* ((actions '((?b "bytes" "b")
                    (?k "kilobytes" "k")
                    (?m "megabytes" "m")
                    (?g "gigabytes" "g")
                    (?t "terabytes" "t")
                    (?K "kibibytes" "ki")
                    (?G "gibibytes" "gi")
                    (?T "tebibytes" "ti")
                    (?n "none")))
         (answer (read-multiple-choice (or prompt "Type: ") actions)))
    (when (nth 2 answer)
      (let* ((value (concat (format "%d"
                                    (read-number (format "%s "
                                                         (nth 1 answer))))
                            (nth 2 answer)))
             (prefix (nth 2 (read-multiple-choice
                             ""
                             `((?g ,(format "greater then %s" value) "+")
                               (?l ,(format "less then %s" value) "-")
                               (?e ,(format "equal then %s" value) ""))))))
        (concat prefix value)))))

(defun ivy-fd--concat-args (args)
  "Concatenate ARGS into a single string separated by spaces.

Argument ARGS is a list of strings to be concatenated."
  (mapconcat
   (apply-partially #'format "%s")
   args " "))


(defun ivy-fd-toggle (keyword)
  "Toggle value of KEYWORD."
  (ivy-fd-hydra-put keyword (not (ivy-fd-hydra-get keyword))))

(defun ivy-fd--plist-keys (plist)
  "Return the keys in PLIST."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    keys))

(defun ivy-fd-plist-merge (plist-a plist-b)
  "Add props from PLIST-B to PLIST-A."
  (dotimes (idx (length plist-b))
    (when (eq (logand idx 1) 0)
      (let ((prop-name (nth idx plist-b)))
        (let ((val (plist-get plist-b prop-name)))
          (plist-put plist-a prop-name val)))))
  plist-a)

(defun ivy-fd--plist-pick (keywords pl)
  "Pick KEYWORDS props from PL."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let* ((value (plist-get pl keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

(defun ivy-fd-get-dir-settings (directory)
  "Return settings for DIRECTORY from `ivy-fd-per-directory-settings'."
  (let* ((settings (seq-copy
                    (or (cdr
                         (assoc directory ivy-fd-per-directory-settings))
                        (cdr
                         (assoc (abbreviate-file-name directory)
                                ivy-fd-per-directory-settings)))))
         (type (plist-get settings :type)))
    (if type
        (append (mapcan
                 (lambda (it) (list (intern (concat ":type." (symbol-name it))) t))
                 (ivy-fd--plist-keys type))
                (ivy-fd--plist-omit settings :type))
      settings)))

(defun ivy-fd-set-directory-settings (directory)
  "Set `ivy-fd-hydra-state' to DIRECTORY settings."
  (setq ivy-fd-hydra-state
        (ivy-fd-get-dir-settings directory)))

(defun ivy-fd-merge-directory-settings (directory)
  "Merge `ivy-fd-hydra-state' with DIRECTORY settings."
  (setq ivy-fd-hydra-state
        (ivy-fd-plist-merge ivy-fd-hydra-state
                            (ivy-fd-get-dir-settings directory))))




(defun ivy-fd--stringify (item)
  "Convert various data types to string representation.

Argument ITEM is the object to be stringified; it can be a string, number,
vector, list, cons cell, symbol, or any other type."
  (pcase item
    ((pred not)
     item)
    ((pred stringp)
     (substring-no-properties item))
    ((pred numberp)
     (number-to-string item))
    ((pred vectorp)
     (apply #'vector (mapcar #'ivy-fd--stringify (append item nil))))
    ((pred proper-list-p)
     (mapcar #'ivy-fd--stringify item))
    ((guard (and (consp item)
                 (atom (cdr item))))
     (cons (ivy-fd--stringify (car item))
           (ivy-fd--stringify (cdr item))))
    ((guard (and item (symbolp item)))
     (substring-no-properties (symbol-name item)))
    (_ item)))

(defun ivy-fd--transient-args ()
  "Retrieve and process arguments for transient command."
  (let ((raw-args))
    (cond (transient-current-command
           (setq raw-args (transient-args transient-current-command)))
          (transient--prefix
           (setq transient-current-prefix transient--prefix)
           (setq transient-current-command (oref transient--prefix command))
           (setq transient-current-suffixes transient--suffixes)
           (setq raw-args (transient-args transient-current-command))))
    (ivy-fd--stringify raw-args)))



(transient-define-suffix ivy-fd-show-args ()
  "Display formatted npm command with arguments."
  :transient t
  :description "Show arguments"
  (interactive)
  (let ((args
         (ivy-fd--transient-args)))
    (message "fdfind %s"
             args)))

(transient-define-suffix ivy-fd-vterm ()
  "Execute fdfind with transient arguments."
  :transient t
  :description (lambda ()
                 (let* ((args
                         (ivy-fd--transient-args))
                        (cmd (concat ivy-fd-exec-path " "
                                     (mapconcat (apply-partially
                                                 #'format "%s")
                                                args " "))))
                   (concat "Run in vterm: " (propertize cmd 'face
                                                        'transient-value))))
  (interactive)
  (require 'vterm nil t)
  (let* ((args
          (ivy-fd--transient-args))
         (cmd (concat ivy-fd-exec-path " "
                      (mapconcat
                       (apply-partially #'format "%s")
                       args " ")))
         (buffer (format "*%s*"
                         (string-join
                          (delete nil
                                  (list "vterm"
                                        (car
                                         (split-string cmd nil t))
                                        (or (vc-root-dir)
                                            default-directory)))
                          "-")))
         (live-p (buffer-live-p (get-buffer buffer))))
    (when live-p
      (switch-to-buffer (get-buffer buffer))
      (when (fboundp 'vterm--invalidate)
        (vterm--invalidate))
      (kill-buffer (get-buffer buffer)))
    (let ((default-directory (or (vc-root-dir) default-directory)))
      (when (fboundp 'vterm)
        (vterm buffer)))
    (when (fboundp 'vterm-send-string)
      (run-at-time
       0.5 nil #'vterm-send-string cmd))))

(transient-define-suffix ivy-fd-run ()
  "Execute fdfind."
  :transient t
  :description "Read"
  (interactive)
  (let* ((args
          (ivy-fd--transient-args))
         (dir (or (ivy-fd--get-arg "--base-directory=" args)
                  default-directory)))
    (ivy-fd-async dir ivy-fd-last-input
                  (ivy-fd--remove-arg "--base-directory="
                                      args))))




(defvar ivy-fd--file-type-descriptions
  '(("file" . "Regular files")
    ("directory" . "Directories")
    ("symlink" . "Symbolic links")
    ("socket" . "Socket")
    ("pipe" . "Named pipe (FIFO)")
    ("executable" . "Executables")
    ("empty" . "Empty files or directories")))

(defun ivy-fd--file-type-reader (&optional prompt initial-input history)
  "Read a file type with completion, using optional PROMPT and INITIAL-INPUT.

Optional argument PROMPT is a string used to prompt the user for input,
defaulting to \"File type: \".

Optional argument INITIAL-INPUT is the initial input in the minibuffer,
defaulting to nil.

Optional argument HISTORY is the history list to use for the input,
defaulting to nil."
  (let* ((alist ivy-fd--file-type-descriptions)
         (longest
          (propertize " " 'display
                      (list 'space :align-to
                            (apply #'max
                                   (or
                                    (mapcar
                                     (pcase-lambda (`(,k . ,_v))
                                       (length k))
                                     alist)
                                    (list 10))))))
         (annotf (lambda (file-type)
                   (concat
                    longest
                    (substring-no-properties
                     (cdr
                      (assoc-string
                       (if
                           (and ivy-mark-prefix
                                (string-prefix-p
                                 ivy-mark-prefix
                                 file-type))
                           (substring-no-properties
                            file-type
                            (length
                             ivy-mark-prefix))
                         file-type)
                       ivy-fd--file-type-descriptions))))))
         (strs (mapcar #'car alist)))
    (ivy-fd--read-multiple (or prompt
                               "File type: ")
                           (lambda (str pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   (annotation-function . ,annotf))
                               (complete-with-action action strs str pred)))
                           :initial-input initial-input
                           :history history)))


(defun ivy-fd--read-dir (&optional prompt initial-input &rest _)
  "Read a directory name with optional PROMPT and INITIAL-INPUT.

Optional argument PROMPT is a string used to prompt the user.

Optional argument INITIAL-INPUT is the initial input in the minibuffer.

Remaining arguments _ are ignored and not used in the function."
  (read-directory-name prompt nil nil t initial-input))

(defun ivy-fd--get-arg (arg args)
  "Extract and return substring from ARGS starting with ARG.

Argument ARG is a string used to match the prefix of elements in ARGS.

Arguments ARGS is a list of strings to search through for a match with ARG."
  (when-let* ((value (seq-find (lambda (it)
                                 (and it
                                      (stringp it)
                                      (string-prefix-p arg it)))
                               args)))
    (substring-no-properties value
                             (length arg))))

(defun ivy-fd--remove-arg (arg args)
  "Return updated ARGS list with elements prefixed by ARG removed.

Argument ARG is a string used as a prefix to identify elements to remove.

Arguments ARGS is a list of strings from which elements prefixed by ARG are
removed."
  (seq-remove (lambda (it)
                (and it
                     (stringp it)
                     (string-prefix-p arg it)))
              args))

;;;###autoload (autoload 'ivy-fd-menu "ivy-fd" nil t)
(transient-define-prefix ivy-fd-menu ()
  "Menu for fdfind."
  :man-page ivy-fd-exec-path
  :value
  (lambda ()
    (append
     (remove nil ivy-fd-args)
     (list "--base-directory=" (or ivy-fd-current-dir default-directory))))
  ["FLAGS:"
   ("-H" "Search hidden files and directories" ("-H" "--hidden"))
   ("-I" "Do not respect .(git|fd)ignore files" ("-I" "--no-ignore"))
   ("-s" "A case-sensitive search" ("-s" "--case-sensitive"))
   ("-i" "A case-insensitive search" ("-i" "--ignore-case"))
   ("-g" "Glob-based search (default: regular expression)" ("-g" "--glob"))
   ("-a" "Show absolute instead of relative paths" ("-a" "--absolute-path"))
   ("-l" "Show absolute instead of relative paths" ("-l" "--list-details"))
   ("-L" "Follow symbolic links" ("-L" "--follow"))
   ("-p" "Search full abs. path" ("-p" "--full-path"))
   ("-F"
    "Treat the pattern as a literal string instead of a regular expression"
    "--fixed-strings")]
  ["OPTIONS:"
   ("-d" "Maximum search depth" ("-d=" "--max-depth=")
    :class transient-option
    :reader transient-read-number-N+)
   ("-D" "Minimum search depth" "--min-depth="
    :class transient-option
    :reader transient-read-number-N+)
   ("-t" "Filter by type" ("-t=" "--type=")
    :class transient-option
    :multi-value repeat
    :reader ivy-fd--file-type-reader)
   ("-e" "Filter by file extension" "-e="
    :class transient-option
    :multi-value repeat)
   ""
   ("-E" "Exclude entries that match the given glob pattern"
    ("-E=" "--exclude="
     :multi-value repeat)
    :class transient-option)
   ("-S" "Limit results based on the size of files" ("-S=" "--size=")
    :class transient-option
    :reader ivy-fd-read-size)
   ("n" "Newer then" "--changed-within="
    :class transient-option
    :reader ivy-fd-read-date)
   ("o" "Older then" "--changed-before="
    :class transient-option
    :reader ivy-fd-read-date)
   ("-o" "Filter by owning user and/or group" ("-o=" "--owner=") :class
    transient-option)
   ("c" "Limit the number of search results to 'count' and quit immediately."
    "--max-results="
    :class transient-option
    :reader transient-read-number-N+)
   ("d" "Directory" "--base-directory="
    :class transient-option
    :reader ivy-fd--read-dir)]
  [["Actions"
    ("-x" "Execute a command for each search result" ("-x=" "--exec=") :class
     transient-option)
    ("-X" "Execute a command with all search results at once" ("-X="
                                                               "--exec-batch=")
     :class transient-option)
    ("C-c C-a" ivy-fd-show-args)
    ("C-c v" ivy-fd-vterm)
    ("RET" ivy-fd-run)]])

;;;###autoload
(defun ivy-fd-read-flags ()
  "Invoke hydra to configure fd flags."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (setq ivy-fd-last-input ivy-text)
        (ivy-quit-and-run (ivy-fd-menu)))
    (ivy-fd-menu)))

;;;###autoload
(defun ivy-fd-find-directory-up ()
  "Change `ivy-fd-current-dir' to parent directory."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (funcall-interactively #'ivy-fd-async
                                      (ivy-fd-parent-dir ivy-fd-current-dir)
                                      ivy-fd-last-input
                                      ivy-fd-args)))
    (funcall-interactively #'ivy-fd-async
                           (ivy-fd-parent-dir default-directory))))

(defun ivy-fd-visit-dir (dir)
  "Change `ivy-fd-current-dir' to DIR."
  (when (and dir
             (file-directory-p dir)
             (file-exists-p dir)
             (file-readable-p dir))
    (progn (setq ivy-fd-last-input ivy-text)
           (ivy-quit-and-run
             (funcall-interactively #'ivy-fd-async dir
                                    ivy-fd-last-input)))))

(defun ivy-fd-expand-file (filename)
  "Expand not absolute FILENAME to `ivy-fd-current-dir'.
If FILENAME is absolute just return it."
  (if (file-name-absolute-p filename)
      filename
    (expand-file-name filename ivy-fd-current-dir)))

(defun ivy-fd-find-file-or-preview (file)
  "Find FILE if `ivy-exit', otherwise preview FILE."
  (setq file (ivy-fd-expand-file file))
  (if ivy-exit
      (progn
        (when (file-exists-p file)
          (find-file file)))
    (ivy-fd-preview-file file)))

(defun ivy-fd-find-file-other-window-action (file)
  "Find FILE in other window."
  (setq file (ivy-fd-expand-file file))
  (find-file-other-window file))


(defun ivy-fd-insert-filename ()
  "Insert FILE name and exit minibuffer."
  (interactive)
  (when-let* ((curr (ivy-state-current ivy-last))
              (variants
               (seq-uniq
                (list
                 curr
                 (replace-regexp-in-string "^./" "" curr)
                 (ivy-fd-expand-file curr)
                 (abbreviate-file-name (ivy-fd-expand-file curr))))))
    (ivy-quit-and-run (insert
                       (completing-read "Insert " variants)))))

(defun ivy-fd-copy-filename ()
  "Copy FILE name."
  (interactive)
  (when-let* ((curr (ivy-state-current ivy-last)))
    (kill-new (ivy-fd-expand-file curr))
    (message "Copied filename")))

;;;###autoload
(defun ivy-fd-find-file-other-window ()
  "Open a file in another window using Ivy for completion."
  (interactive)
  (ivy-exit-with-action #'ivy-fd-find-file-other-window-action))

;;;###autoload
(defun ivy-fd-change-dir ()
  "Read directory for fd in minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (let ((directory (read-directory-name "Search in:\s")))
                 (setq ivy-fd-hydra-state (ivy-fd-merge-directory-settings
                                           directory))
                 (funcall-interactively #'ivy-fd-async
                                        directory
                                        ivy-fd-last-input))))
    (let ((directory (read-directory-name "Search in:\s")))
      (setq ivy-fd-hydra-state (ivy-fd-set-directory-settings
                                directory))
      (funcall-interactively #'ivy-fd-async
                             directory))))

(defun ivy-fd--make-shell-command (args)
  "Concatenate shell command string using ARGS and `ivy-fd-exec-path'.

Argument ARGS is a list of strings to be concatenated into a single string
separated by spaces."
  (concat ivy-fd-exec-path " -0 --color=never " (ivy-fd--concat-args args)
          (concat " %s . ")))

;;;###autoload
(defun ivy-fd-toggle-hidden ()
  "Inside vc directory toggle --hidden flag, othervise :no-ignore-vcs."
  (interactive)
  (if (vc-root-dir)
      (ivy-fd-toggle :hidden)
    (ivy-fd-toggle :no-ignore-vcs))
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (funcall-interactively #'ivy-fd-async
                                      ivy-fd-current-dir
                                      ivy-fd-last-input)))
    (funcall-interactively #'ivy-fd-async
                           (read-directory-name "Search in:\s"))))

;;;###autoload
(defun ivy-fd-change-max-depth ()
  "Change --max-depth flag."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (ivy-fd-fdfind-read-max-depth)
               (funcall-interactively #'ivy-fd-async
                                      ivy-fd-current-dir
                                      ivy-fd-last-input)))
    (ivy-fd-fdfind-read-max-depth)
    (funcall-interactively #'ivy-fd-async)))

(defvar ivy-fd-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<backspace>") #'ivy-fd-find-directory-up)
    (define-key map (kbd "C-l") #'ivy-fd-find-directory-up)
    (define-key map (kbd "C-.")  #'ivy-fd-change-dir)
    (define-key map (kbd "C-q")  #'ivy-fd-read-flags)
    (define-key map (kbd "C-c C-o") #'ivy-fd-find-file-other-window)
    (define-key map (kbd "M-.") #'ivy-fd-toggle-hidden)
    (define-key map (kbd "M-<up>") #'ivy-fd-change-max-depth)
    (define-key map (kbd "C-c C-i") #'ivy-fd-insert-filename)
    (define-key map (kbd "M-w") #'ivy-fd-copy-filename)
    map))


(defun ivy-fd-resolve-project-root ()
  "Resolve project root by searching git directory."
  (locate-dominating-file default-directory ".git" ))

;;;###autoload
(defun ivy-fd-read-directory (&optional directory initial-input)
  "Search in DIRECTORY or `default-directory' with INITIAL-INPUT."
  (interactive)
  (let ((project (or directory (funcall ivy-fd-resolve-project-root-fn)
                     default-directory)))
    (unless directory
      (setq ivy-fd-hydra-state
            (ivy-fd-get-dir-settings project)))
    (funcall-interactively (or 'ivy-fd-async) project initial-input)))

;;;###autoload
(defun ivy-fd-async-project (&optional directory initial-input)
  "Search asynchronously in a project DIRECTORY with `fd'.

Optional argument DIRECTORY is the directory to search in. It defaults to the
project root or `default-directory'.

Optional argument INITIAL-INPUT is the initial input for the search."
  (interactive)
  (let ((project (or directory (funcall ivy-fd-resolve-project-root-fn)
                     default-directory)))
    (unless directory
      (setq ivy-fd-hydra-state
            (ivy-fd-get-dir-settings project)))
    (ivy-fd-hydra-put :type.f t)
    (funcall-interactively (or 'ivy-fd-async) project initial-input)))

;;;###autoload
(defun ivy-fd-async-from-source ()
  "Find directory from source settings `ivy-fd-per-directory-settings'."
  (interactive)
  (let ((dir (completing-read "Source: " ivy-fd-per-directory-settings)))
    (setq ivy-fd-hydra-state (ivy-fd-get-dir-settings dir))
    (ivy-fd-async dir)))

(defun ivy-fd--prompt (dir args)
  "Return a truncated prompt string with directory and formatted flags.

Argument DIR is a directory path used as a base for the prompt.

Argument ARGS is a list of strings representing command-line flags."
  (truncate-string-to-width
   (concat
    (when dir (abbreviate-file-name (or dir default-directory)))
    ": "
    (let ((flags (string-trim (ivy-fd--concat-args args))))
      (if (string-empty-p flags)
          ""
        (concat flags " "))))
   (window-width)
   nil nil t))


;;;###autoload
(defun ivy-fd-async (&optional directory initial-input args)
  "Search in DIRECTORY or `default-directory' with INITIAL-INPUT and ARGS."
  (interactive)
  (setq ivy-fd-current-dir (ivy-fd-slash
                            (expand-file-name
                             (or directory default-directory))))
  (setq ivy-fd-args (or args
                        (ivy-fd-get-dir-settings
                         ivy-fd-current-dir)
                        ivy-fd-args))
  (setq ivy-fd-async-command (ivy-fd--make-shell-command
                              ivy-fd-args))
  (unwind-protect
      (let ((default-directory ivy-fd-current-dir))
        (ivy-read (ivy-fd--prompt (or ivy-fd-current-dir default-directory)
                                  ivy-fd-args)
                  #'ivy-fd-async-function
                  :initial-input initial-input
                  :dynamic-collection t
                  :history 'ivy-fd-async-history
                  :keymap ivy-fd-map
                  :action 'ivy-fd-find-file-or-preview
                  :caller 'ivy-fd-async))
    (ivy-fd-delete-process)))

;; ;;;###autoload
;; (defun ivy-fd-async-read-directory (&optional directory initial-input args)
;;   "Search files asynchronously in a DIRECTORY with `fd' and Ivy.

;; Optional argument DIRECTORY is the directory to read from. It defaults to
;; `default-directory'.

;; Optional argument INITIAL-INPUT is the initial input for the search. It defaults
;; to nil.

;; Optional argument ARGS is additional arguments for the search. It defaults to
;; nil."
;;   (interactive)
;;   (setq ivy-fd-current-dir (ivy-fd-slash
;;                             (expand-file-name
;;                              (or directory default-directory))))
;;   (ivy-fd-hydra-put :type.d t)
;;   (ivy-fd-hydra-put :type.f nil)
;;   (unless directory
;;     (setq ivy-fd-hydra-state (or args
;;                                  (ivy-fd-get-dir-settings ivy-fd-current-dir))))
;;   (setq ivy-fd-async-command (ivy-fd--make-shell-command ivy-fd-args))
;;   (unwind-protect
;;       (let ((default-directory ivy-fd-current-dir)
;;             (prompt (truncate-string-to-width
;;                      (concat
;;                       (abbreviate-file-name ivy-fd-current-dir)
;;                       ": "
;;                       (let ((flags (string-trim (ivy-fd--concat-args ))))
;;                         (if (string-empty-p flags)
;;                             ""
;;                           (concat flags " "))))
;;                      (window-width))))
;;         (ivy-read prompt
;;                   #'ivy-fd-async-function
;;                   :initial-input initial-input
;;                   :dynamic-collection t
;;                   :history 'ivy-fd-async-history
;;                   :keymap ivy-fd-map
;;                   :action 'ivy-fd-find-file-or-preview
;;                   :caller 'ivy-fd-async))
;;     (ivy-fd-delete-process)))

(defun ivy-fd-format-time-readable (time)
  "Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-secs (-
                    (float-time (current-time))
                    (float-time time))))
    (pcase-let ((`(,format-str . ,value)
                 (cond ((< diff-secs 60)
                        (cons "%d second" (truncate diff-secs)))
                       ((< diff-secs 3600)
                        (cons "%d minute" (truncate (/ diff-secs 60))))
                       ((< diff-secs 86400)
                        (cons "%d hour" (truncate (/ diff-secs 3600))))
                       ((< diff-secs 2592000)
                        (cons "%d day" (truncate (/ diff-secs 86400))))
                       (t
                        (cons "%d month" (truncate (/ diff-secs 2592000)))))))
      (format (concat format-str (if (= value 1) " ago" "s ago")) value))))

(defun ivy-fd--read-file-display-transformer (str)
  "Transform filename STR when reading files."
  (let ((filename (if (file-name-absolute-p str)
                      str
                    (expand-file-name str ivy-fd-current-dir))))
    (let ((parts (delete nil `(,str ,(file-symlink-p filename))))
          (mod-time
           (ivy-fd-format-time-readable
            (file-attribute-modification-time
             (file-attributes (if (file-directory-p
                                   filename)
                                  (file-name-as-directory
                                   filename)
                                filename)))))
          (face
           (cond ((not (file-readable-p filename)) 'ivy-match-required-face)
                 ((file-accessible-directory-p filename) 'ivy-subdir)
                 ((and
                   (file-regular-p filename)
                   (file-executable-p filename))
                  'compilation-info)
                 (t nil)))
          result)
      (when face (setcar parts (propertize (car parts) 'face face)))
      (setq result (string-join parts " => "))
      (if mod-time
          (concat result
                  (propertize " " 'display
                              (list 'space :align-to
                                    120))
                  mod-time)
        result))))

(ivy-configure 'ivy-fd-async
    :display-transformer-fn #'ivy-fd--read-file-display-transformer)

(ivy-add-actions 'ivy-fd-async
                 '(("j" ivy-fd-find-file-other-window-action
                    "find file in other window")))

(provide 'ivy-fd)
;;; ivy-fd.el ends here
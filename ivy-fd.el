;;; ivy-fd.el --- Ivy interface for fd -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ivy-fd
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "27.1"))

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
(require 'hydra)

(defcustom ivy-fd-multi-command-flags '("--changed-within 1d"
                                        "--changed-before 1d")
  "Flags to pass on every search."
  :type '(repeat string)
  :group 'ivy-fd)

(defcustom ivy-fd-exec-path (executable-find "fdfind")
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

(defmacro ivy-fd--or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (or
                 ,@(mapcar (lambda (v) (if (symbolp v)
                                           `(,v it)
                                         `(funcall ,v it)))
                           functions))))

(defmacro ivy-fd--and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (it) (and
            ,@(mapcar (lambda (v) (if (symbolp v)
                                 `(,v it)
                               `(funcall ,v it)))
                      functions))))

(defmacro ivy-fd--partial (fn &rest args)
  "Return a partial application of FN to left-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append (list ,@args) pre-args))
                   `(apply ,fn (append (list ,@args) pre-args)))))))

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

(defcustom ivy-fd-settings-type-switchers nil
  "Dynamic settings."
  :group 'ivy-fd
  :type `(alist
          :key-type (directory :tag "Name")
          :value-type (repeat ,ivy-fd-settings-type)))

(defcustom ivy-fd-root-flags  '("-E 'run'"
                                   "-E" "'jest_rs'"
                                   "-E" "'home'"
                                   "--follow")
  "Flags to pass fdind when searching in root directory."
  :type '(repeat string)
  :group 'ivy-fd)

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
  (when-let ((path (ivy-fd-parent path)))
    (ivy-fd-slash path)))

(defun ivy-fd-generic-list-to-string (&rest flags)
  "Flattenize and join FLAGS using spaces."
  (setq flags (delete nil (flatten-list flags)))
  (when flags
    (string-join flags "\s")))

(defun ivy-fd-map-ignored (ignored)
  "Generate fd flag to exclude IGNORED from search."
  (let ((re "^\\(-E\\|--exclude\\)"))
    (if (stringp ignored)
        (if (string-match-p re ignored)
            ignored
          (concat "-E " ignored))
      (if (and ignored (listp ignored))
          (ivy-fd-generic-list-to-string
           (mapcar (lambda (it) (if (and (stringp it)
                                    (not (string-match-p re it)))
                               (concat "-E " it)
                             it))
                   ignored))))))

(defun ivy-fd-make-sortable-pre-part (&optional dir flags ignores)
  "Return string with fdfind command to search in DIR.

FLAGS and IGNORES should be string or list or alist.

IGNORES may omit --exclude flag."
  (setq flags (ivy-fd-generic-list-to-string flags))
  (setq ignores (ivy-fd-generic-list-to-string
                 (ivy-fd-map-ignored ignores)))
  (let* ((dir-flag (when dir (ivy-fd-generic-list-to-string "." dir)))
         (parts (mapcar
                 (ivy-fd--compose
                  (ivy-fd--rpartial concat ";")
                  'ivy-fd-generic-list-to-string
                  (apply-partially #'append '("fdfind" "-0" "--color=never"))
                  (ivy-fd--rpartial append (list ignores flags dir-flag))
                  'list)
                 (if (equal dir "/")
                     (list (ivy-fd-generic-list-to-string
                            ivy-fd-root-flags))
                   ivy-fd-multi-command-flags))))
    (ivy-fd-generic-list-to-string parts)))

;;;###autoload
(defun ivy-fd-make-sortable-command (place &optional common-flags ignores)
  "Return string with fdfind command to search in PLACE.
PLACE can be a string of directory, list of directories,or alist of directories
with extra flags.

COMMON-FLAGS and IGNORES should be string or list or alist.

IGNORES may omit --exclude flag."
  (if (stringp place)
      (ivy-fd-make-sortable-pre-part place common-flags ignores)
    (mapconcat (lambda (it)
                 (if (stringp it)
                     (ivy-fd-make-sortable-pre-part
                      it
                      common-flags
                      ignores)
                   (let ((dir (car it))
                         (flags (cdr it)))
                     (ivy-fd-make-sortable-pre-part
                      dir
                      (append (if (listp flags)
                                  flags
                                (list flags))
                              common-flags)
                      ignores))))
               place
               "\s")))

;;;###autoload
(defun ivy-fd-make-sortable-tr-command (place &optional common-flags ignores)
  "Return combined `fdfind' and `tr' command to search in PLACE.

PLACE can be a string of directory, list of directories,or alist of directories
 with extra flags.

COMMON-FLAGS and IGNORES should be string or list or alist.
IGNORES may omit --exclude flag."
  (let ((command (ivy-fd-make-sortable-command place common-flags ignores)))
    (string-join (list "{" command "}" "|"  "tr '\n' ' '") "\s")))

;;;###autoload
(defun ivy-fd-multi-dir (place &optional flags ignored)
  "Search multiple directories using `fdfind' and format output.

Argument PLACE is a string of directory, list of directories, or alist of
directories with extra flags.

Optional argument FLAGS is a string, list, or alist specifying additional flags
for the search command.

Optional argument IGNORED is a string, list, or alist specifying patterns to
ignore, which may omit the --exclude flag."
  (split-string (shell-command-to-string
                 (ivy-fd-make-sortable-tr-command place flags ignored))
                "\0" t))

(defun ivy-fd-make-command (&optional dir flags)
  "Generate string with fd command in DIR from FLAGS."
  (ivy-fd-generic-list-to-string
   "fdfind" "--color=never"
   flags
   (ivy-fd-generic-list-to-string "." dir)))

(defun ivy-fd-find (place &optional flags ignored)
  "Search files using `fdfind' and format output for Ivy completion.

Argument PLACE is a string representing the directory to search in, a list of
directories, or an alist of directories with extra flags.

Optional argument FLAGS is a string, list, or alist specifying additional flags
for the search command.

Optional argument IGNORED is a string, list, or alist specifying patterns to
ignore, which may omit the --exclude flag."
  (split-string (shell-command-to-string
                 (ivy-fd-make-sortable-tr-command place flags ignored))
                "\0" t))

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

(defvar ivy-fd-async-split-string-re-alist '((t . "[\r\n]"))
  "Store the regexp for splitting shell command output.")

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
    (when-let ((filename (and
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

(defvar ivy-fd-hydra-state '(:hidden nil :no-ignore nil))

(defun ivy-fd-hydra-get (keyword)
  "Get value of KEYWORD from `fd-hydra-state'."
  (plist-get ivy-fd-hydra-state keyword))

(defun ivy-fd-hydra-get-non-empty (keyword)
  "Return non-empty string value for KEYWORD from `ivy-fd-hydra-state' if exists.

Argument KEYWORD is a symbol used to retrieve a value from `ivy-fd-hydra-state'
plist."
  (when-let ((value (plist-get ivy-fd-hydra-state keyword)))
    (when (and (stringp value)
               (not (string-empty-p value)))
      value)))

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

(defun ivy-fd-increase-depth ()
  "Increase the search depth by 1 in `ivy-fd'."
  (let ((depth (or (ivy-fd-maybe-to-number
                    (ivy-fd-hydra-get :max-depth))
                   1)))
    (when (numberp depth)
      (ivy-fd-hydra-put :max-depth (1+ depth)))))

(defun ivy-fd-decrease-depth ()
  "Decrease the search depth by one if it's greater than one."
  (when-let ((depth (ivy-fd-maybe-to-number
                     (ivy-fd-hydra-get :max-depth))))
    (when (and (numberp depth)
               (> depth 1))
      (ivy-fd-hydra-put :max-depth
                        (1- depth)))))

(defun ivy-fd-fdfind-read-max-depth ()
  "Read and set max-depth option."
  (let* ((value (ivy-fd-hydra-get :max-depth))
         (new-value (read-string "--max-depth " (when (numberp value)
                                                  (number-to-string value)))))
    (ivy-fd-hydra-put :max-depth (ivy-fd-maybe-to-number new-value))))

(defun ivy-fd-read-date ()
  "Read fdfind date options."
  (let* ((actions '((?h "hours" "%dh")
                    (?m "minutes" "%dmin")
                    (?w "weeks" "%dweeks")
                    (?d "days" "%dd")
                    (?t "time")
                    (?o "other")
                    (?n "none")))
         (answer (read-multiple-choice "Type: " actions)))
    (if (nth 2 answer)
        (format (nth 2 answer) (read-number (format "%s " (nth 1 answer))))
      (pcase (car answer)
        (?t (read-string (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (current-time))))
        (?o (read-string "Value: "))))))

(defun ivy-fd-read-size ()
  "Read fdfind size options."
  (let* ((actions '((?b "bytes" "b")
                    (?k "kilobytes" "k")
                    (?m "megabytes" "m")
                    (?g "gigabytes" "g")
                    (?t "terabytes" "t")
                    (?K "kibibytes" "ki")
                    (?G "gibibytes" "gi")
                    (?T "tebibytes" "ti")
                    (?n "none")))
         (answer (read-multiple-choice "Type: " actions)))
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

(defvar ivy-fd-multi-options '(:exclude :extension))
(defun ivy-fd-read-multy-options (keyword)
  "Read multiple options for KEYWORD."
  (let* ((name (ivy-fd-keyword-to-option keyword))
         (extensions
          (split-string
           (string-trim (read-string
                         (format "%s (empty to unset) " name)
                         (string-join (ivy-fd-hydra-get keyword) "\s")))
           "\s")))
    (ivy-fd-hydra-put keyword extensions)))

(defun ivy-fd-normalize-multi-option (keyword)
  "Return string with KEYWORD option."
  (when-let ((value (ivy-fd-hydra-get keyword)))
    (mapconcat
     (lambda (v) (concat
             (ivy-fd-keyword-to-option
              keyword)
             " " v))
     value "\s")))

(defun ivy-fd-normalize-multi-options ()
  "Return options for exclude and extensions."
  (string-join (mapcar #'ivy-fd-normalize-multi-option
                       ivy-fd-multi-options)
               "\s"))

(defun ivy-fd-toggle-file-type ()
  "Toggle file and directory flags."
  (let ((value (not (ivy-fd-hydra-get :type.f))))
    (ivy-fd-hydra-put :type.f value)
    (ivy-fd-hydra-put :type.d (when value nil))))

(defun ivy-fd-normalize-type-options ()
  "Return string with --type options."
  (let ((types (mapcar
                (lambda (k) (when-let ((value (ivy-fd-hydra-get
                                          (intern (concat ":type." k)))))
                         (concat "--type " k)))
                '("f" "d" "l" "e" "x"))))
    (string-join (delq nil types) "\s")))

(defun ivy-fd-map-boolean-options ()
  "Return string with active flags from `ivy-fd-boolean-options'."
  (string-join
   (mapcar
    #'ivy-fd-keyword-to-option
    (seq-filter (apply-partially #'ivy-fd-hydra-get)
                ivy-fd-boolean-options))
   "\s"))

(defun ivy-fd-toggle-dir-type ()
  "Toggle directory and file flags."
  (let ((value (not (ivy-fd-hydra-get :type.d))))
    (ivy-fd-hydra-put :type.d value)
    (ivy-fd-hydra-put :type.f (when value nil))))

(defun ivy-fd-get-duration-options ()
  "Return string with changed-within and changed-before options."
  (seq-reduce
   (lambda (acc key) (if-let ((value (ivy-fd-hydra-get-non-empty key)))
                    (concat acc " "
                            (ivy-fd-keyword-to-option key)
                            " " value)
                  acc))
   '(:changed-within :changed-before) ""))

(defun ivy-fd-get-max-depth ()
  "Get string with max-depth option and value or empty string."
  (or
   (when-let ((value (ivy-fd-maybe-to-number
                      (ivy-fd-hydra-get :max-depth))))
     (when (> value 0) (format "--max-depth %d" value)))
   ""))

(defun ivy-fd-get-size ()
  "Get string with size option and value or empty string."
  (if-let ((value (ivy-fd-hydra-get-non-empty :size)))
      (format "--size %s" value)
    ""))

(defun ivy-fd-get-flags ()
  "Return string with fd flags from `ivy-fd-hydra-state'."
  (string-join
   (seq-remove #'string-empty-p
               (mapcar #'funcall
                       '(ivy-fd-map-boolean-options
                         ivy-fd-normalize-type-options
                         ivy-fd-get-duration-options
                         ivy-fd-normalize-multi-options
                         ivy-fd-get-max-depth
                         ivy-fd-get-size)))
   "\s"))

;; (defclass ivy-fd-options ()
;;   ((no-ignore :initarg :no-ignore
;;               :initform nil
;;               :type (or null t)
;;               :custom boolean
;;               :documentation "Do not respect .(git|fd)ignore files.")
;;    (max-depth :initarg :max-depth
;;               :type number
;;               :initform 0
;;               :documentation "Maximum search depth."))
;;   "A class for fd options.")

;; (cl-defmethod call-fd-options ((km-pers ivy-fd-options) &optional scriptname)
;;   "Dial the phone for the ivy-fd-options PERS.
;; Execute the program SCRIPTNAME to dial the phone."
;;   (message "Dialing the phone for %s in %s"  (slot-value km-pers 'name) (slot-value km-pers 'phone)))

;; (setq km-pers (ivy-fd-options :no-ignore t :max-depth 3))
;; (setq km-pers-2 (make-instance 'ivy-fd-options :name "Karim" :birthday "Jule" :phone "111-1111"))
;; (slot-value km-pers :no-ignore)
;; (call-fd-options km-pers-2)
;; (call-fd-options km-pers)

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

(defun ivy-fd-plist-pick (keywords pl)
  "Pick KEYWORDS props from PL."
  (let ((result)
        (keyword))
    (while (setq keyword (pop keywords))
      (when-let ((value (plist-get pl keyword)))
        (unless (null value)
          (setq result (append result (list keyword value))))))
    result))

(defun ivy-fd-plist-omit-nils (plist)
  "Remove nil values from PLIST, returning a cleaned property list.

Argument PLIST is a property list from which entries with nil values are
omitted."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun ivy-fd-plist-omit (pl &rest keywords)
  "Return copy of PL winthout properties KEYWORDS."
  (let ((result))
    (setq keywords (flatten-list keywords))
    (dotimes (idx (length pl))
      (when (eq (logand idx 1) 0)
        (let ((prop-name (nth idx pl)))
          (unless (member prop-name keywords)
            (setq result
                  (plist-put result prop-name
                             (plist-get pl prop-name)))))))
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
                (ivy-fd-plist-omit settings :type))
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

(defhydra ivy-fd-hydra-file-types (:color pink)
  "
--type

_f_ file %(ivy-fd-hydra-get :type.f)
_d_ directory  %(ivy-fd-hydra-get :type.d)
_l_ symlink  %(ivy-fd-hydra-get :type.l)
_x_ executable %(ivy-fd-hydra-get :type.x)
_e_ empty %(ivy-fd-hydra-get :type.e)
"
  ("f" (ivy-fd-toggle-file-type) nil)
  ("d" (ivy-fd-toggle-dir-type) nil)
  ("l" (ivy-fd-toggle :type.l) nil)
  ("x" (ivy-fd-toggle :type.x) nil)
  ("e" (ivy-fd-toggle :type.e) nil)
  ("RET" (ivy-fd-async ivy-fd-current-dir ivy-fd-last-input)
   :exit t)
  ("q" nil))

(defhydra ivy-fd-hydra (:color pink)
  "
fd options:

_h_ --hidden         %(ivy-fd-hydra-get :hidden) search hidden
_I_ --no-ignore      %(ivy-fd-hydra-get :no-ignore) ignore .git|fdignores
_n_ --no-ignore-vcs  %(ivy-fd-hydra-get :no-ignore-vcs) ignore .gitignore
_C_ --case-sensitive %(ivy-fd-hydra-get :case-sensitive) (default: smart case)
_i_ --ignore-case    %(ivy-fd-hydra-get :ignore-case) (default: smart case)
_g_ --glob           %(ivy-fd-hydra-get :glob)
_F_ --fixed-strings  %(ivy-fd-hydra-get :fixed-strings) as a literal string
_a_ --absolute-path  %(ivy-fd-hydra-get :absolute-path)  absolute paths
_L_ --follow         %(ivy-fd-hydra-get :follow)  follow symbolic links
_p_ --full-path      %(ivy-fd-hydra-get :full-path) search full path

   Depth

_m_ --max-depth %(ivy-fd-hydra-get :max-depth)
_<up>_ increase
_<down>_ decrease

--type

_f_ file %(ivy-fd-hydra-get :type.f)
_d_ directory  %(ivy-fd-hydra-get :type.d)
_l_ symlink  %(ivy-fd-hydra-get :type.l)
_x_ executable %(ivy-fd-hydra-get :type.x)
_e_ empty %(ivy-fd-hydra-get :type.e)

_._ --extension %(ivy-fd-hydra-get :extension)
_s_ --size  %(ivy-fd-hydra-get :size)
_E_ --exclude %(ivy-fd-hydra-get :exclude) <pattern>

By file modification

_w_ within %(ivy-fd-hydra-get :changed-within)
_b_ before %(ivy-fd-hydra-get :changed-before)

Settings

_RET_ done
_D_ change directory %`ivy-fd-current-dir
_r_ apply directory settings
_z_ merge with directory settings
"
  ("h" (ivy-fd-toggle :hidden) nil)
  ("I" (ivy-fd-toggle :no-ignore) nil)
  ("n" (ivy-fd-toggle :no-ignore-vcs) nil)
  ("C" (ivy-fd-toggle :case-sensitive) nil)
  ("i" (ivy-fd-toggle :ignore-case) nil)
  ("g" (ivy-fd-toggle :glob) nil)
  ("F" (ivy-fd-toggle :fixed-strings) nil)
  ("a" (ivy-fd-toggle :absolute-path) nil)
  ("L" (ivy-fd-toggle :follow) nil)
  ("p" (ivy-fd-toggle :full-path) nil)
  ("m" (ivy-fd-fdfind-read-max-depth) nil)
  ("<up>" (ivy-fd-increase-depth) nil)
  ("<down>" (ivy-fd-decrease-depth) nil)
  ("f" (ivy-fd-toggle-file-type) nil)
  ("d" (ivy-fd-toggle-dir-type) nil)
  ("l" (ivy-fd-toggle :type.l) nil)
  ("x" (ivy-fd-toggle :type.x) nil)
  ("e" (ivy-fd-toggle :type.e) nil)
  ("." (ivy-fd-read-multy-options :extension) nil)
  ("s" (ivy-fd-hydra-put :size (ivy-fd-read-size)) nil)
  ("E" (ivy-fd-read-multy-options :exclude) nil)
  ("w" (ivy-fd-hydra-put :changed-within (ivy-fd-read-date)) nil)
  ("b" (ivy-fd-hydra-put :changed-before (ivy-fd-read-date)) nil)
  ("RET" (ivy-fd-async ivy-fd-current-dir ivy-fd-last-input) :exit t)
  ("D" (setq ivy-fd-current-dir (read-directory-name
                                 "Directory: "
                                 (or ivy-fd-current-dir default-directory)))
   nil)
  ("r" (ivy-fd-set-directory-settings
        (or ivy-fd-current-dir default-directory))
   nil)
  ("z" (ivy-fd-merge-directory-settings
        (or ivy-fd-current-dir default-directory))
   nil)
  ("q" nil))

;;;###autoload
(defun ivy-fd-read-flags ()
  "Invoke hydra to configure fd flags."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (setq ivy-fd-last-input ivy-text)
        (ivy-quit-and-run (ivy-fd-hydra/body)))
    (ivy-fd-hydra/body)))

;;;###autoload
(defun ivy-fd-find-directory-up ()
  "Change `ivy-fd-current-dir' to parent directory."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (funcall-interactively #'ivy-fd-async
                                      (ivy-fd-parent-dir ivy-fd-current-dir)
                                      ivy-fd-last-input)))
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
  (when-let ((curr (ivy-state-current ivy-last)))
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

(defun ivy-fd-make-shell-command ()
  "Return string fd shell command."
  (concat ivy-fd-exec-path " -0 --color=never " (ivy-fd-get-flags)
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
    (define-key map (kbd "C-c C-f") #'ivy-fd-hydra-file-types/body)
    (define-key map (kbd "C-c C-i") #'ivy-fd-insert-filename)
    (define-key map (kbd "M-w") #'ivy-fd-copy-filename)
    map))

(defvar ivy-fd-sync-command nil)

;;;###autoload
(defun ivy-fd-sync (&optional initial-input)
  "Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (unless ivy-fd-current-dir
    (setq ivy-fd-current-dir default-directory))
  (setq ivy-fd-sync-command (concat "fdfind --color=never "
                                    (ivy-fd-get-flags)
                                    " . "
                                    (abbreviate-file-name
                                     ivy-fd-current-dir)))
  (ivy-read ivy-fd-sync-command (split-string
                                 (shell-command-to-string
                                  ivy-fd-sync-command)
                                 "\n")
            :initial-input initial-input
            :history 'ivy-fd-async-history
            :keymap ivy-fd-map
            :action 'ivy-fd-find-file-or-preview
            :caller 'ivy-fd-sync))

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


;;;###autoload
(defun ivy-fd-async (&optional directory initial-input args)
  "Search in DIRECTORY or `default-directory' with INITIAL-INPUT and ARGS."
  (interactive)
  (setq ivy-fd-current-dir (ivy-fd-slash
                            (expand-file-name
                             (or directory default-directory))))
  (setq ivy-fd-hydra-state (or args
                               (ivy-fd-get-dir-settings
                                ivy-fd-current-dir)))
  (setq ivy-fd-async-command (ivy-fd-make-shell-command))
  (unwind-protect
      (let ((default-directory ivy-fd-current-dir))
        (ivy-read (concat
                   (abbreviate-file-name ivy-fd-current-dir)
                   ": "
                   (let ((flags (string-trim (ivy-fd-get-flags))))
                     (if (string-empty-p flags)
                         ""
                       (concat flags " "))))
                  #'ivy-fd-async-function
                  :initial-input initial-input
                  :dynamic-collection t
                  :history 'ivy-fd-async-history
                  :keymap ivy-fd-map
                  :action 'ivy-fd-find-file-or-preview
                  :caller 'ivy-fd-async))
    (ivy-fd-delete-process)))

;;;###autoload
(defun ivy-fd-async-read-directory (&optional directory initial-input args)
  "Search files asynchronously in a DIRECTORY with `fd' and Ivy.

Optional argument DIRECTORY is the directory to read from. It defaults to
`default-directory'.

Optional argument INITIAL-INPUT is the initial input for the search. It defaults
to nil.

Optional argument ARGS is additional arguments for the search. It defaults to
nil."
  (interactive)
  (setq ivy-fd-current-dir (ivy-fd-slash
                            (expand-file-name
                             (or directory default-directory))))
  (ivy-fd-hydra-put :type.d t)
  (ivy-fd-hydra-put :type.f nil)
  (unless directory
    (setq ivy-fd-hydra-state (or args
                                 (ivy-fd-get-dir-settings ivy-fd-current-dir))))
  (setq ivy-fd-async-command (ivy-fd-make-shell-command))
  (unwind-protect
      (let ((default-directory ivy-fd-current-dir))
        (ivy-read (concat
                   (abbreviate-file-name ivy-fd-current-dir)
                   ": "
                   (let ((flags (string-trim (ivy-fd-get-flags))))
                     (if (string-empty-p flags)
                         ""
                       (concat flags " "))))
                  #'ivy-fd-async-function
                  :initial-input initial-input
                  :dynamic-collection t
                  :history 'ivy-fd-async-history
                  :keymap ivy-fd-map
                  :action 'ivy-fd-find-file-or-preview
                  :caller 'ivy-fd-async))
    (ivy-fd-delete-process)))

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
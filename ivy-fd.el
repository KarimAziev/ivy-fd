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
           (:extensions (repeat (string :tag "Extensions")))
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


(defvar ivy-fd-args nil)
(defvar ivy-fd-current-dir nil)
(defvar ivy-fd-last-input nil)
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
  "Return list of files  in PLACE.
PLACE can be a string of directory, list of directories, or alist of directories
with extra flags.

FLAGS and IGNORES should be string, list or alist of strings.

IGNORED may omit --exclude flag."
  (split-string (shell-command-to-string
                 (ivy-fd-make-sortable-tr-command place flags ignored))
                "\0" t))

(defun ivy-fd-make-command (&optional dir flags)
  "Generate string with fd command in DIR from FLAGS."
  (ivy-fd-generic-list-to-string
   "fdfind" "--color=never"
   flags
   (ivy-fd-generic-list-to-string "." dir)))

(defun ivy-fd-find (place &optional flags)
  "Return list of files  in PLACE.
PLACE can be a string of directory, list of directories, or alist of directories
with extra flags.

FLAGS should be string, list or alist of strings."
  (split-string (shell-command-to-string
                 (ivy-fd-make-command place flags))
                "\n" t))

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

(defun ivy-fd-async-function (input)
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



(defvar ivy-fd-hydra-state '(:hidden nil :no-ignore nil))

(defun ivy-fd-hydra-get (keyword)
  "Get value of KEYWORD from `fd-hydra-state'."
  (plist-get ivy-fd-hydra-state keyword))

(defun ivy-fd-hydra-get-non-empty (keyword)
  "Get value of KEYWORD from `fd-hydra-state'."
  (when-let ((value (plist-get ivy-fd-hydra-state keyword)))
    (when (and (stringp value)
               (not (string-empty-p value)))
      value)))

(defun ivy-fd-hydra-put (keyword value)
  "Put KEYWORD with VALUE to `fd-hydra-state'."
  (setq ivy-fd-hydra-state
        (plist-put ivy-fd-hydra-state keyword value)))

(defun ivy-fd-read-date (&rest _)
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
        (format (nth 2 answer)
                (read-number (format "%s " (nth 1 answer))))
      (pcase (car answer)
        (?t (read-string (format-time-string "%Y-%m-%d %H:%M:%S"
                                             (current-time))))
        (?o (read-string "Value: "))))))

(defun ivy-fd-read-size (&rest _args)
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



(defun ivy-fd-get-flags ()
  "Return string with fd flags from `ivy-fd-hydra-state'."
  (string-join
   ivy-fd-args
   "\s"))



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
  "Return the keys in PLIST."
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




;;;###autoload
(defun ivy-fd-read-flags ()
  "Invoke hydra to configure fd flags."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (setq ivy-fd-last-input ivy-text)
        (ivy-quit-and-run (ivy-fd-transient)))
    (ivy-fd-transient)))

;;;###autoload
(defun ivy-fd-find-directory-up ()
  "Change `ivy-fd-current-dir' to parent directory."
  (interactive)
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (funcall-interactively #'ivy-fd-async
                                      (ivy-fd-parent-dir ivy-fd-current-dir)
                                      ivy-fd-args
                                      ivy-fd-last-input)))
    (funcall-interactively #'ivy-fd-async
                           (ivy-fd-parent-dir default-directory)
                           ivy-fd-args
                           ivy-fd-last-input)))

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

;;;###autoload
(defun ivy-fd-find-file-other-window ()
  "Find FILE if `ivy-exit', otherwise preview FILE."
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
                                        ivy-fd-args
                                        ivy-fd-last-input))))
    (let ((directory (read-directory-name "Search in:\s")))
      (setq ivy-fd-hydra-state (ivy-fd-set-directory-settings
                                directory))
      (funcall-interactively #'ivy-fd-async
                             directory
                             ivy-fd-args
                             ivy-fd-last-input))))

(defun ivy-fd-make-shell-command ()
  "Return string fd shell command."
  (concat ivy-fd-exec-path " -0 --color=never " (string-join ivy-fd-args)
          (concat " %s . ")))

;;;###autoload
(defun ivy-fd-toggle-hidden ()
  "Toggle --hidden flag."
  (interactive)
  (setq ivy-fd-args (if
                        (member "--hidden" ivy-fd-args)
                        (delete "--hidden" ivy-fd-args)
                      (push "--hidden" ivy-fd-args)))
  (if (active-minibuffer-window)
      (progn (setq ivy-fd-last-input ivy-text)
             (ivy-quit-and-run
               (funcall-interactively #'ivy-fd-async
                                      ivy-fd-current-dir
                                      ivy-fd-args
                                      ivy-fd-last-input)))
    (funcall-interactively #'ivy-fd-async
                           (read-directory-name "Search in:\s")
                           ivy-fd-args
                           ivy-fd-last-input)))

(require 'transient)


(defvar ivy-fd-file-types '(("f" . "file")
                            ("d" . "directory")
                            ("l" . "symlink")
                            ("x" . "executable")
                            ("e" . "empty")
                            ("p" . "pipe")))


(transient-define-argument ivy-fd-read-file-type ()
  "Read file type for fd."
  :argument "-t "
  :description "File type"
  :multi-value 'repeat
  :class 'transient-option
  :choices
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata
          (annotation-function . (lambda (str)
                                   (concat " " (or
                                                (cdr
                                                 (assoc str
                                                        ivy-fd-file-types))
                                                ""))))
          (cycle-sort-function . (lambda (it) it))
          (display-sort-function . (lambda (it) it)))
      (complete-with-action action ivy-fd-file-types str pred))))

(transient-define-argument ivy-fd-read-file-ext ()
  "Read file type for fd."
  :argument "-e "
  :description "File extension"
  :multi-value 'repeat
  :class 'transient-option)

(transient-define-prefix ivy-fd-transient ()
  "Fd 8.3.1.
USAGE:
fd [FLAGS/OPTIONS] [<pattern>] [<path>...]

FLAGS:"
  :value (lambda () ivy-fd-args)
  ["Flags"
   ("-H" " Search hidden files and directories" "--hidden")
   ("-I" " Do not respect .(git|fd)ignore" "--no-ignore")
   ("-s" " Case-sensitive search (default: smart-case)" "--case-sensitive")
   ("-i" " Case-insensitive search (default: smart-case)" "--ignore-case")
   ("-g" " Glob-based search (default: regular expression)" "--glob")
   ("-a" " Show absolute instead of relative" "--absolute-path")
   ("-l" " Use a long listing format with file metadata" "--list-details")
   ("-L" " Follow symbolic links" "--follow")
   ("-p" "Search full abs." "--full-path")]
  ["Options"
   ("t" ivy-fd-read-file-type)
   ("e" ivy-fd-read-file-ext)
   ("d" "Maximum search depth " "--max-depth " :class transient-option)
   ("w" "Filter by file modification time (newer than) " "--changed-within "
    :class transient-option
    :reader ivy-fd-read-date)
   ("b" "Filter by file modification time (older than) " "--changed-before "
    :class transient-option
    :reader ivy-fd-read-date)
   ("s" "Size" "--size "
    :class transient-option
    :reader ivy-fd-read-size)]
  ["Actions"
   ("RET" "Run" ivy-fd-async)])

;;;###autoload
(defun ivy-fd-change-max-depth ()
  "Change --max-depth flag."
  (interactive)
  (read-string "Max-depth"))

(defvar ivy-fd-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<backspace>") #'ivy-fd-find-directory-up)
    (define-key map (kbd "C-l") #'ivy-fd-find-directory-up)
    (define-key map (kbd "C-.")  #'ivy-fd-change-dir)
    (define-key map (kbd "C-q")  #'ivy-fd-read-flags)
    (define-key map (kbd "C-c C-o") #'ivy-fd-find-file-other-window)
    (define-key map (kbd "M-.") #'ivy-fd-toggle-hidden)
    (define-key map (kbd "M-<up>") #'ivy-fd-change-max-depth)
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
    (funcall-interactively (or 'ivy-fd-async) project nil initial-input)))

;;;###autoload
(defun ivy-fd-async-project (&optional directory initial-input)
  "Search in DIRECTORY or `default-directory' with INITIAL-INPUT."
  (interactive)
  (let ((project (or directory (funcall ivy-fd-resolve-project-root-fn)
                     default-directory)))
    (funcall-interactively (or 'ivy-fd-async)
                           (or directory project) nil initial-input)))

;;;###autoload
(defun ivy-fd-identity-or-preview (file)
  "Return absolute name of FILE if `ivy-exit' is non nil, otherwise preview."
  (setq file (ivy-fd-expand-file file))
  (if ivy-exit
      file
    (ivy-fd-preview-file file)))


;;;###autoload
(defun ivy-fd-async (&optional directory args initial-input action)
  "Search in DIRECTORY or `default-directory' with INITIAL-INPUT and ARGS."
  (interactive)
  (setq ivy-fd-current-dir (ivy-fd-slash
                            (expand-file-name
                             (or directory default-directory))))
  (setq ivy-fd-args (or args (transient-args transient-current-command)))
  (setq ivy-fd-async-command (concat ivy-fd-exec-path " -0 --color=never "
                                     (string-join ivy-fd-args)
                                     (concat " %s . ")))
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
                  :keymap ivy-fd-map
                  :caller 'ivy-fd-async
                  :action
                  (lambda (file)
                    (setq file (ivy-fd-expand-file file))
                    (if ivy-exit
                        (funcall (or action 'find-file) file)
                      (ivy-fd-preview-file file)))))
    (ivy-fd-delete-process)))

(ivy-add-actions 'ivy-fd-async
                 '(("j" ivy-fd-find-file-other-window-action
                    "find file in other window")))

(provide 'ivy-fd)
;;; ivy-fd.el ends here
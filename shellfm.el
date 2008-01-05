;;;; Customize options
(defgroup shellfm nil
  "Emacs interface to shell-fm."
  :link '(url-link "http://nex.scrapping.cc/shell-fm/")
  :group 'external
  :group 'multimedia
  :tag "Shell.FM")

(defcustom shellfm-executable "/usr/bin/shell-fm"
  "Path to shell-fm executable."
  :group 'shellfm
  :type 'string
  :tag "Shell-fm executable")

(defcustom shellfm-args ""
  "Addition command line options to be passed to shell-fm."
  :group 'shellfm
  :type 'string
  :tag "Shell-fm command line options")

(defcustom lastfm-default-url "lastfm://globaltags/Emacs"
  "Default lastfm:// URL for shell-fm."
  :group 'shellfm
  :type '(choice string (const :tag "Empty URL" "lastfm://"))
  :tag "Last.fm default URL")

(defcustom lastfm-user ""
  "Your Last.fm account name."
  :group 'shellfm
  :type 'string
  :tag "Last.fm account")


;;;; Miscellaneous internal functions

(defun pattern-parse-file (file pattern)
  "Extract strings matching first subexpression of PATTERN from FILE.

FILE is an ordinary filename.

PATTERN is a regular expression which has at least one
subexpression.

This function return a list of matching strings."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((result '()))
      (while (re-search-forward pattern nil t)
        (add-to-list 'result (match-string-no-properties 1)))
      result)))

(defvar shellfm-tag-pattern "[ a-z0-9-]*"
  "Regular expression to match Last.fm tags")

;; (defun shellfm-get-lasftm-user ()
;;   "Extract Last.fm username from shell-fm config."
;;   (car (pattern-parse-file "~/.shell-fm/shell-fm.rc"
;;                            "^username = \\(.+\\)$")))

(defun shellfm-get-overall-tags ()
  "Get a list of most popular Last.fm tags from shell-fm cache."
  (pattern-parse-file "~/.shell-fm/cache/overall-tags"
                      (concat "<tag name=\"\\("
                              shellfm-tag-pattern
                              "\\)\" count=.+ url=.+/>")))

(defun shellfm-get-top-artists ()
  "Get a list of user's top artists from shell-fm cache."
  (pattern-parse-file "~/.shell-fm/cache/top-artists"
                      "^.*,\\(.+\\)$"))

(defun shellfm-get-private-tags ()
  "Get a list of user's own Last.fm tags from shell-fm cache."
  (pattern-parse-file (concat "~/.shell-fm/cache/usertags-" lastfm-user)
                      (concat "^.+,\\("
                              shellfm-tag-pattern
                              "\\),.+$")))


;;;; Visible functions

(defun shellfm-running ()
  "Returns t if shell-fm subprocess is running, nil otherwise."
  (get-process "shell-fm"))

(defun shellfm (&optional arg)
  "Start shell-fm subprocess."
  (interactive "P")
  (if (not (shellfm-running))
      (progn (start-process "shell-fm" nil shellfm-executable
                            (concat "-b" shellfm-args) lastfm-default-url)
             (require 'shellfm-functions))
    (message "Shell.FM is already running")))

(provide 'shellfm)
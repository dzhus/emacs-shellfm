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
  :tag "Executable")

(defcustom shellfm-args ""
  "Addition command line options to be passed to shell-fm."
  :group 'shellfm
  :type 'string
  :tag "Command line options")

(defcustom shellfm-user ""
  "Your Last.fm account name."
  :group 'shellfm
  :type 'string
  :tag "Last.fm account")

(defun shellfm (&optional arg)
  "Start shell-fm subprocess."
  (interactive)
  (if (not (get-process "shell-fm"))
      (progn (start-process "shell-fm" nil shellfm-executable)
             (require 'shellfm-functions)
             (if arg
                 (call-interactively 'shellfm-url)))
    (message "Shell.FM is already running")))

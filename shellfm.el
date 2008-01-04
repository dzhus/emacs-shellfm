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

(defun shellfm (&optional arg)
  "Start shell-fm subprocess.

If it is already running"
  (interactive "P")
  (if (not (get-process "shell-fm"))
      (progn (start-process "shell-fm" nil shellfm-executable
                            (concat "-b" shellfm-args) lastfm-default-url)
             (require 'shellfm-functions))
    (message "Shell.FM is already running"))
  (if arg (call-interactively 'shellfm-url)))


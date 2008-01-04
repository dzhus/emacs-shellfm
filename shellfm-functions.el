(defsubst shellfm-command (string)
  "Send a string terminated by newline to the shell-fm
subprocess."
  (process-send-string "shell-fm" (concat string "\n")))

(defsubst shellfm-radio-command (namespace value)
  "Switch to Last.fm radio given its type and value.

Switch to lastfm://<namespace>/<value> radio"
  (shellfm-url (concat namespace "/" value)))

;;; Interactive user-oriented functions
(defun shellfm-url (url)
  "Switch to given lastfm:// URL.

You may omit lastfm:// part."
  (interactive "slasftm:// URL: ")
  (shellfm-command (concat "r" url)))

(defun shellfm-station-tag (tag)
  "Switch to global tag station.

Several tags separated with comma (like `rock,jazz,vocals`) may
be passed."
  (interactive "sSwitch to global tag station: ")
  (shellfm-radio-command "globaltags" tag))

(defun shellfm-station-recommended ()
  "Switch to recommended tracks station."
  (interactive "nObscurity [0-100]: ")
  ;; 100 is unknown magic constant. It's uncertain if this is an
  ;; obscurity level.
  (shellfm-radio-command "user" (concat lastfm-user "/recommended/100/")))

(defun shellfm-station-artist (artist)
  "Switch to similar artists station."
  (interactive "sSwitch to artist station: ")
  (shellfm-radio-command "artist" artist))

(defun shellfm-station-group (group)
  "Switch to group station."
  (interactive "sGroup: ")
  (shellfm-radio-command "group" group))

(defun shellfm-station-similar-artists ()
  "Switch to listening to artists similar to that of current track."
  (interactive)
  (shellfm-command "s"))

(defun shellfm-station-fans ()
  "Switch to station of current artist's fans."
  (interactive)
  (shellfm-command "f"))

(defun shellfm-skip-track ()
  "Skip current track."
  (interactive)
  (shellfm-command "n"))

(defun shellfm-love-track ()
  "Mark current track as loved."
  (interactive)
  (shellfm-command "l"))

(defun shellfm-ban-track ()
  "Ban current track."
  (interactive)
  (shellfm-command "B"))

(defun shellfm-pause ()
  "Pause/play current track."
  (interactive)
  (shellfm-command "p"))

(defun shellfm-stop ()
  "Stop playing."
  (interactive)
  (shellfm-command "S"))

(defun shellfm-kill ()
  "Kill shell-fm subprocess and disable all related functions."
  (interactive)
  (delete-process "shell-fm")
  (unload-feature 'shellfm-functions))

(provide 'shellfm-functions)
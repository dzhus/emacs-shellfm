(require 'shellfm)

;;;; Completion lists

(defvar shellfm-completion-artists (shellfm-get-top-artists)
  "A list of known artists to be used in minibuffer completion.")

(defvar shellfm-completion-tags (append (shellfm-get-overall-tags)
                                        (shellfm-get-private-tags))
  "A list of known tags to be used in minibuffer completion.")


;;;; Inline functions talking to shell-fm process

(defsubst shellfm-command (string)
  "Send a string terminated by newline to the shell-fm
subprocess."
  (process-send-string "shell-fm" (concat string "\n")))

(defsubst shellfm-radio-command (namespace value)
  "Switch to Last.fm radio given its type and value.

Switch to lastfm://<namespace>/<value> radio"
  (shellfm-url (concat namespace "/" value)))


;;;; Radio tuning commands

(defun shellfm-url (url)
  "Switch to given lastfm:// URL.

You may omit lastfm:// part."
  (interactive "slasftm:// URL: ")
  (shellfm-command (concat "r" url)))

(defun shellfm-station-tag (&optional tag)
  "Switch to global tag station.

Several tags separated with comma (like `rock,jazz,vocals`) may
be passed."
  (interactive)
  (let ((real-tag 
         (if tag tag
           ;; Override local minibuffer keymap to avoid blocking of
           ;; tags with spaces
           (let ((minibuffer-local-completion-map
                  (assq-delete-all 32 minibuffer-local-completion-map)))
             (completing-read "Tag(s): " shellfm-completion-tags)))))
    (shellfm-radio-command "globaltags" real-tag)))

(defun shellfm-station-recommended ()
  "Switch to recommended tracks station."
  (interactive "nObscurity [0-100]: ")
  ;; 100 is unknown magic constant. I'm uncertain if this is really an
  ;; obscurity level.
  (shellfm-radio-command "user" (concat lastfm-user "/recommended/100/")))

(defun shellfm-station-artist (&optional artist)
  "Switch to similar artist station."
  (interactive)
  (let ((real-artist
         (if artist artist
           (let ((minibuffer-local-completion-map
                  (assq-delete-all 32 minibuffer-local-completion-map)))
             (completing-read "Artist: " shellfm-completion-artists)))))
    (shellfm-radio-command "artist" real-artist)))

(defun shellfm-station-group (group)
  "Switch to group station."
  (interactive "sGroup: ")
  (shellfm-radio-command "group" group))


;;;; Current track commands

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


;;;; Global state control commands

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
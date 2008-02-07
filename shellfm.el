;;; shellfm.el --- Emacs Shell.FM interface

;; Copyright (C) 2008 Dmitry Dzhus

;; Author: Dmitry Dzhus <mail@sphinx.net.ru>
;; Keywords: scrobbling audio music last.fm

;; This file is a part of emacs-shellfm.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Emacs-shellfm is the Emacs interface for Shell.fm console client
;; for Last.fm.

;;; Code:

;;;; Customization options

(defgroup shellfm nil
  "Emacs interface to shell-fm."
  :link '(url-link "http://nex.scrapping.cc/shell-fm/")
  :group 'external
  :group 'multimedia
  :tag "Shell.FM")

(defcustom shellfm-program "/usr/bin/shell-fm"
  "Path to shell-fm executable."
  :group 'shellfm
  :type 'string
  :tag "Shell-fm program path")

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

(defcustom shellfm-track-hook '(shellfm-track-info)
  "Functions to be run when shell-fm starts streaming a new track."
  :group 'shellfm
  :type 'hook)

(defcustom shellfm-status-hook '(shellfm-show-status)
  "Functions to be run when shell-fm status changes.

See `shellfm-set-status'."
  :group 'shellfm
  :type 'hook)


;;;; Shell-fm cache processing functions

(defun pattern-parse-file (file pattern)
  "Return a list of strings from FILE matching first subexpression of PATTERN.

FILE is an ordinary filename as in `insert-file-contents'.

PATTERN is a regular expression which has at least one
subexpression."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((result '()))
      (while (re-search-forward pattern nil t)
        (add-to-list 'result (match-string-no-properties 1)))
      result)))

(defvar shellfm-tag-pattern "[ a-z0-9-]*"
  "Regular expression to match Last.fm tags.")

(defun shellfm-get-lastfm-user ()
  "Extract Last.fm username from shell-fm config."
  (car (pattern-parse-file "~/.shell-fm/shell-fm.rc"
                           "^username = \\(.+\\)$")))

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

(defun shellfm-get-friends ()
  "Get a list of user's friends from shell-fm cache."
  (pattern-parse-file "~/.shell-fm/cache/friends"
                     "^\\(.+\\)$"))

(defun shellfm-get-neigbors ()
  "Get a list of user's neighbors from shell-fm cache."
  (pattern-parse-file "~/.shell-fm/cache/neighbors"
                     "^[0-9.]+,\\(.+\\)$"))

(defun shellfm-get-private-tags ()
  "Get a list of user's own Last.fm tags from shell-fm cache."
  (pattern-parse-file (concat "~/.shell-fm/cache/usertags-" (shellfm-get-lastfm-user))
                      (concat "^.+,\\("
                              shellfm-tag-pattern
                              "\\),.+$")))


;;;; Shell-fm global state variables

(defvar shellfm-current-title "No tracks played yet"
  "Title of Last.fm track being played.")

(defvar shellfm-current-artist "Unknown"
  "Currently played Last.fm artist.")

(defvar shellfm-current-station "no station"
  "Last.fm station being streamed.")

(defvar shellfm-status 'dead
  "Current shell-fm subprocess status.

Possible values of this symbol are:
dead -- subprocess is not running.
started -- shell-fm subprocess just started.
radio -- streaming a Last.fm radio.
paused -- playback is paused.
stopped -- streaming has been stopped.")

(defun shellfm-set-status (status)
  "Set shell-fm global status to STATUS.

This is a setter function for `shellfm-status' variable."
  (setq shellfm-status status)
  (run-hooks 'shellfm-status-hook))

(defun shellfm-set-track (title artist)
  "Store current track TITLE and ARTIST.

This is a setter function for `shellfm-current-artist' and
`shellfm-current-title' variables."
  (setq shellfm-current-artist artist)
  (setq shellfm-current-title title)
  (run-hooks 'shellfm-track-hook))

(defun shellfm-set-station (station)
  "Store current STATION.

This is a setter function for `shellfm-current-station'
variable."
  (setq shellfm-current-station station))


;;;; Shell-fm subprocess filtering

(defvar shellfm-nowplaying-regexp "Now playing \"\\(.+\\)\" by \\(.+\\)\."
  "Regular expression to match \"Now playing\" message in shell-fm output.

It must contain two subexpressions, for track and artist
respectively.")

(defvar shellfm-station-regexp "^Receiving \\(.+\\).$"
  "Regular expression to match \"Receiving <station>.\" message in shell-fm output.

This variable must contain one subexpression to match station
name.")

(defun shellfm-process-filter (process data)
  "Filter function for shell-fm subprocess.

Reads shell-fm output and updates `shellfm-current-title',
`shellfm-current-artist', `shellfm-current-station',
`shellfm-status' variables."
  (when (string-match shellfm-nowplaying-regexp data)
    (shellfm-set-track (match-string 1 data)
                               (match-string 2 data)))
  (when (string-match shellfm-station-regexp data)
    (shellfm-set-station (match-string 1 data))
    (shellfm-set-status 'radio)))


;;;; Completion lists

(defvar shellfm-completion-artists (shellfm-get-top-artists)
  "A list of known artists to be used in minibuffer completion.")

(defvar shellfm-completion-tags (append (shellfm-get-overall-tags)
                                        (shellfm-get-private-tags))
  "A list of known tags to be used in minibuffer completion.")

(defvar shellfm-completion-recipients (append (list (shellfm-get-lastfm-user))
                                              (shellfm-get-friends)
                                              (shellfm-get-neigbors))
  "A list of known recipients to send recommendations to.")


;;;; Inline functions talking to shell-fm process

(defsubst shellfm-command (string)
  "Send a STRING to the shell-fm subprocess.

Newline is added automagically."
  (when (shellfm-running-p)
    (process-send-string "shell-fm" (concat string "\n"))))

(defsubst shellfm-radio-command (namespace name)
  "Switch to lastfm://NAMESPACE/NAME radio."
  (shellfm-url (concat namespace "/" name)))


;;;; Radio tuning commands

(defun shellfm-url (url)
  "Switch to given lastfm:// URL.

You may omit lastfm:// part."
  (interactive "slasftm:// URL: ")
  (shellfm-command (concat "r" url)))

(defun shellfm-completing-read (prompt completion-table)
  "Read user input interactively and return it.

PROMPT is input message, COMPLETION-TABLE is a list of possible
completions as in `completing-read'.

Character case is ingored while completing, space is used to
insert itself, not for word completing."
  (let ((completion-ignore-case t)
        (minibuffer-local-completion-map
         (assq-delete-all 32 minibuffer-local-completion-map)))
    (completing-read prompt completion-table)))

(defun shellfm-station-completing (prompt completion-table namespace &optional name)
  "Prompt for station, if needed and switch to it.

Start a completing interactive prompt given PROMPT text,
COMPLETION-TABLE, and switch to station using its NAMESPACE and NAME as in
`shellfm-radio-command'."
  (let ((real-name
         (if name name
           (shellfm-completing-read prompt completion-table))))
    (shellfm-radio-command namespace real-name)))

(defun shellfm-station-tag (&optional tag)
  "Switch to global TAG station.

Several tags separated with comma (like \"rock,jazz,vocals\") may
be passed.

This function always returns nil."
  (interactive)
  (shellfm-station-completing "Tag(s): " shellfm-completion-tags "globaltags" tag))

(defun shellfm-station-artist (&optional artist)
  "Switch to similar ARTIST station.

This function always returns nil."
  (interactive)
  (shellfm-station-completing "Artist: " shellfm-completion-artists "artist" artist))

(defun shellfm-station-recommended ()
  "Switch to recommended tracks station."
  (interactive)
  ;; 100 is unknown magic constant. I'm uncertain if this is really an
  ;; obscurity level.
  (shellfm-radio-command "user" (concat (shellfm-get-lastfm-user) "/recommended/100/")))

(defun shellfm-station-playlist ()
  "Switch to personal playlist station."
  (interactive)
  (shellfm-radio-command "user" (concat (shellfm-get-lastfm-user) "/playlist")))

(defun shellfm-station-group (group)
  "Switch to GROUP station."
  (interactive "sGroup: ")
  (shellfm-radio-command "group" group))


;;;; Current track commands

(defmacro define-shellfm-simple-command (command-name string &optional doc)
  "Define a new command COMMAND-NAME activated by STRING sent to Shell.fm.

DOC is an optional documentation string."
  `(defun ,command-name ()
     ,doc
     (interactive)
     (shellfm-command ,string)))

(define-shellfm-simple-command shellfm-station-similar-artists "s"
  "Switch to listening to artists similar to that of current track.")

(define-shellfm-simple-command shellfm-station-fans "f"
  "Switch to station of current artist's fans.")

(define-shellfm-simple-command shellfm-skip-track "n"
  "Skip current track.")

(define-shellfm-simple-command shellfm-love-track "l"
  "Mark current track as loved.")

(define-shellfm-simple-command shellfm-ban-track "B"
  "Ban current track.")

(define-shellfm-simple-command shellfm-add-to-playlist "a"
  "Add current track to personal playlist.")

(defun shellfm-track-info ()
  "Show current track title and artist in echo area."
  (interactive)
  (if (memq shellfm-status '(radio paused))
      (message (format "Currently playing %s — %s"
                       (propertize shellfm-current-artist 'face 'bold)
                       (propertize shellfm-current-title 'face 'bold)))
    (message "Not available.")))

(defmacro define-shellfm-tag-command (command-name tagging-type &optional doc)
  "Define a new command to tag track, artist or album.

COMMAND-NAME is a name to be given to a new function,
TAGGING-TYPE is either \"t\", \"a\" or \"l\" for command tagging
track, artist and album, respectively. DOC is an optional
documentation string."
  `(defun ,command-name (&optional tags)
     ,doc
     (interactive)
     (let ((real-tags
            (if tags tags
              ;; If called interactively, read tags one by one
              (let ((tag-list '()))
                (while
                    (let ((tag (shellfm-completing-read
                                (if (null tag-list)
                                    "Enter tag: "
                                  (concat "Enter next tag or hit RET to send "
                                          (number-to-string (length tag-list))
                                          " tag(s): "))
                                shellfm-completion-tags)))
                      (if (string= "" tag) nil
                        (add-to-list 'tag-list tag))))
                tag-list))))
       (shellfm-command (concat "T" ,tagging-type
                                (mapconcat (lambda (s) s) real-tags ","))))))
     
(define-shellfm-tag-command shellfm-tag-track "t"
  "Tag current track")

(define-shellfm-tag-command shellfm-tag-artist "a"
  "Tag current artist")

(define-shellfm-tag-command shellfm-tag-album "l"
  "Tag current album")


(defmacro define-shellfm-recommend-command (command-name recommending-type &optional doc)
  "Define a new recommendation command.

COMMAND-NAME is a name to be given to function, RECOMMENDING-TYPE
is either \"a\", \"t\" or \"l\". DOC is an optional documentation string."
  `(defun ,command-name (&optional recipient comment)
     ,doc
     (interactive)
     (let ((recommendation
           (if (and recipient comment)
               (concat recipient "\n" comment)
             (concat (shellfm-completing-read "Recipient: "
                                              shellfm-completion-recipients)
                     "\n"
                     (read-string "Comment: ")))))
       (shellfm-command (concat "R" ,recommending-type recommendation)))))

(define-shellfm-recommend-command shellfm-recommend-track "t"
  "Recommend current track")

(define-shellfm-recommend-command shellfm-recommend-artist "a"
  "Recommend current artist")

(define-shellfm-recommend-command shellfm-recommend-album "l"
  "Recommend current album")


(defmacro define-shellfm-dispatching-command (command-name prompt choice-map &optional doc)
  "Define an interactive command prompting user for further actions.

COMMAND-NAME will ask user with `read-char' using PROMPT and call
another function depending on his choice. CHOICE-MAP is an alist
where key is a character provided by user and value is a function
to call in that case.

Example for CHOICE-MAP: 

    '((?p . play) (?x . exit))

In case user entered a character missing in CHOICE-MAP, nothing
happens.

DOC is an optional documentation string."
  `(defun ,command-name ()
     ,doc
     (interactive)
     (let* ((choice (read-char ,prompt))
            (command (cdr (assoc choice ,choice-map))))
       (when command
         (call-interactively command)))))

(define-shellfm-dispatching-command shellfm-tag
  "Tag (t)rack, (a)rtist or a(l)bum?"
  '((?t . shellfm-tag-track)
    (?a . shellfm-tag-artist)
    (?l . shellfm-tag-album))
  "Tag current track, artist or album.")

(define-shellfm-dispatching-command shellfm-recommend
  "Recommend (t)rack, (a)rtist or a(l)bum?"
  '((?t . shellfm-recommend-track)
    (?a . shellfm-recommend-artist)
    (?l . shellfm-recommend-album))
  "Recommend current track, artist or album to another user.")


;;;; Global state polling and control commands

(defun shellfm-show-status ()
  "Show current Shell.FM status in echo area."
  (interactive)
  (if (eq shellfm-status 'radio)
      (message (concat "Listening to"
                       (propertize shellfm-current-station 'face 'bold)))
    (message (concat "Shell.FM is " (symbol-name shellfm-status)))))

(defun shellfm-pause ()
  "Pause/play current track."
  (interactive)
  (shellfm-command "p")
  (cond ((eq shellfm-status 'radio)
         (shellfm-set-status 'paused))
        ((eq shellfm-status 'paused)
         (shellfm-set-status 'radio))
        (t (error "Not available"))))

(defun shellfm-stop ()
  "Stop playing."
  (interactive)
  (shellfm-command "S")
  (shellfm-set-status 'stopped))


;;;; Global keymap

(defvar shellfm-keymap-prefix "\C-c,")

;;; This code _must_ be rewritten using macros
(let ((shellfm-menu-map (make-sparse-keymap))
      (shellfm-station-menu-map (make-sparse-keymap))
      (shellfm-tag-menu-map (make-sparse-keymap))
      (shellfm-recommend-menu-map (make-sparse-keymap)))
  ;; Tag
  (define-key shellfm-tag-menu-map [shellfm-tag-track]
    '("Track" . shellfm-tag-track))
  (define-key shellfm-tag-menu-map [shellfm-tag-artist]
    '("Artist" . shellfm-tag-artist))
  (define-key shellfm-tag-menu-map [shellfm-tag-album]
    '("Album" . shellfm-tag-album))

  ;; Recommend
  (define-key shellfm-recommend-menu-map [shellfm-recommend-track]
    '("Track" . shellfm-recommend-track))
  (define-key shellfm-recommend-menu-map [shellfm-recommend-artist]
    '("Artist" . shellfm-recommend-artist))
  (define-key shellfm-recommend-menu-map [shellfm-recommend-album]
    '("Album" . shellfm-recommend-album))

  ;; Station
  (define-key shellfm-station-menu-map [shellfm-group]
    '("Group radio" . shellfm-station-radio))
  (define-key shellfm-station-menu-map [shellfm-artist]
    '("Similar to artist" . shellfm-station-artist))
  (define-key shellfm-station-menu-map [shellfm-fans]
    '("Fans" . shellfm-station-fans))
  (define-key shellfm-station-menu-map [shellfm-playlist]
    '("Personal playlist" . shellfm-station-playlist))
  (define-key shellfm-station-menu-map [shellfm-url]
    '("URL" . shellfm-url))
  (define-key shellfm-station-menu-map [shellfm-tag]
    '("Global tag" . shellfm-station-tag))
  (define-key shellfm-station-menu-map [shellfm-recommended]
    '("Recommended tracks" . shellfm-station-recommended))

  ;; General
  (define-key shellfm-menu-map [shellfm-status]
    '("Show Shell.FM status" . shellfm-show-status))
  (define-key shellfm-menu-map [shellfm-track-info]
    '("Show track info" . shellfm-track-info))
  (define-key shellfm-menu-map [shellfm-recommend]
    `("Recommend" . ,shellfm-recommend-menu-map))
  (define-key shellfm-menu-map [shellfm-tag]
    `("Tag" . ,shellfm-tag-menu-map))
  (define-key shellfm-menu-map [shellfm-love]
    '("Love track" . shellfm-love-track))
  (define-key shellfm-menu-map [shellfm-add-playlist]
    '("Add to playlist" . shellfm-add-to-playlist))
  (define-key shellfm-menu-map [shellfm-skip]
    '("Skip track" . shellfm-skip-track))
  (define-key shellfm-menu-map [shellfm-ban]
    '("Ban track" . shellfm-ban-track))
  (define-key shellfm-menu-map [stop]
    '("Stop" . shellfm-stop))
  (define-key shellfm-menu-map [shellfm-pause]
    '("Pause/Play" . shellfm-pause))
  (define-key shellfm-menu-map [shellfm-station]
    `("Switch to station" . ,shellfm-station-menu-map))
  (define-key shellfm-menu-map [shellfm-start]
    '("Launch/kill Shell.FM" . shellfm))
  (define-key-after global-map [menu-bar shellfm] (cons "Shell.FM" shellfm-menu-map)))


;;;; Visible functions

(defun shellfm-running-p ()
  "Return t if shell-fm subprocess is running, nil otherwise."
  (if (not (eq shellfm-status 'dead))
      (if (not (get-process "shell-fm"))
          (progn
            (shellfm-set-status 'dead)
            (error "Shell.FM started, but subprocess is dead!"))
        t)
    nil))

(defun shellfm ()
  "Start or stop shell-fm subprocess."
  (interactive)
  (if (not (shellfm-running-p))
      (progn 
        (let ((sp
               (start-process "shell-fm" nil shellfm-program
                              (concat "-b" shellfm-args) lastfm-default-url)))
          (set-process-filter sp 'shellfm-process-filter))
        (shellfm-set-status 'started))
    (progn
        (delete-process "shell-fm")
        (shellfm-set-status 'dead))))

(provide 'shellfm)

;;; shellfm.el ends here

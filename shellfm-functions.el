;;; shellfm-functions.el --- user-level functions for Emacs Shell.FM interface

;; Copyright (C) 2008 Dmitry Dzhus

;; Author: Dmitry Dzhus <mail@sphinx.net.ru>
;; Keywords: scrobbling audio music last.fm

;; This file is a part of emacs-shellfm.
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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This part of emacs-shellfm is loaded whenever user starts listening
;; session and contains functions to control Shell.FM subprocess.

(require 'shellfm)

;;;; Completion lists

;;; Code:
(defvar shellfm-completion-artists (shellfm-get-top-artists)
  "A list of known artists to be used in minibuffer completion.")

(defvar shellfm-completion-tags (append (shellfm-get-overall-tags)
                                        (shellfm-get-private-tags))
  "A list of known tags to be used in minibuffer completion.")


;;;; Inline functions talking to shell-fm process

(defsubst shellfm-command (string)
  "Send a STRING to the shell-fm subprocess.

Newline is added automagically."

  (process-send-string "shell-fm" (concat string "\n")))

(defsubst shellfm-radio-command (namespace value)
  "Switch to Last.fm radio given its type and value.

Switch to lastfm://NAMESPACE/VALUE radio"
  (shellfm-url (concat namespace "/" value)))


;;;; Radio tuning commands

(defun shellfm-url (url)
  "Switch to given lastfm:// URL.

You may omit lastfm:// part."
  (interactive "slasftm:// URL: ")
  (shellfm-command (concat "r" url)))

(defun shellfm-station-completing (prompt completion-table namespace &optional arg)
  "Prompt for station, if needed and switch to it.

Start a completing interactive prompt given PROMPT text,
COMPLETION-TABLE, and switch to station using its NAMESPACE as in
`shellfm-radio-command'. ARG is station value in
`shellfm-radio-command' used for non-interactive call."
  (let ((real-arg
         (if arg arg
           ;; Override local minibuffer keymap to avoid blocking of
           ;; tags with spaces
           (let ((completion-ignore-case t)
                 (minibuffer-local-completion-map
                  (assq-delete-all 32 minibuffer-local-completion-map)))
             (completing-read prompt completion-table)))))
    (shellfm-radio-command namespace real-arg)))


(defun shellfm-station-tag (&optional tag)
  "Switch to global TAG station.

Several tags separated with comma (like `rock,jazz,vocals`) may
be passed.

This function may be called non-interactively.

This function always returns nil."
  (interactive)
  (shellfm-station-completing "Tag(s): " shellfm-completion-tags "globaltags" tag))

(defun shellfm-station-artist (&optional artist)
  "Switch to similar ARTIST station.

This function may be called non-interactively.

This function always returns nil."
  (interactive)
  (shellfm-station-completing "Artist: " shellfm-completion-artists "artist" artist))

(defun shellfm-station-recommended ()
  "Switch to recommended tracks station."
  (interactive)
  ;; 100 is unknown magic constant. I'm uncertain if this is really an
  ;; obscurity level.
  (shellfm-radio-command "user" (concat lastfm-user "/recommended/100/")))

(defun shellfm-station-group (group)
  "Switch to GROUP station."
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

(defun shellfm-track-info ()
  "Show current track title and artist in echo area."
  (interactive)
  (if (memq shellfm-status '(radio paused))
      (message (format "Currently playing %s — %s"
                       shellfm-current-artist
                       shellfm-current-title))
    (message "Not available.")))


;;;; Global state polling and control commands

(defun shellfm-show-status ()
  "Show current Shell.FM status in echo area."
  (interactive)
  (if (eq shellfm-status 'radio)
      (message (concat "Listening to " shellfm-current-station))
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

(defun shellfm-kill ()
  "Kill shell-fm subprocess and disable all related functions."
  (interactive)
  (delete-process "shell-fm")
  (unload-feature 'shellfm-functions)
  (shellfm-set-status 'dead))

(provide 'shellfm-functions)

;;; shellfm-functions.el ends here

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

;;;; Customization options
;;; Code:
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
  "Extract strings from FILE matching first subexpression of PATTERN.

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
  "Regular expression to match Last.fm tags.")

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
  (setq shellfm-status status))

(defun shellfm-set-track (title artist)
  "Store current track TITLE and ARTIST.

This is a setter function for `shellfm-current-artist' and
`shellfm-current-title' variables."
  (setq shellfm-current-artist artist)
  (setq shellfm-current-title title))

(defun shellfm-set-station (station)
  "Store current STATION.

This is a setter function for `shellfm-current-station'
variable."
  (setq shellfm-current-station station))


;;;; Shell-fm subprocess filtering

(defvar shellfm-nowplaying-regexp "Now playing \"\\(.+\\)\" by \\(.+\\)\."
  "Regular expression to match \"Now playing <track> by <artist>.\" message in shell-fm output.

It must contain two subexpressions, for track and artist
respectively.")

(defvar shellfm-station-regexp "^Receiving \\(.+\\).$"
  "Regular expression to match \"Receiving <station>.\" message in shell-fm output.

This variable must contain one subexpression to match station name.")

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


;;;; Visible functions

(defun shellfm-running-p ()
  "Return t if shell-fm subprocess is running, nil otherwise."
  (not (eq shellfm-status 'dead)))

(defun shellfm ()
  "Start shell-fm subprocess."
  (interactive)
  (if (not (shellfm-running-p))
      (progn (require 'shellfm-functions)
             (let ((sp
                    (start-process "shell-fm" nil shellfm-executable
                                   (concat "-b" shellfm-args) lastfm-default-url)))
               (set-process-filter sp 'shellfm-process-filter))
             (shellfm-set-status 'started))
    (message "Shell.FM is already running")))

(provide 'shellfm)

;;; shellfm.el ends here

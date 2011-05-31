; -*- mode: emacs-lisp; paredit-mode: t; -*-
;;; twitch.el --- Twitter client library.
;;;
;;; Copyright (C) 2011 Vijay Lakshminarayanan
;;;
;;; Author: Vijay Lakshminarayanan <laksvij AT gmail.com>
;;; Version: 0.0.1
;;; Keywords: twitter
;;; Contributors:
;;;
;;; This file is NOT part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to the
;;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.

(require 'twitch)

(defvar *twitching-user-dir*
  (expand-file-name (convert-standard-filename "~/.emacs.d/twitching"))
  "Name of the directory where the user's tweets are stored.")

(defvar *twitching-timer* nil
  "The timer object that keeps getting tweets.")

;;;###autoload
(defun twitching-to-get-my-tweets ()
  (interactive)
  (labels ((mintos (min) (print (* min 60))))
    (if *twitching-timer*
        (message "Timer already running.")
      (setq *twitching-timer*
            (run-with-timer 0
                            300
                            #'twitching-get-home-timeline)))))

(defun twitching-get-home-timeline ()
  "Fetch home timelines."
  (let ((twitching-buffer (get-buffer-create "*Twitching*"))
        (endl-regex (apply #'concat (mapcar #'char-to-string '(?\[ 10 13 ?\])))))
    (with-current-buffer twitching-buffer
      (goto-char (point-max))
      (mapcar (lambda (tweet)
                (let ((tweet (replace-regexp-in-string endl-regex
                                                       " "
                                                       (format "%S" tweet))))
                  (insert (concat tweet "\n"))))
              (twitch-get-home-timeline))))
  (message "retrieved tweets"))

(defun twitching-stop ()
  "Stop the timer that fetches tweets."
  (interactive)
  (if *twitching-timer*
    (progn
      (cancel-timer *twitching-timer*)
      (setq *twitching-timer* nil)
      (message "Stopped twitching timer."))
    (message "twitching timer not running.")))


;;; Define `twitching-mode'

(defvar *twitching-current-tweet* nil
  "Dynamic variable used to hold the current tweet.")

;; keymap

(defvar *twitching-keymap*
  (let ((keymap (make-keymap))
        (nodigits t))
    (suppress-keymap keymap nodigits)
    (define-key keymap (kbd "n")   'twitching-next-tweet)
    (define-key keymap (kbd "C-n") 'twitching-next-tweet)
    (define-key keymap (kbd "p")   'twitching-prev-tweet)
    (define-key keymap (kbd "C-p") 'twitching-prev-tweet)
    (define-key keymap (kbd "f")   'twitching-create-filter)
    ))

;;; overlay
(defun twitching-overlay-on-line ()
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (twitching-overlay start end)))

(defvar *twitching-top-line-category*
  (put '*twitching-top-line-category* 'face '((:weight bold)
                                              (:slant italic)
                                              (:background "ivory3")
                                              (:foreground "chocolate"))))

(defvar *twitching-status-line-category*
  (put '*twitching-status-line-category* 'face '((:weight normal)
                                                 (:background "#EEEEEE")
                                                 (:foreground "#222222"))))

(defun twitching-overlay (start end)
  "Renders the line as a tweet specified by the region in START
  and END."
  (let* ((tweet-str (buffer-substring-no-properties start end))
         (status (read tweet-str))
         (text (twitch-twitter-status-text status))
         (created-at (twitch-twitter-status-created-at status))
         (user (twitch-twitter-status-user status))
         (screen-name (twitch-twitter-user-screen-name user))
         (user-name (twitch-twitter-user-name user))
         (entity (twitch-twitter-status-entities status))
         (urls (twitch-twitter-entity-urls entity))
         (overlay (make-overlay start end))
         (line1 (propertize (concat screen-name " | " user-name " | " created-at)
                            'category '*twitching-top-line-category*))
         (line2 (propertize (concat "\n" text)
                           'category '*twitching-status-line-category*))
         (display (concat line1 line2)))
    (overlay-put overlay 'tweet status)
    (overlay-put overlay 'display display)))

;;; mode interactive functions

(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (let* ((overlays (overlays-at (point)))
         (twt-ovrlys (mapcar (lambda (o) (overlay-get o 'tweet)) overlays))
         (tweet (find t twt-ovrlys :test (lambda (x y) (and x y)))))
    (unless tweet
      (mapcar #'delete-overlay overlays)
      (twitching-overlay-on-line))
    (goto-line (+ (line-number-at-pos) n))))

(defun twitching-prev-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-next-tweet (- n)))

(defun twitching-create-filter ()
  "Create a twitter filter."
  (interactive))

;;; twitching.el ends here

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
  "Start timer to fetch home timeline."
  (interactive)
  (labels ((mintos (min) (print (* min 60))))
    (if *twitching-timer*
        (message "Timer already running.")
      (setq *twitching-timer*
            (run-with-timer 0
                            300
                            #'twitching-get-home-timeline)))))

(defun twitching-get-home-timeline ()
  "Fetch home timeline."
  (interactive)
  (let ((twitching-buffer (get-buffer-create "*Twitching*")))
    (with-current-buffer twitching-buffer
      (goto-char (point-max))
      (mapcar (lambda (tweet)
                (let ((tweet (replace-regexp-in-string "[\r\n]"
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
                                              (:background "white")
                                              (:foreground "chocolate"))))

(defvar *twitching-status-line-category*
  (put '*twitching-status-line-category* 'face '((:weight normal)
                                                 (:background "white")
                                                 (:foreground "red4"))))

(defvar *twitching-plaintext-category*
  (put '*twitching-plaintext-category* 'face '((:background "white")
                                               (:foreground "black"))))

(defvar *twitching-hashtags-category*
  (put '*twitching-hashtags-category* 'face '((:background "ivory3")
                                             (:foreground "firebrick")
                                             (:slant italic))))

(defvar *twitching-mentions-category*
  (put '*twitching-mentions-category* 'face '((:background "thistle2")
                                              (:foreground "DarkGreen")
                                              (:weight bold))))

(defvar *twitching-urls-category*
  (put '*twitching-urls-category* 'face '((:background "white")
                                          (:foreground "blue1")
                                          (:slant italic)
                                          (:underline t))))

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
         (overlay (make-overlay start end))
         (sep " | ")
         (line1 (propertize (concat screen-name sep user-name sep created-at)
                            'category '*twitching-top-line-category*))
         (line2 (twitching-decorate-status-text status))
         (display (concat line1 "\n" line2)))
    (overlay-put overlay 'tweet status)
    (overlay-put overlay 'display display)))

(defun twitching-decorate-status-text (status)
  "Decorates the status text."
  (let* ((text (twitch-twitter-status-text status))
         (entity (twitch-twitter-status-entities status))
         (urls (twitch-twitter-entity-urls entity))
         (mentions (twitch-twitter-entity-user-mentions entity))
         (hashtags (twitch-twitter-entity-hashtags entity))
         (properties '((hashtag . *twitching-hashtags-category*)
                       (mention . *twitching-mentions-category*)
                       (url . *twitching-urls-category*)
                       (text . *twitching-plaintext-category*)))
         (fn (lambda (list type)
               (mapcar (lambda (x) `(,@(cdr (assoc 'indices x)) ,type)) list)))
         (hashtags (funcall fn hashtags 'hashtag))
         (mentions (funcall fn mentions 'mention))
         (urls (funcall fn urls 'url))
         (indices (sort (append hashtags mentions urls)
                        (lambda (idx1 idx2)
                          (< (second idx1) (second idx2)))))
         (max (length text))
         (ci 0)
         result)
    (dolist (idx indices result)
      (when (/= ci (car idx)) (push `(,ci ,(car idx) text) result))
      (push idx result)
      (setq ci (cadr idx)))
    (when (> max ci) (push (list ci max 'text) result))
    (setq result
          (mapcar (lambda (idx)
                    (let* ((txt (substring text (first idx) (second idx)))
                           (type (third idx)))
                      (propertize txt 'category (cdr (assoc type properties)))))
           (reverse result)))
    (apply #'concat result)))

;;; mode interactive functions
(defun twitching-next-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (flet ((layout ()
           (let* ((overlays (overlays-at (point)))
                  (fn (lambda (o) (overlay-get o 'tweet)))
                  (twt-ovrlys (mapcar fn overlays))
                  (tweet (find t twt-ovrlys :test (lambda (x y) (and x y)))))
             (when (and (not tweet) (not (eobp)))
               (mapcar #'delete-overlay overlays)
               (twitching-overlay-on-line)))))
    (layout)
    (let ((pos (plusp n))
          (n (abs n)))
      (dotimes (i n)
        (goto-line (funcall (if pos #'1+ #'1-) (line-number-at-pos)))
        (layout)))))

(defun twitching-prev-tweet (n)
  "Move down N tweets."
  (interactive "p")
  (twitching-next-tweet (- n)))

(defun twitching-create-filter ()
  "Create a twitter filter."
  (interactive))

;;; twitching.el ends here

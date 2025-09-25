;;; km-autorevert.el --- Automatically revert buffers when switching windows -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/km-autorevert
;; Version: 0.1.0
;; Keywords: files
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically revert buffers when switching windows or buffers.

;;; Code:

(require 'autorevert)

(defcustom km-autorevert-switch-buffer-hook '(km-autorevert-auto-revert-buffer)
  "A list of hooks run after changing the current buffer."
  :type 'hook
  :group 'autorevert)

(defcustom km-autorevert-switch-window-hook '(km-autorevert-auto-revert-buffer)
  "A list of hooks run after changing the focused windows."
  :type 'hook
  :group 'autorevert)

(defcustom km-autorevert-switch-frame-hook '(km-autorevert-auto-revert-buffers-h)
  "Hook triggered when switching frames to auto-revert buffers.

A hook that runs functions when switching frames to automatically
revert buffers. This hook is triggered when a frame gains focus,
allowing for automatic buffer updates.

Functions added to this hook should handle buffer reversion tasks
to ensure that the content displayed is up-to-date."
  :type 'hook
  :group 'autorevert)

(defcustom km-autorevert--switch-frame-hook-debounce-delay 2.0
  "The delay for which `km-autorevert-switch-frame-hook' won't trigger again.

This exists to prevent switch-frame hooks getting triggered too aggressively."
  :group 'autorevert
  :type 'float)

(defun km-autorevert-auto-revert-buffer ()
  "Enable auto-revert mode unless certain conditions are met."
  (unless (or (bound-and-true-p auto-revert-mode)
              (active-minibuffer-window)
              (and buffer-file-name
                   auto-revert-remote-files
                   (file-remote-p buffer-file-name nil t)))
    (let ((auto-revert-mode t))
      (auto-revert-handler))))

(defun km-autorevert-auto-revert-buffers-h ()
  "Auto revert stale buffers in visible windows, if necessary."
  (dolist (buf (km-autorevert--visible-buffers))
    (with-current-buffer buf
      (km-autorevert-auto-revert-buffer))))

(defun km-autorevert--visible-buffers (&optional buffer-list all-frames)
  "Return visible buffers, optionally filtering by BUFFER-LIST and ALL-FRAMES.

Optional argument BUFFER-LIST is a list of buffers to filter the visible buffers
against.

Optional argument ALL-FRAMES, if non-nil, includes buffers from all visible
frames."
  (let ((buffers
         (delete-dups
          (cl-loop for frame in (if all-frames (visible-frame-list)
                                  (list (selected-frame)))
                   if (window-list frame)
                   nconc (mapcar #'window-buffer it)))))
    (if buffer-list
        (cl-loop for buf in buffers
                 unless (memq buf buffer-list)
                 collect buffers)
      buffers)))

(defun km-autorevert-run-switch-buffer-hooks-h (&optional _)
  "Trigger `km-autorevert-switch-buffer-hook' when selecting a new buffer."
  (let ((gc-cons-threshold most-positive-fixnum))
    (run-hooks 'km-autorevert-switch-buffer-hook)))

(defun km-autorevert-run-switch-window-hooks-h (&optional _)
  "Run hooks after switching windows unless in minibuffer or same frame.

List of hooks are specified in `km-autorevert-switch-window-hook'."
  (unless (or (minibufferp)
              (not (equal (old-selected-frame)
                          (selected-frame)))
              (equal (old-selected-window)
                     (minibuffer-window)))
    (let ((gc-cons-threshold most-positive-fixnum))
      (run-hooks 'km-autorevert-switch-window-hook))))

(defun km-autorevert--run-switch-frame-hooks-fn (_)
  "Remove the hook and run frame switch hooks if debounce delay passed."
  (remove-hook 'pre-redisplay-functions
               #'km-autorevert--run-switch-frame-hooks-fn)
  (let ((gc-cons-threshold most-positive-fixnum))
    (dolist (fr (visible-frame-list))
      (let ((state (frame-focus-state fr)))
        (when (and state (not (eq state 'unknown)))
          (let ((last-update (frame-parameter fr 'km-autorevert-last-focus)))
            (when (or (null last-update)
                      (> (float-time (time-subtract (current-time) last-update))
                         km-autorevert--switch-frame-hook-debounce-delay))
              (with-selected-frame fr
                (unwind-protect
                    (let ((inhibit-redisplay t))
                      (run-hooks 'km-autorevert-switch-frame-hook))
                  (set-frame-parameter fr 'km-autorevert-last-focus
                                       (current-time)))))))))))

(let (last-focus-state)
  (defun km-autorevert-run-switch-frame-hooks-fn ()
    "Trigger `km-autorevert-switch-frame-hook' once per frame focus change."
    (or (equal last-focus-state
               (setq last-focus-state
                     (mapcar #'frame-focus-state (frame-list))))
        (add-hook 'pre-redisplay-functions
                  #'km-autorevert--run-switch-frame-hooks-fn))))

;;;###autoload
(define-minor-mode km-autorevert-mode
  "Automatically revert buffers when switching windows or buffers.

Enable automatic buffer reversion when certain events occur, such as switching
windows or buffers, and after saving files.

This mode ensures that buffers in visible windows are up-to-date by running
specified hooks when these events happen."
  :group 'files
  :global t
  (remove-hook 'window-selection-change-functions
               #'km-autorevert-run-switch-window-hooks-h)
  (remove-hook 'window-buffer-change-functions
               #'km-autorevert-run-switch-buffer-hooks-h)
  (remove-hook 'server-switch-hook
               #'km-autorevert-run-switch-buffer-hooks-h)
  (remove-hook 'after-save-hook
               #'km-autorevert-auto-revert-buffers-h)
  (when km-autorevert-mode
    (when (fboundp 'km-autorevert-run-switch-frame-hooks-fn)
      (add-function :after after-focus-change-function
                    #'km-autorevert-run-switch-frame-hooks-fn))
    (add-hook 'window-selection-change-functions
              #'km-autorevert-run-switch-window-hooks-h)
    (add-hook 'window-buffer-change-functions
              #'km-autorevert-run-switch-buffer-hooks-h)
    (add-hook 'server-switch-hook #'km-autorevert-run-switch-buffer-hooks-h)
    (add-hook 'after-save-hook #'km-autorevert-auto-revert-buffers-h)))

(provide 'km-autorevert)
;;; km-autorevert.el ends here

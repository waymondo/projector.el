;;; projector.el --- Lightweight library for managing project/repository-aware shell and command buffers
;;
;; Copyright 2013-2014 Justin Talbott
;;
;; Author: Justin Talbott <justin@waymondo.com>
;; URL: https://github.com/waymondo/projector
;; Version: 0.2.0
;; Package-Requires: ((alert "1.1") (projectile "0.11.0"))
;;
;;; Commentary:
;;
;;; Installation:
;;
;;   (require 'projector)
;;   (setq alert-default-style 'notifier) ; for background alerts
;;   (setq projector-always-background-regex '("^mysql.server\\.*" "^powder\\.*"))
;;
;;; Code:

(require 'cl)
(require 'alert)

(defcustom projector-always-background-regex '()
  "A list of regex patterns for shell commands to always run in the background."
  :type 'list
  :group 'projector)

(defvar projector-buffer-prefix "projector: "
  "Prefix for all projector-created buffers.")

(defalias 'projector-command-history 'shell-command-history)

(declare-function ido-complete-space "ido")
(defvar projector-ido-no-complete-space nil
  "Advice flag to allow space to insert actual space with `ido' completion.")
(defadvice ido-complete-space (around ido-insert-space activate)
  "Allow space on keyboard to insert space when `ido'-ing shell commands."
  (if projector-ido-no-complete-space
      (insert " ")
    ad-do-it))

(defun projector-shell-buffer-name ()
  (concat "*" projector-buffer-prefix (projectile-project-name) "*"))

(defun projector-shell-command-buffer-name (cmd)
  (concat "*" projector-buffer-prefix (projectile-project-name) " " cmd "*"))

(defun projector-shell-command-output-title (process msg)
  (concat (process-name process) " - " msg))

(defun projector-string-match-pattern-in-list (str lst)
  (consp (memq t (mapcar (lambda (s) (numberp (string-match s str))) lst))))

(defun projector-make-shell ()
  (with-temp-buffer
    (cd (projectile-project-root))
    (shell (projector-shell-buffer-name))
    (get-buffer (projector-shell-buffer-name))))

(defun projector-output-message-kill-buffer-sentinel (process msg)
  (when (memq (process-status process) '(exit signal))
    (alert (with-current-buffer (get-buffer (process-buffer process)) (buffer-string))
           :title (projector-shell-command-output-title process msg))
    (kill-buffer (process-buffer process))))

(defun projector-async-shell-command-get-buffer ()
  (let ((command-buffer-name (projector-shell-command-buffer-name cmd)))
    (async-shell-command cmd command-buffer-name)
    (get-buffer command-buffer-name)))

(defun projector-run-command-buffer (in-current-directory notify-on-exit dir-string)
  (let* ((projector-ido-no-complete-space t)
         (cmd (completing-read (concat "Shell command (" dir-string "): ")
                               (delete-duplicates projector-command-history :test #'equal) nil nil nil
                               'projector-command-history
                               (car projector-command-history))))
    (if (or notify-on-exit (projector-string-match-pattern-in-list cmd projector-always-background-regex))
        (with-temp-buffer
          (unless in-current-directory (cd (projectile-project-root)))
          (set-process-sentinel (start-process-shell-command cmd cmd cmd) #'projector-output-message-kill-buffer-sentinel))
      (switch-to-buffer
       (save-window-excursion
         (unless in-current-directory (cd (projectile-project-root)))
         (projector-async-shell-command-get-buffer))))))

;;;###autoload
(defun projector-run-shell-command-project-root (&optional notify-on-exit)
  "Execute command from minibuffer at the projector root.
By default, it outputs into a dedicated buffer.
With the optional argument NOTIFY-ON-EXIT, execute command in the background
and send the exit message as a notification."
  (interactive "P")
  (let ((dir-string (concat (projectile-project-name) " root")))
    (projector-run-command-buffer nil (consp notify-on-exit) dir-string)))

;;;###autoload
(defun projector-run-shell-command-project-root-background ()
  "Execute command from minibuffer at the projector root in the background.
Sends the exit message as a notification."
  (interactive)
  (let ((dir-string (concat (projectile-project-name) " root")))
    (projector-run-command-buffer nil t dir-string)))

;;;###autoload
(defun projector-run-shell-command-current-directory (&optional notify-on-exit)
  "Execute command from minibuffer in the current directory.
By default, it outputs into a dedicated buffer.
With the optional argument NOTIFY-ON-EXIT, execute command in the background
and send the exit message as a notification."
  (interactive "P")
  (let ((dir-string "current-directory"))
    (projector-run-command-buffer t (consp notify-on-exit) dir-string)))

;;;###autoload
(defun projector-run-shell-command-current-directory-background ()
  "Execute command from minibuffer in the current directory.
Sends the exit message as a notification."
  (interactive)
  (let ((dir-string "current-directory"))
    (projector-run-command-buffer t t dir-string)))

(defun projector-is-shell-buffer-name (buf-name)
  (when (and (>= (length buf-name) (length string-to-match))
             (string-equal (substring buf-name 0 (length string-to-match)) string-to-match))
    buf-name))

(defun projector-shell-buffers ()
  (save-excursion
    (delq
     nil
     (mapcar (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (and (eq major-mode 'shell-mode)
                        (buffer-name buf)
                        (projector-is-shell-buffer-name (buffer-name buf))
                        ))))
             (buffer-list)))))


;;;###autoload
(defun projector-switch-to-or-create-project-shell ()
  "Find or create a dedicated shell for the current repository."
  (interactive)
  (switch-to-buffer
   (or (get-buffer (projector-shell-buffer-name))
       (save-window-excursion (projector-make-shell))))
  (end-of-buffer))

;;;###autoload
(defun projector-open-project-shell ()
  "Use `completing-read' to find or create a project shell for a repository."
  (interactive)
  (let ((project-path (completing-read "Open projector shell: " projectile-known-projects)))
    (with-temp-buffer
      (cd project-path)
      (shell (projector-shell-buffer-name)))))

;;;###autoload
(defun projector-switch-to-shell-buffer ()
  "Use `completing-read' to switch to an open projector shell buffer."
  (interactive)
  (let ((string-to-match (concat "*" projector-buffer-prefix)))
    (switch-to-buffer
     (completing-read "Projector Shell Buffer: " (projector-shell-buffers)))))

;;;###autoload
(defun projector-switch-to-shell-buffer-in-project ()
  "Use `completing-read' to switch to an open projector shell buffer in the current repository."
  (interactive)
  (let ((string-to-match (substring (projector-shell-buffer-name) 0 -1)))
    (switch-to-buffer
     (completing-read "Projector Shell Buffer: " (projector-shell-buffers)))))

(provide 'projector)
;;; projector.el ends here

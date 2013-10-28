;;;; projector.el --- a lightweight Emacs library for managing project/repository-aware shell and shell command buffers.
;;
;; Copyright 2013 Justin Talbott
;;
;; Author: Justin Talbott <justin@waymondo.com>
;; URL: https://github.com/waymondo/projector
;; Version: 0.1.0
;; Package-Requires:
;;
;; Example Installation:
;;
;;   (require 'projector)
;;   (setq projector-projects-root "~/code/")
;;

(eval-when-compile (require 'cl))
(autoload 'vc-git-root "vc-git")
(autoload 'vc-svn-root "vc-svn")
(autoload 'vc-hg-root "vc-hg")

(defcustom projector-projects-root nil
  "The root folder that your code repositories reside in."
  :type 'string
  :group 'projector)

;; internal settings
(defvar projector-buffer-prefix "projector: ")
(defalias 'projector-command-history 'shell-command-history)

(require 'ido)
(defvar projector-ido-no-complete-space nil)
(defadvice ido-complete-space (around ido-insert-space activate)
  "Allow space on keyboard to insert space when ido-ing shell commands"
  (if projector-ido-no-complete-space
      (insert " ")
    ad-do-it))

(defun projector-find-root ()
  "Guess the projector root of the given FILE-PATH."
  (or (vc-git-root default-directory)
      (vc-svn-root default-directory)
      (vc-hg-root default-directory)
      default-directory))

(defun projector-project-name ()
  (concat (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1" (projector-find-root)) ""))

(defun projector-shell-buffer-name ()
  (concat "*" projector-buffer-prefix (projector-project-name) "*"))

(defun projector-shell-command-buffer-name (cmd)
  (concat "*" projector-buffer-prefix (projector-project-name) " " cmd "*"))

(defun projector-shell-command-output-title (process msg)
  (concat (process-name process) " - " msg))

(defun projector-make-shell ()
  (with-temp-buffer
    (cd (projector-find-root))
    (shell (projector-shell-buffer-name))
    (get-buffer (projector-shell-buffer-name))))

(defun projector-notify (title message)
  "Send shell command exit notification to `terminal-notifier', `growlnotify', or Emacs (message)"
  (if (executable-find "terminal-notifier")
      (projector-notify-terminal-notifier title message)
    (if (executable-find "growlnotify")
        (projector-notify-growl title message)
      (message title))))

(defun projector-notify-growl (title message)
  (start-process "projector-notify" " projector-notify" "growlnotify" title "-a" "Emacs")
  (process-send-string " projector-notify" message)
  (process-send-eof " projector-notify"))

(defun projector-notify-terminal-notifier (title message)
  (start-process "projector-notify" " projector-notify" "terminal-notifier"
                 "-message" message "-title" title "-activate" "org.gnu.Emacs")
  (process-send-eof " projector-notify"))

(defun projector-output-message-kill-buffer-sentinel (process msg)
  (when (memq (process-status process) '(exit signal))
    (projector-notify (projector-shell-command-output-title process msg)
                      (with-current-buffer (get-buffer (process-buffer process)) (buffer-string)))
    (kill-buffer (process-buffer process))))

(defun projector-async-shell-command-get-buffer ()
  (let ((command-buffer-name (projector-shell-command-buffer-name cmd)))
    (async-shell-command cmd command-buffer-name)
    (get-buffer command-buffer-name)))

(defun projector-run-command-buffer (in-current-directory notify-on-exit dir-string)
  (let* ((projector-ido-no-complete-space t)
         (cmd (ido-completing-read (concat "Shell command (" dir-string "): ")
                                   (delete-duplicates projector-command-history :test #'equal) nil nil nil
                                   'projector-command-history
                                   (car projector-command-history))))
    (if notify-on-exit
        (with-temp-buffer
          (unless in-current-directory (cd (projector-find-root)))
          (set-process-sentinel (start-process-shell-command cmd cmd cmd) #'projector-output-message-kill-buffer-sentinel))
      (switch-to-buffer
       (save-window-excursion
         (unless in-current-directory (cd (projector-find-root)))
         (projector-async-shell-command-get-buffer))))))

;;;###autoload
(defun projector-run-shell-command-project-root (&optional arg)
  "Execute command from minibuffer at the projector root.
  By default, it outputs into a dedicated buffer.
  `C-u' prefix - execute command in the background
  and send the exit message to `terminal-notifier', `growlnotify', or Emacs (message)."
  (interactive "P")
  (let ((dir-string (concat (projector-project-name) " root")))
    (projector-run-command-buffer nil (consp arg) dir-string)))

;;;###autoload
(defun projector-run-shell-command-project-root-background ()
  "Execute command from minibuffer at the projector root in the background
  and send the exit message to `terminal-notifier', `growlnotify', or Emacs (message)."
  (interactive)
  (let ((dir-string (concat (projector-project-name) " root")))
    (projector-run-command-buffer nil t dir-string)))

;;;###autoload
(defun projector-run-shell-command-current-directory (&optional arg)
  "Execute command from minibuffer in the current directory
  By default, it outputs into a dedicated buffer.
  `C-u' prefix - execute command in the background
  and send the exit message to `terminal-notifier', `growlnotify', or Emacs (message)."
  (interactive "P")
  (let ((dir-string "current-directory"))
    (projector-run-command-buffer t (consp arg) dir-string)))

;;;###autoload
(defun projector-run-shell-command-current-directory-background ()
  "Execute command from minibuffer in the current directory
  and send the exit message to `terminal-notifier', `growlnotify', or Emacs (message)."
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
       (save-window-excursion (projector-make-shell)))))

;;;###autoload
(defun projector-open-project-shell ()
  "Use `ido-completing-read' to find or create a project shell for a repository."
  (interactive)
  (let ((project-name (ido-completing-read "Open projector shell: " (directory-files projector-projects-root nil "^[^.]"))))
    (with-temp-buffer
      (cd (concat projector-projects-root project-name))
      (shell (projector-shell-buffer-name)))))

;;;###autoload
(defun projector-switch-to-shell-buffer ()
  "Use `ido-completing-read' to switch to an open projector shell buffer."
  (interactive)
  (let ((string-to-match (concat "*" projector-buffer-prefix)))
    (switch-to-buffer
     (ido-completing-read "Projector Shell Buffer: " (projector-shell-buffers)))))

;;;###autoload
(defun projector-switch-to-shell-buffer-in-project ()
  "Use `ido-completing-read' to switch to an open projector shell buffer in the current repository."
  (interactive)
  (let ((string-to-match (substring (projector-shell-buffer-name) 0 -1)))
    (switch-to-buffer
     (ido-completing-read "Projector Shell Buffer: " (projector-shell-buffers)))))

(provide 'projector)

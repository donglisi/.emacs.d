(load "~/.emacs.d/27.2/minibuffer.el")
(load "~/.emacs.d/27.2/simple.el")
(load "~/.emacs.d/27.2/compile.el")

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'fzf)
(require 'anzu)
(require 'goto-last-change)
(require 'ggtags)
(require 'google-translate)
(require 'sudo-save)

(menu-bar-mode 0)
(global-font-lock-mode 0)
(global-eldoc-mode 0)
(savehist-mode)
(tool-bar-mode 0)
(electric-indent-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(global-anzu-mode)
(blink-cursor-mode 0)
(show-paren-mode)
(delete-selection-mode)
(global-google-translate-mode)

(setq inhibit-startup-screen t)
(setq auto-save-list-file-prefix nil)
(setq lazy-highlight-initial-delay 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq mouse-drag-copy-region t)
(setq read-file-name-completion-ignore-case t)
(setq next-error-highlight nil)
(setq ggtags-highlight-tag nil)
(setq scroll-step 1)

(setq-default mouse-1-click-follows-link nil)
(setq-default enable-recursive-minibuffers t)
(setq-default explicit-shell-file-name "~/.local/bin/bashn")
(setq-default mode-line-format (list '(:eval (if (buffer-file-name) "%f" "%b")) " (%l %C)"))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(setenv "MANWIDTH" "192")

(defun kill-all-buffers () (interactive) (mapc 'kill-buffer (buffer-list)))

(defun eval-region-unmark (beg end)
  (interactive "r")
  (eval-region beg end)
  (deactivate-mark))

(defvar killed-file-list nil)
(defun add-file-to-killed-file-list ()
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))
(add-hook 'kill-buffer-hook 'add-file-to-killed-file-list)
(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (interactive)
  (when killed-file-list
    (find-file (pop killed-file-list))))

(defun fzfk ()
  (interactive)
  (let ((default-directory "/home/d/linux"))
    (fzf-find-file)))

(defun find-file-set-key ()
  (interactive)
  (local-set-key (kbd "TAB") (lambda () (interactive (progn (minibuffer-complete) (minibuffer-completion-help)))))
  (local-set-key (kbd "M-DEL") (lambda () (interactive) (backward-kill-word 1) (minibuffer-completion-help)))
  (local-set-key (kbd "M-\\") (lambda () (interactive)
    (let ((path (minibuffer-contents)))
      (delete-minibuffer-contents)
      (insert (replace-regexp-in-string "/[^/]*/?$" "/" path)))
    (minibuffer-completion-help)))
  (local-set-key (kbd "/") (lambda () (interactive) (insert "/") (minibuffer-completion-help)))
  (minibuffer-completion-help))

(defun find-file-root ()
  (interactive)
  (let ((inhibit-message t) (default-directory "/"))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))

(defun find-file-dir ()
  (interactive)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))

(defun find-file-home ()
  (interactive)
  (let ((inhibit-message t) (default-directory "~/"))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))

(defun isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (goto-char isearch-other-end))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward)))

(defun isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward)))

(defun imenu-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-complete (call-interactively 'imenu)))

(defun switch-buffer-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-complete (call-interactively 'switch-to-buffer)))

(defun kill-current-buffer ()
   (interactive)
   (kill-buffer (current-buffer)))

(defun search-selection (beg end)
      (interactive "r")
      (kill-ring-save beg end)
      (isearch-mode t nil nil nil)
      (isearch-yank-pop))

(defun highlight-toggle ()
  (interactive)
  (let ((str (get-char-property (point) 'hi-lock-overlay-regexp)))
     (if str (hi-lock-unface-buffer str) (hi-lock-face-symbol-at-point))))

(defun mouse-highlight-toggle (click)
  (interactive "e")
  (mouse-set-point click)
  (highlight-toggle))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(add-hook 'Man-mode-hook 'delete-window)

(add-hook 'ggtags-global-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") 'compile-goto-error)
    (local-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)))

(add-hook 'completion-list-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") 'choose-completion)
    (local-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)))

(add-hook 'c-mode-common-hook
  (lambda ()
    (display-line-numbers-mode)
    (local-set-key (kbd "<double-down-mouse-1>") (lambda ()(interactive)))
    (local-set-key (kbd "<double-mouse-1>") 'ggtags-find-tag-dwim)
    (local-set-key (kbd "<mouse-2>") 'xref-pop-marker-stack)
    (local-set-key (kbd "M-n") 'next-error)
    (local-set-key (kbd "M-p") 'previous-error)
    (local-set-key (kbd "<mouse-8>") 'next-error)
    (local-set-key (kbd "<mouse-9>") 'previous-error)
    (when (derived-mode-p 'c-mode 'asm-mode) (ggtags-mode))))

(define-key isearch-mode-map (kbd "C-s") 'isearch-repeat-forward+)
(define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward+)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x M-f") 'find-file-root)
(global-set-key (kbd "<f3>") 'search-selection)
(global-set-key (kbd "<f4>") (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "<f5>") (lambda () (interactive) (buffer-disable-undo) (buffer-enable-undo) (message "reset-undo")))
(global-set-key (kbd "<f10>") 'tmm-menubar)
(global-set-key (kbd "<f11>") 'count-lines-page)
(global-set-key (kbd "<f12>") 'kill-current-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-\\") 'goto-last-change)
(global-set-key (kbd "C-x f") 'find-file-home)
(global-set-key (kbd "C-x C-t") 'fzfk)
(global-set-key (kbd "C-x C-f") 'find-file-dir)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-k") 'kill-all-buffers)
(global-set-key (kbd "C-x C-r") 'reopen-killed-file)
(global-set-key (kbd "M-e") 'eval-region-unmark)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-m") 'man)
(global-set-key (kbd "M-c") 'shell-command)
(global-set-key (kbd "M-i") 'imenu-completion)
(global-set-key (kbd "C-x b") 'switch-buffer-completion)
(global-set-key (kbd "C-M-l") (lambda () (interactive) (recenter-top-bottom -1)))
(global-set-key (kbd "C-M-r") (lambda () (interactive) (move-to-window-line-top-bottom -1)))
(global-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)
(global-set-key (kbd "<mouse-3>") 'mouse-highlight-toggle)
(global-set-key (kbd "<mouse-6>") 'switch-buffer-completion)
(global-set-key (kbd "<mouse-7>") 'imenu-completion)
(global-set-key (kbd "TAB") (lambda () (interactive) (insert "\t")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 128 :width normal))))
 '(line-number ((t (:foreground "black"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-inactive ((t (:background "grey75" :foreground "black" :weight light)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hi-lock-file-patterns-policy 'never)
 '(imenu-use-popup-menu nil)
 '(max-mini-window-height 0.9))

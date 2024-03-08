(load "~/.emacs.d/lisp/buff-menu.el")
(load "~/.emacs.d/lisp/hi-lock.el")
(load "~/.emacs.d/lisp/simple.el")
(load "~/.emacs.d/lisp/minibuffer.el")
(require 'which-func)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'goto-last-change)
(require 'wo-ctrl-c)
(require 'origin-point-position)
(require 'ggtags)
(require 'fzf)
(require 'isearch-plus)
(require 'minibuffer-find-file)

(menu-bar-mode 0)
(global-font-lock-mode 0)
(global-eldoc-mode 0)
(savehist-mode)
(tool-bar-mode 0)
(electric-indent-mode 0)
(scroll-bar-mode 0)
(fringe-mode 0)
(blink-cursor-mode 0)
(show-paren-mode)
(save-place-mode)
(delete-selection-mode)
(global-anzu-mode)

(setq ring-bell-function 'ignore)
(setq show-help-function nil)
(setq inhibit-startup-screen t)
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq mouse-drag-copy-region t)
(setq read-file-name-completion-ignore-case t)
(setq scroll-step 1)

(setq-default frame-title-format '("emacs"))
(setq-default mouse-1-click-follows-link nil)
(setq-default enable-recursive-minibuffers t)
(setq-default mode-line-format (list '(:eval (if (buffer-file-name) "%f" "%b")) " (%p %l %C) " '(:eval
  (when line-number-mode (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count) my-mode-line-buffer-line-count)))))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f1>") (lambda () (interactive) (unhighlight-regexp t)))
(global-set-key (kbd "<f2>") 'buffer-list-toggle)
(global-set-key (kbd "<f3>") 'save-buffer)
(global-set-key (kbd "<f5>") (lambda () (interactive) (buffer-disable-undo) (buffer-enable-undo) (message "reset undo")))
(global-set-key (kbd "<f6>") (lambda () (interactive) (bury-buffer (current-buffer)) (switch-buffer-toggle)))
(global-set-key (kbd "<f9>") 'count-lines-page)
(global-set-key (kbd "TAB") (lambda () (interactive) (insert "\t")))
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c f") (lambda () (interactive) (message (which-function))))
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-m") 'man)
(global-set-key (kbd "C-M-l") (lambda () (interactive) (recenter-top-bottom -1)))
(global-set-key (kbd "C-M-r") (lambda () (interactive) (move-to-window-line-top-bottom -1)))
(global-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)
(global-set-key "\S-\M-p" "\C-u1\C-v")
(global-set-key "\S-\M-n" "\C-u1\M-v")
(global-set-key (kbd "<prior>") 'scroll-down-command)
(global-set-key (kbd "<next>") 'scroll-up-command)
(global-set-key (kbd "C-v") (lambda () (interactive) (scroll-up-command 22)))
(global-set-key (kbd "M-v") (lambda () (interactive) (scroll-down-command 22)))
(global-set-key (kbd "C-M-u") 'upcase-region)
(global-set-key (kbd "C-M-d") 'downcase-region)

(defun eval-region-unmark (beg end)
  (interactive "r")
  (eval-region beg end)
  (deactivate-mark))
(global-set-key (kbd "M-e") 'eval-region-unmark)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect (progn ,@body) (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column (transpose-lines 1) (forward-line -2)))
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key [M-up] 'move-line-up)

(defun move-line-down ()
  (interactive)
  (save-column (forward-line 1) (transpose-lines 1) (forward-line -1)))
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key [M-down] 'move-line-down)

(defun kill-current-buffer () (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(defun kill-all-buffers () (interactive) (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k") 'kill-all-buffers)

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x C-o") 'kill-other-buffers)

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
(global-set-key (kbd "C-x C-r") 'reopen-killed-file)

(defun switch-buffer-toggle ()
  (interactive)
  (switch-to-buffer nil))
(global-set-key (kbd "<mouse-9>") 'switch-buffer-toggle)
(global-set-key (kbd "<f4>") 'switch-buffer-toggle)

(defun imenu-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-completion-help (call-interactively 'imenu)))
(global-set-key (kbd "M-i") 'imenu-completion)

(add-hook 'emacs-startup-hook
  (lambda ()
    (global-set-key (kbd "<wheel-left>") (lambda () (interactive)
      (if (get-buffer-window "*Completions*") (minibuffer-path-up) (find-file-current))))
    (global-set-key (kbd "<wheel-right>") (lambda () (interactive)
      (if (get-buffer-window "*Completions*") (minibuffer-path-origin) (imenu-completion))))))

(defun switch-buffer-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-complete (call-interactively 'switch-to-buffer)))
(global-set-key (kbd "C-x b") 'switch-buffer-completion)

(defun menu-this-window ()
  (interactive)
  (Buffer-menu-this-window)
  (delete-other-windows)
  (kill-buffer (get-buffer "*Buffer List*")))
(add-hook 'Buffer-menu-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") 'menu-this-window)
    (local-set-key (kbd "<return>") 'menu-this-window)
    (local-set-key (kbd "<mouse-2>") 'delete-window)))

(defun buffer-list-toggle ()
  (interactive)
  (if (get-buffer-window "*Buffer List*")
    (progn (delete-other-windows) (kill-buffer (get-buffer "*Buffer List*")))
    (list-buffers)))
(global-set-key (kbd "<mouse-8>") 'buffer-list-toggle)

(add-hook 'c-mode-hook
  (defun my-c-mode-hook ()
    (setcar (cdr (assoc "Class" imenu-generic-expression ))
      "^\\(template[    ]*<[^>]+>[  ]*\\)?\\(class\\|struct\\|union\\|typedef struct\\)[     ]+\\([[:alnum:]_]+\\(<[^>]+>\\)?\\)\\([     \n]\\|\\\\\n\\)*[:{]")))

(defun translation-word ()
  (interactive)
  (let* ((default-directory "~/")
         (str (shell-command-to-string (concat "trans-wrapper " (thing-at-point 'word 'no-properties) " | head -40"))))
    (save-point-position t)
    (message "%s" str)))
(global-set-key (kbd "C-c w") 'translation-word)

(defun my-put-file-path-on-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "<f7>") 'my-put-file-path-on-clipboard)

(defun my-put-file-name-on-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-name))))
    (when filename
      (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "<f8>") 'my-put-file-name-on-clipboard)

(recentf-mode)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key (kbd "C-x C-h") 'recentf-open-files)

(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))
(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(customize-set-variable 'search-whitespace-regexp nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "1ASC" :slant normal :weight normal :height 112 :width normal))))
 '(line-number ((t (:foreground "black"))))
 '(mode-line ((t (:background "grey75" :foreground "black"))))
 '(mode-line-inactive ((t (:background "grey75" :foreground "black" :weight light)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-mode-width 4)
 '(Buffer-menu-name-width 26)
 '(Buffer-menu-size-width 6)
 '(cursor-in-non-selected-windows t)
 '(display-line-numbers-width 4)
 '(ggtags-global-abbreviate-filename 200)
 '(hi-lock-file-patterns-policy 'never)
 '(hi-lock-highlight-range 2000000)
 '(imenu-use-popup-menu nil)
 '(max-mini-window-height 0.9)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(5 ((shift) . hscroll) ((meta)) ((control) . text-scale)))
 '(next-error-highlight-no-select nil)
 '(next-screen-context-lines 1)
 '(package-selected-packages '(anzu))
 '(read-buffer-completion-ignore-case t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(warning-suppress-types '((comp)))
 '(xref-marker-ring-length 1000))

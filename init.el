(load "~/.emacs.d/27.2/minibuffer.el")
(load "~/.emacs.d/27.2/man.el")
(load "~/.emacs.d/27.2/simple.el")
(load "~/.emacs.d/27.2/compile.el")
(load "~/.emacs.d/27.2/cc-mode.el")
(load "~/.emacs.d/27.2/hi-lock.el")
(load "~/.emacs.d/27.2/tramp-sh.el")

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'fzf)
(require 'anzu)
(require 'goto-last-change)
(require 'ggtags)

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
(save-place-mode)
(delete-selection-mode)

(setq minibuffer-origin-path nil)
(setq message-origin-point-position nil)
(setq inhibit-startup-screen t)
(setq auto-save-list-file-prefix nil)
(setq lazy-highlight-initial-delay 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq mouse-drag-copy-region t)
(setq read-file-name-completion-ignore-case t)
(setq next-error-highlight nil)
(setq minibuffer-message-timeout 0.5)
(setq ggtags-highlight-tag nil)
(setq scroll-step 1)
(setq lazy-highlight-cleanup t)

(setq-default mouse-1-click-follows-link nil)
(setq-default enable-recursive-minibuffers t)
(setq-default mode-line-format (list '(:eval (if (buffer-file-name) "%f" "%b")) " (%l %C)"))

(add-to-list 'auto-mode-alist '("\\.S\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\Makefile\\'" . fundamental-mode))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(defalias 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
(setenv "MANWIDTH" "192")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-M-u") 'upcase-region)
(global-set-key (kbd "C-M-d") 'downcase-region)

(defun eval-region-unmark (beg end)
  (interactive "r")
  (eval-region beg end)
  (deactivate-mark))
(global-set-key (kbd "M-e") 'eval-region-unmark)

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

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key (kbd "C-x C-h") 'recentf-open-files)

(defun fzfk ()
  (interactive)
  (let ((default-directory "/home/d/linux"))
    (fzf-find-file)))
(global-set-key (kbd "C-x C-t") 'fzfk)

(add-hook 'minibuffer-setup-hook
  (lambda ()
    (interactive)
    (if (get-buffer-window "*Completions*")
      (let ((window (get-buffer-window (window-buffer (minibuffer-selected-window)))))
        (setq origin-point-position (window-point window))
        (set-window-point window (window-start window))))))

(add-hook 'minibuffer-exit-hook
  (lambda ()
    (interactive)
    (if (get-buffer-window "*Completions*")
      (let ((window (get-buffer-window (window-buffer (minibuffer-selected-window)))))
        (set-window-point window (symbol-value 'origin-point-position))))))

(defun keyboard-escape-quit2 ()
  (interactive)
  (if message-origin-point-position (progn (goto-char message-origin-point-position) (setq message-origin-point-position nil)))
  (keyboard-escape-quit))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit2)

(defun keyboard-quit2 ()
  (interactive)
  (if message-origin-point-position (progn (goto-char message-origin-point-position) (setq message-origin-point-position nil)))
  (keyboard-quit))
(global-set-key (kbd "C-g") 'keyboard-quit2)

(defun minibuffer-path-up ()
  (interactive)
  (let ((path (minibuffer-contents)))
    (setq minibuffer-origin-path path)
    (delete-minibuffer-contents)
    (insert (replace-regexp-in-string "/[^/]*/?$" "/" path)))
  (minibuffer-completion-help))

(defun minibuffer-path-origin ()
  (interactive)
  (delete-minibuffer-contents)
  (insert minibuffer-origin-path)
  (minibuffer-completion-help))

(defun find-file-set-key ()
  (interactive)
  (local-set-key (kbd "TAB") (lambda () (interactive (progn (minibuffer-complete) (minibuffer-completion-help)))))
  (local-set-key (kbd "M-DEL") (lambda () (interactive) (backward-kill-word 1) (minibuffer-completion-help)))
  (local-set-key (kbd "M-\\") 'minibuffer-path-up)
  (local-set-key (kbd "/") (lambda () (interactive) (insert "/") (minibuffer-completion-help)))
  (minibuffer-completion-help))

(defun find-file-root ()
  (interactive)
  (let ((inhibit-message t) (default-directory "/"))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))
(global-set-key (kbd "C-x M-f") 'find-file-root)

(defun find-file-current ()
  (interactive)
  (let ((inhibit-message t))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))
(global-set-key (kbd "C-x C-f") 'find-file-current)
(global-set-key (kbd "<mouse-6>") 'find-file-current)

(defun find-file-home ()
  (interactive)
  (let ((inhibit-message t) (default-directory "~/"))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))
(global-set-key (kbd "C-x f") 'find-file-home)

(defun imenu-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-complete (call-interactively 'imenu)))
(global-set-key (kbd "M-i") 'imenu-completion)
(global-set-key (kbd "<mouse-7>") 'imenu-completion)

(defun switch-buffer-completion ()
  (interactive)
  (minibuffer-with-setup-hook 'minibuffer-complete (call-interactively 'switch-to-buffer)))
(global-set-key (kbd "C-x b") 'switch-buffer-completion)

(defun isearch-repeat-forward+ ()
  (interactive)
  (unless isearch-forward
    (goto-char isearch-other-end))
  (isearch-repeat-forward)
  (unless isearch-success
    (isearch-repeat-forward)))
(define-key isearch-mode-map (kbd "C-s") 'isearch-repeat-forward+)

(defun isearch-repeat-backward+ ()
  (interactive)
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward)
  (unless isearch-success
    (isearch-repeat-backward)))
(define-key isearch-mode-map (kbd "C-r") 'isearch-repeat-backward+)

(defun search-selection ()
  (interactive)
  (region-active-p)
  (kill-ring-save (region-beginning) (region-end))
  (isearch-mode t nil nil nil)
  (isearch-yank-pop))
(global-set-key (kbd "<f3>") 'search-selection)

(defun highlight-toggle ()
  (interactive)
  (let ((str (get-char-property (point) 'hi-lock-overlay-regexp)))
      (if str (hi-lock-unface-buffer str) (hi-lock-face-symbol-at-point))))

(defun mouse-highlight-toggle (click)
  (interactive "e")
  (if (region-active-p)
    (progn
      (hi-lock-face-symbol-at-point2 (buffer-substring (region-beginning) (region-end)))
      (deactivate-mark))
    (progn (mouse-set-point click) (highlight-toggle))))
(global-set-key (kbd "<mouse-3>") 'mouse-highlight-toggle)
(global-set-key (kbd "<f1>") (lambda () (interactive) (unhighlight-regexp t)))

(defun highligt-selection (beg end)
  (interactive "r")
  (hi-lock-face-symbol-at-point2 (buffer-substring beg end)))
(global-set-key (kbd "<f9>") 'highligt-selection)

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key [M-up] 'move-line-up)

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))
(global-set-key (kbd "M-n") 'move-line-down)
(global-set-key [M-down] 'move-line-down)

(defun kill-current-buffer ()
   (interactive)
   (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "<f12>") 'kill-current-buffer)

(defun kill-all-buffers () (interactive) (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k") 'kill-all-buffers)

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x C-o") 'kill-other-buffers)

(add-hook 'ggtags-global-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") 'compile-goto-error)
    (local-set-key (kbd "<mouse-9>") 'previous-error)
    (local-set-key (kbd "<mouse-8>") 'next-error)
    (local-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)))

(defun execute-kbd-tab () (interactive) (execute-kbd-macro (kbd "TAB")))

(add-hook 'completion-list-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") (lambda () (interactive (progn (choose-completion) (execute-kbd-tab)))))
    (local-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)
    (local-set-key (kbd "<mouse-6>") 'minibuffer-path-up)
    (local-set-key (kbd "<mouse-7>") 'minibuffer-path-origin)))

(add-hook 'c-mode-hook
  (lambda ()
    (defun insert-printf ()
      (interactive)
      (insert "printf(\"\\n\");")
      (backward-char 5))
    (global-set-key (kbd "C-c M-f") 'insert-printf)
    
    (defun insert-printk ()
      (interactive)
      (insert "printk(\"\\n\");")
      (backward-char 5))
    (global-set-key (kbd "C-c M-k") 'insert-printk)

    (defun insert-main ()
      (interactive)
      (insert "#include <stdio.h>\n\nint main(int argc, char *argv[])\n{\n\tprintf(\"\\n\");\n\treturn 0;\n}")
      (backward-char 18))
    (global-set-key (kbd "C-c M-m") 'insert-main)

    (local-set-key (kbd "<double-down-mouse-1>") (lambda ()(interactive)))
    (local-set-key (kbd "<double-mouse-1>") 'ggtags-find-tag-dwim)
    (local-set-key (kbd "M-n") (lambda () (interactive) (if (get-buffer-window "*ggtags-global*") (next-error) (move-line-down))))
    (local-set-key (kbd "M-p") (lambda () (interactive) (if (get-buffer-window "*ggtags-global*") (previous-error) (move-line-up))))
    (local-set-key (kbd "<mouse-2>") 'xref-pop-marker-stack)
    (local-set-key (kbd "<mouse-8>") 'next-error)
    (local-set-key (kbd "<mouse-9>") 'previous-error)
    (global-set-key (kbd "TAB") (lambda () (interactive) (insert "\t")))
    (ggtags-mode)))

(defun translation-word ()
  (interactive)
  (let* ((default-directory "~/")
        (str (shell-command-to-string (concat "transw " (thing-at-point 'word 'no-properties) " | head -40"))))
    (setq message-origin-point-position (window-point))
    (goto-char (window-start))
    (message "%s" str)))
(global-set-key (kbd "<f11>") 'translation-word)
(global-set-key (kbd "C-c w") 'translation-word)

(defun my-put-file-path-on-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "<f6>") 'my-put-file-path-on-clipboard)

(defun my-put-file-name-on-clipboard ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-name))))
    (when filename
      (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max)))
      (message filename))))
(global-set-key (kbd "<f7>") 'my-put-file-name-on-clipboard)

(global-set-key (kbd "<f4>") (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "<f5>") (lambda () (interactive) (buffer-disable-undo) (buffer-enable-undo) (message "reset-undo")))
(global-set-key (kbd "<f8>") 'save-buffer)
(global-set-key (kbd "<f10>") 'tmm-menubar)
(global-set-key (kbd "TAB") (lambda () (interactive) (insert "\t")))
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
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

(customize-set-variable 'search-whitespace-regexp nil)
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
 '(enable-local-variables nil)
 '(hi-lock-file-patterns-policy 'never)
 '(hi-lock-highlight-range 2000000)
 '(imenu-use-popup-menu nil)
 '(max-mini-window-height 0.9)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(next-screen-context-lines 1)
 '(read-buffer-completion-ignore-case t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(use-dialog-box nil))

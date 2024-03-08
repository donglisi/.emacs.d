(setq minibuffer-origin-path "")

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
  (let ((inhibit-message t) (default-directory (if (buffer-file-name) default-directory "~/")))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))
(global-set-key (kbd "C-x C-f") 'find-file-current)

(defun find-file-home ()
  (interactive)
  (let ((inhibit-message t) (default-directory "~/"))
    (minibuffer-with-setup-hook 'find-file-set-key (call-interactively 'find-file))))
(global-set-key (kbd "C-x f") 'find-file-home)

(add-hook 'completion-list-mode-hook
  (lambda ()
    (local-set-key (kbd "<mouse-1>") (lambda () (interactive (progn (choose-completion) (execute-kbd-macro (kbd "TAB"))))))
    (local-set-key (kbd "<mouse-2>") 'keyboard-escape-quit)))

(provide 'minibuffer-find-file)

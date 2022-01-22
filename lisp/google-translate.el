(defun transp (cmd beg end)
  (interactive "r")
  (message "%s" (shell-command-to-string (concat 
	cmd " \"" (replace-regexp-in-string "`" "\\`" (buffer-substring beg end) nil t) "\""))))

(defun translation-comment (beg end)
  (interactive "r")
  (transp "~/.emacs.d/bin/transc" beg end))

(defun translation-paragraph (beg end)
  (interactive "r")
  (transp "~/.emacs.d/bin/transp" beg end))

(defun translation-word ()
  (interactive)
  (message (shell-command-to-string (concat "transw " (thing-at-point 'word 'no-properties) " | head -n 20"))))

(defvar google-translate-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'translation-comment)
    (define-key map (kbd "C-c t") 'translation-paragraph)
    (define-key map (kbd "C-c w") 'translation-word) map))

(define-minor-mode google-translate-mode
  "Google Translate"
  :keymap google-translate-mode-keymap)

(define-globalized-minor-mode global-google-translate-mode google-translate-mode (lambda () (google-translate-mode)))

(provide 'google-translate)

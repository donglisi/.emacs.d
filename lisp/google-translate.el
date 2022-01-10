(defun translation-phrase (beg end)
  (interactive "r")
  (message (shell-command-to-string (concat "~/.emacs.d/bin/transp \"" (buffer-substring beg end) "\""))))

(defun translation-word ()
  (interactive)
  (message (shell-command-to-string (concat "~/.emacs.d/bin/transw " (current-word)))))

(defvar google-translate-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p") 'translation-phrase)
    (define-key map (kbd "C-c w") 'translation-word) map))

(define-minor-mode google-translate-mode
  "Google Translate"
  :keymap google-translate-mode-keymap)

(define-globalized-minor-mode global-google-translate-mode google-translate-mode (lambda () (google-translate-mode)))

(provide 'google-translate)
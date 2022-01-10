(defun translation-selection (beg end)
  (interactive "r")
  (message (shell-command-to-string (concat "~/.emacs.d/bin/trans \"" (buffer-substring beg end) "\""))))

(defun translation-selection-brief (beg end)
  (interactive "r")
  (message (shell-command-to-string (concat "~/.emacs.d/bin/transb \"" (buffer-substring beg end) "\""))))

(defun translation-word ()
  (interactive)
  (message (shell-command-to-string (concat "~/.emacs.d/bin/transw " (current-word)))))

(defvar google-translate-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v") 'translation-selection)
    (define-key map (kbd "C-c b") 'translation-selection-brief)
    (define-key map (kbd "C-c w") 'translation-word) map))

(define-minor-mode google-translate-mode
  "Google Translate"
  :keymap google-translate-mode-keymap)

(define-globalized-minor-mode global-google-translate-mode google-translate-mode (lambda () (google-translate-mode)))

(provide 'google-translate)

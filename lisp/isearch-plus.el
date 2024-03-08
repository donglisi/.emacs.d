(setq lazy-highlight-initial-delay 0)
(setq lazy-highlight-cleanup t)

(defun isearch-wrapper (flag)
  (interactive)
  (if (region-active-p)
    (let ((selection (buffer-substring-no-properties (region-beginning) (region-end))))      
      (goto-char (region-beginning))
      (isearch-mode nil)
      (isearch-yank-string selection)
      (deactivate-mark))
    (if flag (isearch-forward nil t) (isearch-backward nil t))))

(defun isearch-forward+ ()
  (interactive)
  (isearch-wrapper t))
(define-key global-map "\C-s" 'isearch-forward+)

(defun isearch-backward+ ()
  (interactive)
  (isearch-wrapper nil))
(define-key global-map "\C-r" 'isearch-backward+)

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

(provide 'isearch-plus)

(setq origin-point-position nil)
(setq origin-point-position-other nil)

(defun save-point-position (flag)
  (let* ((l (window-list (selected-frame)))
         (w (if flag (car l) (nth 1 l)))
         (ow (if flag (nth 1 l) (nth 2 l))))
    (setq origin-point-position (window-point w))
    (set-window-point w (window-start w))
    (if ow
      (progn
        (setq origin-point-position-other (window-point ow))
        (set-window-point ow (window-start ow))))))

(defun goto-origin-point-position (flag)
  (let* ((l (window-list (selected-frame)))
         (w (if flag (car l) (nth 1 l)))
         (ow (if flag (nth 1 l) (nth 2 l))))
    (if origin-point-position
      (progn
        (set-window-point w origin-point-position)
        (setq origin-point-position nil)))
    (if origin-point-position-other
      (progn
        (set-window-point ow origin-point-position-other)
        (setq origin-point-position-other nil)))))

(provide 'origin-point-position)

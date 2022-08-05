(defun wo-ctrl-c-map ()
  "Return a keymap freeing keys from control-modifier."
  (let ((newmap (make-sparse-keymap)))
    (mapc
     (lambda (map)
       (map-keymap
        (lambda (event binding)
          (let ((basic-event (vector (event-basic-type event))))
            (when (and (equal (event-modifiers event) '(control))
                       (equal (key-binding basic-event) #'self-insert-command)
                       (null (lookup-key newmap basic-event)))
              (define-key newmap basic-event binding))))
        map))
     (current-active-maps))
    newmap))

(defvar-local wo-ctrl-c-mode-active nil
  "If `wo-ctrl-c-mode' is active it sets this variable to a non-nil value.
This is a protection against consecutive calls of (wo-ctrl-c-mode 1).
The value is actually a list containing the original local map as element.")

(define-minor-mode wo-ctrl-c-mode
  "Bind all keys with control modifier also directly."
  :lighter " α"
  (if wo-ctrl-c-mode
      (unless wo-ctrl-c-mode-active ;;< protection against two consecutive calls of (wo-ctrl-c-mode 1)
    (setq wo-ctrl-c-mode-active (list (current-local-map)))
    (let ((map (wo-ctrl-c-map)))
      (set-keymap-parent map (car wo-ctrl-c-mode-active))
      (use-local-map map)))
    (when wo-ctrl-c-mode-active
      (use-local-map (car wo-ctrl-c-mode-active))
      (setq wo-ctrl-c-mode-active nil))))

(defun wo-ctrl-c-when-read-only ()
  "Activate `wo-ctrl-c-mode' when buffer is read-only."
  (if buffer-read-only
      (wo-ctrl-c-mode)
    (wo-ctrl-c-mode -1)))

(add-hook 'read-only-mode-hook #'wo-ctrl-c-when-read-only)

;; `find-file-noselect' sets `buffer-read-only' directly:
(add-hook 'find-file-hook #'wo-ctrl-c-when-read-only)

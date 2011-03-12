(defun tq-find-dir-containing-file (file &optional dir)
  (or dir (setq dir default-directory))
  (if (file-exists-p (concat dir file))
      dir
    (if (equal dir "/")
        nil
      (tq-find-dir-containing-file file (expand-file-name (concat dir "../"))))))

(defun tq-map-overlays (tuple-list)
  "make-overlay for each of a TUPLE(two line-numbers) LIST, recursively."
  (if (not tuple-list)
      nil
    (cons
     (make-overlay (point-at-bol (car (car tuple-list))) (point-at-eol (cadr (car tuple-list))))
     (tq-map-overlays (cdr tuple-list)))))

(defun tq-clear-cov-overlays ()
  (remove-overlays (point-min) (point-max) 'tqcov t))

(defun tq-cov-overlay-exists-in-list-p (ovl-list)
  (if (not ovl-list)
      nil
    (if (overlay-get (car ovl-list) 'tqcov)
        t
      (tq-cov-overlay-exists-in-list-p (cdr ovl-list)))))

(defun tq-cov-overlay-exists-p ()
  (tq-cov-overlay-exists-in-list-p (overlays-in (point-min) (point-max))))

(defun tq-cov-overlay-current-buffer-with-command (cmd)
  (print cmd)
  (save-excursion
    (beginning-of-buffer)
    (dolist (ovl (tq-map-overlays (read (shell-command-to-string cmd))))
      (progn
        (overlay-put ovl 'face (cons 'background-color "red4"))
        (overlay-put ovl 'tqcov t)
        ))))

(defun tq-toggle-cov-overlays (buffer)
  (interactive (list (current-buffer)))
  (with-current-buffer buffer
    (if (tq-cov-overlay-exists-p)
        (tq-clear-cov-overlays)
      (tq-cov-overlay-current-buffer-with-command
       (concat "ruby"
               " "
               (tq-find-dir-containing-file "coverage_stats.csv"
                                            (file-name-directory (buffer-file-name buffer)))
               "tqcov.rb"
               " "
               (expand-file-name (buffer-file-name buffer)))))))

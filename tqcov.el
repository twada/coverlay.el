(defvar tq-cov-alist nil)
(defvar tq-cov-data-file-name "coverage_stats.csv")

(require 'csv-mode)

(defun tq-cov-create-stats-buffer (data-file-path)
  (with-current-buffer (get-buffer-create "*tqcov-stats*")
    (csv-mode)
    (erase-buffer)
    (insert-file-contents data-file-path)
    (beginning-of-buffer)
    (current-buffer)
    ))

(defun tq-csv-current-field-to-string ()
  "This function does not move point."
  (buffer-substring (point)
                    (save-excursion
                      (forward-sexp 1)
                      (point))))

(defun tq-csv-next-field-to-string ()
  "This function DOES move point."
  (csv-forward-field 1)
  (forward-char 1)
  (tq-csv-current-field-to-string))

(defun tq-csv-line-to-list ()
  "This function DOES move point."
  (list
   (tq-csv-current-field-to-string)
   (string-to-number (tq-csv-next-field-to-string))
   (string-to-number (tq-csv-next-field-to-string))))

(defun tq-handle-uncovered-line (alist filename lineno)
  (setq file-segments (assoc filename alist))
  (setq segment-list-body (cdr file-segments))
  (if (not segment-list-body)
      (setcdr file-segments (list lineno lineno))
    (if (= (car segment-list-body) (- lineno 1))
        (setcar segment-list-body lineno)
      (setcdr file-segments (append (list lineno lineno) segment-list-body)))))

(defun tq-cov-create-tuple-pairs (even-list)
  (if (not even-list)
      nil
    (cons
     (list (car even-list) (car (cdr even-list)))
     (tq-cov-create-tuple-pairs (nthcdr 2 even-list)))))

(defun tq-cov-parse-buffer (buf)
  (setq statsbuf (or buf (current-buffer)))
  (setq alist nil)
  (with-current-buffer buf
    (while (not (eobp))
      (setq csv-cols (tq-csv-line-to-list))
      (setq filename (nth 0 csv-cols))
      (setq lineno (nth 1 csv-cols))
      (setq count (nth 2 csv-cols))
      (when (not (assoc filename alist))
        ;; (print (format "segments for %s does not exist" filename))
        (setq alist (cons (list filename) alist)))
      (when (= count 0)
        ;; (print (format "count %d is zero" count))
        (tq-handle-uncovered-line alist filename lineno))
      (forward-line 1))
    )
  alist
  )

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
               (tq-find-dir-containing-file tq-cov-data-file-name
                                            (file-name-directory (buffer-file-name buffer)))
               "tqcov.rb"
               " "
               (expand-file-name (buffer-file-name buffer)))))))

(provide 'tqcov)

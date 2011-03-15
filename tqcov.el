(defvar tq-cov-alist nil)
(defvar tq-cov-data-file-name "coverage_stats.csv")
(defvar tq-cov-untested-line-background-color "red4")

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
  "Convert current csv field to string. This function does not move point."
  (buffer-substring (point)
                    (save-excursion
                      (forward-sexp 1)
                      (point))))

(defun tq-csv-next-field-to-string ()
  "Convert next csv field to string. This function DOES move point."
  (csv-forward-field 1)
  (forward-char 1)
  (tq-csv-current-field-to-string))

(defun tq-csv-line-to-list ()
  "Convert current coverage stats line to list (string int int). This function DOES move point."
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

(defun tq-cov-parse-buffer (buf)
  "Parse buffer to alist. car of alist is filename, cdr is segment of lines."
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

(defun tq-cov-create-tuple-pairs (even-list)
  "convert (foo bar baz hoge) to ((foo bar) (baz hoge))"
  (if (not even-list)
      nil
    (cons
     (list (car even-list) (car (cdr even-list)))
     (tq-cov-create-tuple-pairs (nthcdr 2 even-list)))))

(defun tq-cov-reverse-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo)) (English . (foo bar baz))) to '((Japanese . (piyo fuga hoge)) (English . (baz bar foo)))"
  (if (not target-alist)
      nil
    (cons
     (cons
      (car (car target-alist))
      (reverse (cdr (car target-alist))))
     (tq-cov-reverse-cdr-of-alist (cdr target-alist)))))

(defun tq-cov-tuplize-cdr-of-alist (target-alist)
  (if (not target-alist)
      nil
    (cons
     (cons
      (car (car target-alist))
      (tq-cov-create-tuple-pairs (cdr (car target-alist))))
     (tq-cov-tuplize-cdr-of-alist (cdr target-alist)))))

(defun tq-cov-create-stats-alist-from-buffer (buf)
  (tq-cov-tuplize-cdr-of-alist (tq-cov-reverse-cdr-of-alist (tq-cov-parse-buffer buf))))

(defun tq-find-dir-containing-file (file &optional dir)
  (or dir (setq dir default-directory))
  (print (format "searching: %s" dir))
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

(defun tq-cov-overlay-current-buffer-with-list (tuple-list)
  (save-excursion
    (beginning-of-buffer)
    (dolist (ovl (tq-map-overlays tuple-list))
      (progn
        (overlay-put ovl 'face (cons 'background-color tq-cov-untested-line-background-color))
        (overlay-put ovl 'tqcov t)
        ))))


(defun tq-cov-search-stats-file-path (buffer)
  (concat (tq-find-dir-containing-file tq-cov-data-file-name
                                       (file-name-directory (buffer-file-name buffer)))
          tq-cov-data-file-name))

(defun tq-cov-get-or-load-stats-alist (buffer)
  (if (not tq-cov-alist)
      (progn
        (print "(not tq-cov-alist)")
        (setq stats-buf (tq-cov-create-stats-buffer (tq-cov-search-stats-file-path buffer)))
        (print "stats-buf created")
        (setq tq-cov-alist (tq-cov-create-stats-alist-from-buffer stats-buf))
        tq-cov-alist
        )
    (print "tq-cov-alist already loaded")
    tq-cov-alist
  ))

(defun tq-cov-toggle-overlays (buffer)
  (interactive (list (current-buffer)))
  (setq statsbuf (tq-cov-get-or-load-stats-alist buffer))
  (with-current-buffer buffer
    (if (tq-cov-overlay-exists-p)
        (tq-clear-cov-overlays)
      (tq-cov-overlay-current-buffer-with-list
       (cdr (assoc (expand-file-name (buffer-file-name buffer)) statsbuf))))))





(defun tq-cov-overlay-current-buffer-with-command (cmd)
  (print cmd)
  (tq-cov-overlay-current-buffer-with-list (read (shell-command-to-string cmd))))

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

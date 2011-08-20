(defvar tq-cov-alist nil)
(defvar tq-cov-data-file-name "coverage_stats.csv")
(defvar tq-cov-buffer-name "*tqcov-stats*")
(defvar tq-cov-untested-line-background-color "red4")

(require 'csv-mode)

(defun tq-cov-current-csv-field-to-string ()
  "Convert current csv field to string. This function does not move point."
  (buffer-substring (point)
                    (save-excursion
                      (forward-sexp 1)
                      (point))))

(defun tq-cov-next-csv-field-to-string ()
  "Convert next csv field to string. This function DOES move point."
  (csv-forward-field 1)
  (forward-char 1)
  (tq-cov-current-csv-field-to-string))

(defun tq-cov-current-csv-line-to-list ()
  "Convert current coverage stats line to list (string int int). This function DOES move point."
  (list
   (tq-cov-current-csv-field-to-string)
   (string-to-number (tq-cov-next-csv-field-to-string))
   (string-to-number (tq-cov-next-csv-field-to-string))))

(defun tq-cov-handle-uncovered-line (alist filename lineno)
  (setq file-segments (assoc filename alist))
  (setq segment-list-body (cdr file-segments))
  (if (not segment-list-body)
      (setcdr file-segments (list lineno lineno))
    (if (= (car segment-list-body) (- lineno 1))
        (setcar segment-list-body lineno)
      (setcdr file-segments (append (list lineno lineno) segment-list-body)))))

(defun tq-cov-parse-buffer (buf)
  "Parse buffer to alist. car of each element is filename, cdr is segment of lines."
  (setq alist nil)
  (with-current-buffer buf
    (while (not (eobp))
      (setq csv-cols (tq-cov-current-csv-line-to-list))
      (setq filename (expand-file-name (nth 0 csv-cols)))
      (setq lineno (nth 1 csv-cols))
      (setq count (nth 2 csv-cols))
      (when (not (assoc filename alist))
        ;; (print (format "segments for %s does not exist" filename))
        (setq alist (cons (list filename) alist)))
      (when (= count 0)
        ;; (print (format "count %d is zero" count))
        (tq-cov-handle-uncovered-line alist filename lineno))
      (forward-line 1))
    )
  alist
  )

(defun tq-cov-create-tuple-pairs (even-list)
  "convert (foo bar baz hoge) to ((foo bar) (baz hoge))"
  (setq result '())
  (while even-list
    (setq result (cons (list (car even-list) (car (cdr even-list))) result))
    (setq even-list (nthcdr 2 even-list)))
  (nreverse result))

(defun tq-cov-reverse-cdr (target-list)
  (cons
   (car target-list)
   (reverse (cdr target-list))))

(defun tq-cov-reverse-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo)) (English . (foo bar baz))) to '((Japanese . (piyo fuga hoge)) (English . (baz bar foo)))"
  (mapcar 'tq-cov-reverse-cdr target-alist))

(defun tq-cov-tuplize-cdr (target-list)
  (progn
    (setcdr target-list (tq-cov-create-tuple-pairs (cdr target-list)))
    target-list))

(defun tq-cov-tuplize-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo moge)) (English . (foo bar baz moo)))  to '((Japanese . ((hoge fuga) (piyo moge)) (English . ((foo bar) (baz moo))))"
  (mapcar 'tq-cov-tuplize-cdr target-alist))

(defun tq-cov-create-stats-alist-from-buffer (buf)
  (tq-cov-tuplize-cdr-of-alist (tq-cov-reverse-cdr-of-alist (tq-cov-parse-buffer buf))))

(defun tq-cov-make-overlay (tuple)
  (make-overlay (point-at-bol (car tuple)) (point-at-eol (cadr tuple))))

(defun tq-map-overlays (tuple-list)
  "make-overlay for each of a TUPLE(two line-numbers) LIST."
  (mapcar 'tq-cov-make-overlay tuple-list))

(defun tq-clear-cov-overlays ()
  (remove-overlays (point-min) (point-max) 'tqcov t))

(defun tq-cov-overlay-exists-in-list-p (ovl-list)
  (catch 'loop
    (dolist (ovl ovl-list)
      (if (overlay-get ovl 'tqcov) (throw 'loop t) nil)
      nil)))

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

(defun tq-cov-create-stats-buffer (data-file-path)
  "get or create buffer filled with contents specified as data-file-path"
  (with-current-buffer (get-buffer-create tq-cov-buffer-name)
    (csv-mode)
    (erase-buffer)
    (insert-file-contents data-file-path)
    (beginning-of-buffer)
    (current-buffer)
    ))

(defun tq-find-dir-containing-file (file &optional cov-project-dir)
  (or cov-project-dir (setq cov-project-dir default-directory))
  ;; (print (format "searching: %s" cov-project-dir))
  (if (file-exists-p (concat cov-project-dir file))
      cov-project-dir
    (if (equal cov-project-dir "/")
        nil
      (tq-find-dir-containing-file file (expand-file-name (concat cov-project-dir "../"))))))

(defun tq-cov-project-dir (buffer)
  (setq cov-project-dir
        (tq-find-dir-containing-file
         tq-cov-data-file-name
         (file-name-directory (buffer-file-name buffer)))))

(defun tq-cov-search-stats-file-path (buffer)
  (concat (tq-cov-project-dir buffer) tq-cov-data-file-name))

(defun tq-cov-get-or-load-stats-alist (buffer)
  (if tq-cov-alist
      tq-cov-alist
    (setq stats-buf (tq-cov-create-stats-buffer (tq-cov-search-stats-file-path buffer)))
    (setq tq-cov-alist (tq-cov-create-stats-alist-from-buffer stats-buf))
    tq-cov-alist))

(defun tq-cov-stats-tuples-for (buffer stats-alist)
  (cdr (assoc (expand-file-name (buffer-file-name buffer)) stats-alist)))

(defun tq-cov-toggle-overlays (buffer)
  "toggle coverage overlay"
  (interactive (list (current-buffer)))
  (setq stats-alist (tq-cov-get-or-load-stats-alist buffer))
  (with-current-buffer buffer
    (if (tq-cov-overlay-exists-p)
        (tq-clear-cov-overlays)
      (tq-cov-overlay-current-buffer-with-list
       (tq-cov-stats-tuples-for buffer stats-alist)))))

(provide 'tqcov)

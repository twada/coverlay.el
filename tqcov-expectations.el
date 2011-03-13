(require 'csv-mode)

(defun tq-cov-test-setup (data-file)
  (setq dir (file-name-directory (buffer-file-name (current-buffer))))
  (with-current-buffer (get-buffer-create "*tqcov-stats.csv*")
    (csv-mode)
    (erase-buffer)
    (insert-file-contents (concat dir data-file))
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
  (cons
   (tq-csv-current-field-to-string)
   (cons
    (string-to-number (tq-csv-next-field-to-string))
    (cons
     (string-to-number (tq-csv-next-field-to-string)) nil))))

(defun tq-handle-uncovered-line (alist filename lineno)
  (setq file-segments (assoc filename alist))
  (setq segment-list-body (cdr file-segments))
  (if (not segment-list-body)
      (setcdr file-segments (list lineno lineno))
    (if (= (car segment-list-body) (- lineno 1))
        (setcar segment-list-body lineno)
      (setcdr file-segments (append (list lineno lineno) segment-list-body)))))

(defun tq-iterate-lines ()
  (setq alist nil)
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
  alist
  )

(defun tmp-test ()
  (interactive)
  (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
    (tq-iterate-lines)
    ))


(expectations
   (desc "quoted list")
   (expect '(3 5)
     '(3 5))
   (expect '((3 5) (8 12))
     '((3 5) (8 12)))

   (desc "alist learning")
   (expect '("foo" . "bar")
     (setq words '(("hoge" . "fuga") ("foo" . "bar") ("toto" . "titi")))
     (assoc "foo" words)
     )
   (expect nil
     (setq words '(("hoge" . "fuga") ("foo" . "bar") ("toto" . "titi")))
     (assoc "BOO" words)
     )
   (expect '("hoge")
     (setq words '(("hoge")))
     (assoc "hoge" words)
     )
   (expect nil
     (setq words '(("hoge")))
     (cdr (assoc "hoge" words))
     )
   (expect '("fuga" "piyo" "moge")
     (setq words '(("hoge" "fuga" "piyo" "moge")))
     (cdr (assoc "hoge" words))
     )


   (desc "file contents loading")
   (expect 5
     (setq dir (file-name-directory (buffer-file-name (current-buffer))))
     (with-current-buffer (get-buffer-create "*tqcov-stats.csv*")
       (erase-buffer)
       (insert-file-contents (concat dir "coverage_stats.csv"))
       (count-lines (point-min) (point-max))))

   (desc "csv-forward-field")
   (expect 1
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-forward-field 0)
       (point)
       ))
   (expect 21
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-forward-field 1)
       (point)
       ))
   (expect 23
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-forward-field 2)
       (point)
       ))
   (expect 25
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-forward-field 3)
       (point)
       ))
   (expect 46
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-forward-field 4)
       (point)
       ))

   (desc "substr field")
   (expect "/path/to/app/init.js"
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (tq-csv-current-field-to-string)))

   (expect 1
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (string-to-number (tq-csv-next-field-to-string))))

   (expect 2
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (tq-csv-next-field-to-string)
       (string-to-number (tq-csv-next-field-to-string))))

   (expect '("/path/to/app/init.js" 1 2)
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (tq-csv-line-to-list)))

   (expect '("/path/to/app/init.js" 3 1)
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (tq-csv-line-to-list)
       (forward-line)
       (tq-csv-line-to-list)))

   (desc "map-csv-lines-to-list")
   (expect '("/path/to/app/init.js" 3 1)
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (tq-csv-line-to-list)
       (forward-line)
       (tq-csv-line-to-list)))

   (desc "csv-split-string")
   (expect '("/path/to/app/init.js,1,2")
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (csv-split-string
		(buffer-substring-no-properties (point) (line-end-position)))))

   (desc "alist")
   (expect '("/path/to/app/init.js" 6 4)
     (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
       (assoc "/path/to/app/init.js" (tq-iterate-lines))
       ))
   (expect '("/path/to/app/init.js" 8 8 6 4)
     (with-current-buffer (tq-cov-test-setup "coverage_stats2.csv")
       (assoc "/path/to/app/init.js" (tq-iterate-lines))
       ))
   (expect '("/path/to/lib/utils.js" 82 82 76 76 70 70 62 62 55 55 38 37 29 27 24 22 17 17 6 4)
     (with-current-buffer (tq-cov-test-setup "coverage_stats3.csv")
       (assoc "/path/to/lib/utils.js" (tq-iterate-lines))
       ))
   (expect '("/path/to/app/init.js" 20 19)
     (with-current-buffer (tq-cov-test-setup "coverage_stats4.csv")
       (assoc "/path/to/app/init.js" (tq-iterate-lines))
       ))
   (expect '("/path/to/lib/utils.js" 82 82 76 76 70 70 62 62 55 55 38 37 29 27 24 22 17 17 6 4)
     (with-current-buffer (tq-cov-test-setup "coverage_stats4.csv")
       (assoc "/path/to/lib/utils.js" (tq-iterate-lines))
       ))

   ;; (desc "csv-interactive-args")
   ;; (expect '(1 103)
   ;;   (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
   ;;     (csv-interactive-args 'single)))

   )

(require 'csv-mode)

(defun tq-cov-test-setup (data-file)
  (setq dir (file-name-directory (buffer-file-name (current-buffer))))
  (with-current-buffer (get-buffer-create "*tqcov-stats.csv*")
    (csv-mode)
    (erase-buffer)
    (insert-file-contents (concat dir data-file))
    (current-buffer)
    ))

(defun tq-csv-field-to-string ()
  (buffer-substring (point)
                    (save-excursion
                      (forward-sexp 1)
                      (point))))

(defun tq-csv-next-field-to-string ()
  (csv-forward-field 1)
  (forward-char 1)
  (tq-csv-field-to-string))

(defun tq-csv-line-to-list ()
  (cons
   (tq-csv-field-to-string)   (cons
    (string-to-number (tq-csv-next-field-to-string))
    (cons
     (string-to-number (tq-csv-next-field-to-string)) nil))))


(expectations
   (desc "quoted list")
   (expect '(3 5)
     '(3 5))
   (expect '((3 5) (8 12))
     '((3 5) (8 12)))

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
       (tq-csv-field-to-string)))

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


   ;; (desc "csv-interactive-args")
   ;; (expect '(1 103)
   ;;   (with-current-buffer (tq-cov-test-setup "coverage_stats.csv")
   ;;     (csv-interactive-args 'single)))

   )

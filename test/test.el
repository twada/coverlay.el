(require 'coverlay)

(defun coverlay-test-setup (data-file)
  (setq dir (file-name-directory (buffer-file-name (current-buffer))))
  (coverlay-create-stats-buffer (concat dir "/fixtures/" data-file)))

(expectations

  (desc "detect source file line")
  (expect t
    (coverlay-source-filep "SF:/path/to/target.js"))
  (expect nil
    (coverlay-source-filep "DA:15,1"))
  (expect nil
    (coverlay-source-filep "end_of_record"))

  (desc "detect data line")
  (expect nil
    (coverlay-data-linep "SF:/path/to/target.js"))
  (expect t
    (coverlay-data-linep "DA:15,1"))
  (expect nil
    (coverlay-data-linep "end_of_record"))

  (desc "detect end of record")
  (expect nil
    (coverlay-end-of-recordp "SF:/path/to/target.js"))
  (expect nil
    (coverlay-end-of-recordp "DA:15,1"))
  (expect t
    (coverlay-end-of-recordp "end_of_record"))

  (desc "extract source file path")
  (expect "/path/to/target.js"
    (coverlay-extract-source-file "SF:/path/to/target.js"))
  (expect "/path/to/another.js"
    (coverlay-extract-source-file "SF:/path/to/another.js"))

  (desc "extract data")
  (expect '(15 1)
    (coverlay-extract-data-list "DA:15,1"))
  (expect '(21 0)
    (coverlay-extract-data-list "DA:21,0"))

  (desc "coverlay-parse-buffer")
  (expect '("/path/to/target.js" 25 25 21 21)
    (with-current-buffer (coverlay-test-setup "tiny.lcov")
      (assoc "/path/to/target.js" (coverlay-parse-buffer (current-buffer)))
      ))
  (expect '("/path/to/target.js" 25 25 21 21)
    (with-current-buffer (coverlay-test-setup "twofiles.lcov")
      (assoc "/path/to/target.js" (coverlay-parse-buffer (current-buffer)))
      ))
  (expect '("/path/to/another.js" 23 22)
    (with-current-buffer (coverlay-test-setup "twofiles.lcov")
      (assoc "/path/to/another.js" (coverlay-parse-buffer (current-buffer)))
      ))

)

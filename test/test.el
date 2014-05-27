(require 'coverlay)

(defun coverlay-test-setup (data-file)
  (setq dir (file-name-directory (buffer-file-name (current-buffer))))
  (coverlay-create-stats-buffer (concat dir "/fixtures/" data-file)))

(expectations

  ;; line detection & extraction

  (desc "detect source file line")
  (expect t
    (coverlay-source-file-p "SF:/path/to/target.js"))
  (expect nil
    (coverlay-source-file-p "DA:15,1"))
  (expect nil
    (coverlay-source-file-p "end_of_record"))

  (desc "detect data line")
  (expect nil
    (coverlay-data-line-p "SF:/path/to/target.js"))
  (expect t
    (coverlay-data-line-p "DA:15,1"))
  (expect nil
    (coverlay-data-line-p "end_of_record"))

  (desc "detect end of record")
  (expect nil
    (coverlay-end-of-record-p "SF:/path/to/target.js"))
  (expect nil
    (coverlay-end-of-record-p "DA:15,1"))
  (expect t
    (coverlay-end-of-record-p "end_of_record"))

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

  ;; file parsing

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

  (desc "coverlay-create-stats-alist-from-buffer")
  (expect '((21 21) (25 25))
    (setq stats-buf (coverlay-test-setup "tiny.lcov"))
    (setq stats-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    (cdr (assoc "/path/to/target.js" stats-alist)))
  (expect '((21 21) (25 25))
    (setq stats-buf (coverlay-test-setup "twofiles.lcov"))
    (setq stats-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    (cdr (assoc "/path/to/target.js" stats-alist)))
  (expect '((22 23))
    (setq stats-buf (coverlay-test-setup "twofiles.lcov"))
    (setq stats-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    (cdr (assoc "/path/to/another.js" stats-alist)))

  (desc "real data")
  (expect '((25 25) (29 29) (36 36) (455 456) (463 463))
    (setq stats-buf (coverlay-test-setup "espower.lcov"))
    (setq stats-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    (cdr (assoc "/path/to/espower/lib/espower.js" stats-alist)))
  (expect '((21 21) (25 25) (121 121) (332 332) (335 335) (342 342) (423 423) (468 468) (470 470) (511 511) (593 593) (596 596) (600 600))
    (setq stats-buf (coverlay-test-setup "power-assert-formatter.lcov"))
    (setq stats-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    (cdr (assoc "/path/to/power-assert-formatter/lib/power-assert-formatter.js" stats-alist)))

  ;; tuple tests

  (desc "coverlay-create-tuple-pairs")
  (expect '(("foo" "bar") ("baz" "hoge"))
    (coverlay-create-tuple-pairs '("foo" "bar" "baz" "hoge")))
  (expect '(("foo" "bar") ("baz" "hoge") ("fuga" nil))
    (coverlay-create-tuple-pairs '("foo" "bar" "baz" "hoge" "fuga")))

  (desc "coverlay-reverse-cdr-of-alist")
  (expect '("Japanese" . ("piyo" "fuga" "hoge"))
    (setq words '(("Japanese" . ("hoge" "fuga" "piyo")) ("English" . ("foo" "bar" "baz"))))
    (assoc "Japanese" (coverlay-reverse-cdr-of-alist words)))
  (expect '("English" . ("baz" "bar" "foo"))
    (setq words '(("Japanese" . ("hoge" "fuga" "piyo")) ("English" . ("foo" "bar" "baz"))))
    (assoc "English" (coverlay-reverse-cdr-of-alist words)))

  (desc "coverlay-tuplize-cdr-of-alist")
  (expect '("Japanese" . (("hoge" "fuga") ("piyo" "moge")))
    (setq words '(("Japanese" . ("hoge" "fuga" "piyo" "moge")) ("English" . ("foo" "bar" "baz" "moo"))))
    (assoc "Japanese" (coverlay-tuplize-cdr-of-alist words)))
  (expect '("English" . (("foo" "bar") ("baz" "moo")))
    (setq words '(("Japanese" . ("hoge" "fuga" "piyo" "moge")) ("English" . ("foo" "bar" "baz" "moo"))))
    (assoc "English" (coverlay-tuplize-cdr-of-alist words)))

  ;; learning tests

  (desc "quoted list learning")
  (expect '(3 5)
    '(3 5))
  (expect '((3 5) (8 12))
    '((3 5) (8 12)))

  (desc "alist learning")
  (expect '("foo" . "bar")
    (setq words '(("hoge" . "fuga") ("foo" . "bar") ("toto" . "titi")))
    (assoc "foo" words))
  (expect nil
    (setq words '(("hoge" . "fuga") ("foo" . "bar") ("toto" . "titi")))
    (assoc "BOO" words))
  (expect '("hoge")
    (setq words '(("hoge")))
    (assoc "hoge" words))
  (expect nil
    (setq words '(("hoge")))
    (cdr (assoc "hoge" words)))
  (expect '("fuga" "piyo" "moge")
    (setq words '(("hoge" "fuga" "piyo" "moge")))
    (cdr (assoc "hoge" words)))

)

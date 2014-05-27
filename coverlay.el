;;; coverlay.el --- Code Coverage Overlay for Emacs

;; Copyright (C) 2011 Takuto Wada

;; Author: Takuto Wada <takuto.wada at gmail com>
;; Keywords: coverage, overlay
;; Homepage: http://github.com/twada/coverlay.el
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; ------------
;;
;; add this to your .emacs or init.el
;; (add-to-list 'load-path "~/.emacs.d/path/to/coverlay/dir")
;;
;; and load it
;; (require 'coverlay)
;;
;; then keymap it if you want
;;  (define-key js2-mode-map "\C-c5" 'coverlay-toggle-overlays)

;; Dependencies
;; ------------
;;
;; tests(expectations) are depend on `el-expectataions.el`.
;; 
;;
;;; Code:

(defvar coverlay-alist nil)
(defvar coverlay-data-file-name "coverage.lcov")
(defvar coverlay-buffer-name "*coverlay-stats*")
(defvar coverlay-untested-line-background-color "red4")
(defvar coverlay-project-dir nil)

;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
(defun coverlay-string-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun coverlay-source-filep (line)
  (coverlay-string-starts-with line "SF:"))

(defun coverlay-data-linep (line)
  (coverlay-string-starts-with line "DA:"))

(defun coverlay-end-of-recordp (line)
  (coverlay-string-starts-with line "end_of_record"))

(defun coverlay-extract-rhs (line) 
  (substring line (+ (string-match "\:" line) 1)))

(defun coverlay-extract-source-file (line) 
  (coverlay-extract-rhs line))

(defun coverlay-extract-data-list (line) 
  (mapcar 'string-to-number (split-string (coverlay-extract-rhs line) ",")))

(defun coverlay-current-line ()
  (buffer-substring (point)
                    (save-excursion
                      (end-of-line)
                      (point))))

(defun coverlay-handle-uncovered-line (alist filename lineno)
  (setq file-segments (assoc filename alist))
  (setq segment-list-body (cdr file-segments))
  (if (not segment-list-body)
      (setcdr file-segments (list lineno lineno))
    (if (= (car segment-list-body) (- lineno 1))
        (setcar segment-list-body lineno)
      (setcdr file-segments (append (list lineno lineno) segment-list-body)))))

(defun coverlay-parse-buffer (buf)
  "Parse buffer to alist. car of each element is filename, cdr is segment of lines."
  (setq alist nil)
  (setq filename nil)
  (with-current-buffer buf
    (while (not (eobp))
      (setq current-line (coverlay-current-line))
      (when (coverlay-source-filep current-line)
        (setq filename (coverlay-extract-source-file current-line))
        (when (not (assoc filename alist))
          ;; (print (format "segments for %s does not exist" filename))
          (setq alist (cons (list filename) alist))))
      (when (coverlay-data-linep current-line)
        (setq cols (coverlay-extract-data-list current-line))
        (setq lineno (nth 0 cols))
        (setq count (nth 1 cols))
        (when (= count 0)
          ;; (print (format "count %d is zero" count))
          (coverlay-handle-uncovered-line alist filename lineno)))
      (forward-line 1))
    )
  alist
  )

(defun coverlay-create-tuple-pairs (even-list)
  "convert (foo bar baz hoge) to ((foo bar) (baz hoge))"
  (setq result '())
  (while even-list
    (setq result (cons (list (car even-list) (car (cdr even-list))) result))
    (setq even-list (nthcdr 2 even-list)))
  (nreverse result))

(defun coverlay-reverse-cdr (target-list)
  (cons
   (car target-list)
   (reverse (cdr target-list))))

(defun coverlay-reverse-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo)) (English . (foo bar baz))) to '((Japanese . (piyo fuga hoge)) (English . (baz bar foo)))"
  (mapcar 'coverlay-reverse-cdr target-alist))

(defun coverlay-tuplize-cdr (target-list)
  (progn
    (setcdr target-list (coverlay-create-tuple-pairs (cdr target-list)))
    target-list))

(defun coverlay-tuplize-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo moge)) (English . (foo bar baz moo)))  to '((Japanese . ((hoge fuga) (piyo moge)) (English . ((foo bar) (baz moo))))"
  (mapcar 'coverlay-tuplize-cdr target-alist))

(defun coverlay-create-stats-alist-from-buffer (buf)
  (coverlay-tuplize-cdr-of-alist (coverlay-reverse-cdr-of-alist (coverlay-parse-buffer buf))))

(defun coverlay-make-overlay (tuple)
  (make-overlay (point-at-bol (car tuple)) (point-at-eol (cadr tuple))))

(defun coverlay-map-overlays (tuple-list)
  "make-overlay for each of a TUPLE(two line-numbers) LIST."
  (mapcar 'coverlay-make-overlay tuple-list))

(defun coverlay-clear-cov-overlays ()
  (remove-overlays (point-min) (point-max) 'coverlay t))

(defun coverlay-overlay-exists-in-list-p (ovl-list)
  (catch 'loop
    (dolist (ovl ovl-list)
      (if (overlay-get ovl 'coverlay) (throw 'loop t) nil)
      nil)))

(defun coverlay-overlay-exists-p ()
  (coverlay-overlay-exists-in-list-p (overlays-in (point-min) (point-max))))

(defun coverlay-overlay-current-buffer-with-list (tuple-list)
  (save-excursion
    (beginning-of-buffer)
    (dolist (ovl (coverlay-map-overlays tuple-list))
      (progn
        (overlay-put ovl 'face (cons 'background-color coverlay-untested-line-background-color))
        (overlay-put ovl 'coverlay t)
        ))))

(defun coverlay-create-stats-buffer (data-file-path)
  "get or create buffer filled with contents specified as data-file-path"
  (with-current-buffer (get-buffer-create coverlay-buffer-name)
    (erase-buffer)
    (insert-file-contents data-file-path)
    (beginning-of-buffer)
    (current-buffer)
    ))

(defun coverlay-find-dir-containing-file (file &optional coverlay-project-dir)
  (or coverlay-project-dir (setq coverlay-project-dir default-directory))
  ;; (print (format "searching: %s %s" coverlay-project-dir file))
  (if (file-exists-p (concat coverlay-project-dir file))
      coverlay-project-dir
    (if (equal coverlay-project-dir "/")
        nil
      (coverlay-find-dir-containing-file file (expand-file-name (concat coverlay-project-dir "../"))))))

(defun coverlay-search-project-dir (buffer)
  (setq coverlay-project-dir
        (coverlay-find-dir-containing-file
         coverlay-data-file-name
         (file-name-directory (buffer-file-name buffer)))))

(defun coverlay-search-stats-file-path (buffer)
  (concat (coverlay-search-project-dir buffer) coverlay-data-file-name))

(defun coverlay-get-or-load-stats-alist (buffer)
  (if coverlay-alist
      coverlay-alist
    (setq stats-buf (coverlay-create-stats-buffer (coverlay-search-stats-file-path buffer)))
    (setq coverlay-alist (coverlay-create-stats-alist-from-buffer stats-buf))
    coverlay-alist))

(defun coverlay-stats-tuples-for (buffer stats-alist)
  (cdr (assoc (expand-file-name (buffer-file-name buffer)) stats-alist)))

(defun coverlay-reload-stats-alist (buffer)
  "reload coverage data"
  (interactive (list (current-buffer)))
  (setq stats-buf (coverlay-create-stats-buffer (coverlay-search-stats-file-path buffer)))
  (setq coverlay-alist (coverlay-create-stats-alist-from-buffer stats-buf)))

(defun coverlay-toggle-overlays (buffer)
  "toggle coverage overlay"
  (interactive (list (current-buffer)))
  (setq stats-alist (coverlay-get-or-load-stats-alist buffer))
  (with-current-buffer buffer
    (if (coverlay-overlay-exists-p)
        (coverlay-clear-cov-overlays)
      (coverlay-overlay-current-buffer-with-list
       (coverlay-stats-tuples-for buffer stats-alist)))))

(provide 'coverlay)

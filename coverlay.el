;;; coverlay.el --- Test coverage overlay for Emacs

;; Copyright (C) 2011-2014 Takuto Wada

;; Author: Takuto Wada <takuto.wada at gmail com>
;; Keywords: coverage, overlay
;; Homepage: https://github.com/twada/coverlay.el
;; Version: 0.5.0

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

(require 'filenotify)

;;; Commentary:
;; ------------
;;
;; Load coverlay.el in your .emacs
;;
;;     (require 'coverlay)
;;
;; Minor mode will toggle overlays in all buffers according to current lcov file
;;
;;     M-x coverlay-mode
;;
;; Load lcov file into coverlay buffer
;;
;;     M-x coverlay-load-file /path/to/lcov-file
;;
;; Load and watch a lcov file for changes
;;
;;     M-x coverlay-watch-file /path/to/lcov-file
;;
;; Toggle overlay for current buffer
;;
;;     M-x coverlay-toggle-overlays
;;

;;; Todo/Ideas:
;;
;; * optionally color covered lines
;; * add status view
;; * better lcov data change detection on watch/load
;; * add branch coverage
;; * ability to load/watch multiple lcov files

;;; Code:

;; coverage data parsed from lcov-file-buffer
(defvar coverlay-alist nil)

;; holds a token for the current watch (if any) for removal
(defvar coverlay--watch-descriptor nil)

;;
;; Customizable variables

(defgroup coverlay nil
  "Test coverage overlay for Emacs."
  :group 'tools
  :prefix "coverlay:")

(defcustom coverlay:data-buffer-name "*coverlay-stats*"
  "temp buffer name for coverage data."
  :type 'string
  :group 'coverlay)

(defcustom coverlay:untested-line-background-color "red4"
  "background-color for untested lines."
  :type 'string
  :group 'coverlay)


;;
;; command: coverlay-load-file

;;;###autoload
(defun coverlay-load-file (filepath)
  "(re)load lcov coverage data from FILEPATH."
  (interactive (list (read-file-name "lcov file: ")))
  (coverlay--lcov-update filepath))

(defun coverlay-file-load-callback ()
  "Initialize overlays in buffer after loading."
  (let* ((filename (buffer-file-name))
         (buffer-coverage-data (coverlay-stats-tuples-for (current-buffer) coverlay-alist)))
    (when buffer-coverage-data
      (message (format "loading coverlay for file: %s" filename))
      (coverlay-overlay-current-buffer-with-data buffer-coverage-data))))

;;
;; command: coverlay-watch-file

;;;###autoload
(defun coverlay-watch-file (filepath)
  "Watch file at FILEPATH for coverage data."
  (if file-notify--library
      (coverlay--do-watch-file filepath)
    (message "file notify not supported, please use coverlay-load-file instead")))

(defun coverlay--do-watch-file (filepath)
  "Use notify lib to Watch file at FILEPATH for coverage data."
  (interactive (list (read-file-name "lcov file: ")))
  (coverlay-end-watch)
  (coverlay-load-file filepath)
  (message (format "coverlay watching %s" filepath))
  (setq coverlay--watch-descriptor
        (file-notify-add-watch filepath '(change)
                               #'coverlay-watch-callback)))

(defun coverlay-end-watch ()
  "Remove the current filewatch if any."
  (file-notify-rm-watch coverlay--watch-descriptor))

(defun coverlay-watch-callback (args)
  "Reload data on coverage change in ARGS."
  (let ((filepath (nth 2 args)))
    (progn
      (message (format "coverlay updating from %s" filepath))
      (coverlay--lcov-update filepath))))

(defun coverlay--lcov-update (filepath)
  "Update internal state and all buffers for new lcov data from FILEPATH."
  (coverlay--clear-all-buffers)
  (coverlay-create-buffer-from-filepath filepath)
  (coverlay--overlay-all-buffers))

(defun coverlay-create-buffer-from-filepath (filepath)
  "Get or create stats buffer from FILEPATH."
  (let ((stats-buf (coverlay-create-stats-buffer filepath)))
    (setq coverlay-alist (coverlay-create-stats-alist-from-buffer stats-buf))))

(defun coverlay-create-stats-buffer (data-file-path)
  "Get or create buffer filled with contents specified as DATA-FILE-PATH."
  (with-current-buffer (get-buffer-create coverlay:data-buffer-name)
    (erase-buffer)
    (insert-file-contents data-file-path)
    (goto-char (point-min))
    (current-buffer)))

(defun coverlay-create-stats-alist-from-buffer (buffer)
  "Create the alist from data in BUFFER."
  (coverlay-tuplize-cdr-of-alist (coverlay-reverse-cdr-of-alist (coverlay-parse-buffer buffer))))

(defun coverlay-reverse-cdr-of-alist (target-alist)
  "Convert '((Japanese . (hoge fuga piyo)) (English . (foo bar baz))) to '((Japanese . (piyo fuga hoge)) (English . (baz bar foo))) in TARGET-ALIST."
  (mapcar 'coverlay-reverse-cdr target-alist))

(defun coverlay-reverse-cdr (target-list)
  "Reverse CDR in TARGET-LIST."
  (cons
   (car target-list)
   (reverse (cdr target-list))))

(defun coverlay-tuplize-cdr-of-alist (target-alist)
  "Convert '((Japanese . (hoge fuga piyo moge)) (English . (foo bar baz moo)))  to '((Japanese . ((hoge fuga) (piyo moge)) (English . ((foo bar) (baz moo)))) in TARGET-ALIST."
  (mapcar 'coverlay-tuplize-cdr target-alist))

(defun coverlay-tuplize-cdr (target-list)
  "Tupelize cdr of TARGET-LIST."
  (progn
    (setcdr target-list (coverlay-create-tuple-pairs (cdr target-list)))
    target-list))

(defun coverlay-create-tuple-pairs (even-list)
  "Convert (foo bar baz hoge) to ((foo bar) (baz hoge)) in EVEN-LIST."
  (let ((result '()))
    (while even-list
      (setq result (cons (list (car even-list) (car (cdr even-list))) result))
      (setq even-list (nthcdr 2 even-list)))
    (nreverse result)))

(defun coverlay-parse-buffer (buffer)
  "Parse BUFFER to alist.  car of each element is filename, cdr is segment of lines."
  (let (alist filename)
    (with-current-buffer buffer
      (while (not (eobp))
        (let ((current-line (coverlay-current-line)))
         (when (coverlay-source-file-p current-line)
           (setq filename (coverlay-extract-source-file current-line))
           (when (not (assoc filename alist))
             ;; (print (format "segments for %s does not exist" filename))
             (setq alist (cons (list filename) alist))))
         (when (coverlay-data-line-p current-line)
           (let* ((cols (coverlay-extract-data-list current-line))
                  (lineno (nth 0 cols))
                  (count (nth 1 cols)))
             (when (= count 0)
               ;; (print (format "count %d is zero" count))
               (coverlay-handle-uncovered-line alist filename lineno)))))
        (forward-line 1))
      alist)))

(defun coverlay-handle-uncovered-line (alist filename lineno)
  "Add uncovered line at LINENO in FILENAME to ALIST."
  (let* ((file-segments (assoc filename alist))
         (segment-list-body (cdr file-segments)))
    (if (not segment-list-body)
        (setcdr file-segments (list lineno lineno))
      (if (= (car segment-list-body) (- lineno 1))
          (setcar segment-list-body lineno)
        (setcdr file-segments (append (list lineno lineno) segment-list-body))))))

(defun coverlay-current-line ()
  "Get current line of current buffer."
  (buffer-substring (point)
                    (save-excursion
                      (end-of-line)
                      (point))))

(defun coverlay-source-file-p (line)
  "Predicate if LINE contains lcov source data (SF)."
  (coverlay-string-starts-with line "SF:"))

(defun coverlay-data-line-p (line)
  "Predicate if LINE contains lcov line coverage data (DA)."
  (coverlay-string-starts-with line "DA:"))

(defun coverlay-end-of-record-p (line)
  "Predicate if LINE contains lcov end marker (end_of_record)."
  (coverlay-string-starts-with line "end_of_record"))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
(defun coverlay-string-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun coverlay-extract-source-file (line)
  "Extract file name from lcov source LINE."
  (coverlay-extract-rhs line))

(defun coverlay-extract-data-list (line)
  "Extract data list from lcov line coverage LINE."
  (mapcar 'string-to-number (split-string (coverlay-extract-rhs line) ",")))

(defun coverlay-extract-rhs (line)
  "Extract right hand lcov value from LINE."
  (substring line (+ (string-match "\:" line) 1)))


;;
;; command: coverlay-toggle-overlays

;;;###autoload
(defun coverlay-toggle-overlays (buffer)
  "Toggle coverage overlay in BUFFER."
  (interactive (list (current-buffer)))
  (if (not coverlay-alist)
      (message "coverlay.el: Coverage data not found. Please use `coverlay-load-file` to load them.")
    (with-current-buffer buffer
      (coverlay-toggle-overlays-current-buffer))))

(defun coverlay-toggle-overlays-current-buffer ()
  "Toggle overlay in current buffer."
  (if (coverlay-overlay-exists-p)
      (coverlay-clear-cov-overlays)
    (coverlay-overlay-current-buffer)))

(defun coverlay-overlay-buffer (buffer)
  "Overlay BUFFER."
  (with-current-buffer buffer
    (coverlay-overlay-current-buffer)))

(defun coverlay-overlay-current-buffer ()
  "Overlay current buffer."
  (coverlay-clear-cov-overlays)
  (coverlay-overlay-current-buffer-with-list
     (coverlay-stats-tuples-for (current-buffer) coverlay-alist)))

(defun coverlay-overlay-exists-p ()
  "Predicate if coverlay overlays exists in current buffer."
  (coverlay-overlay-exists-in-list-p (overlays-in (point-min) (point-max))))

(defun coverlay-overlay-exists-in-list-p (ovl-list)
  "Predicate if coverlay overlays exists in OVL-LIST."
  (catch 'loop
    (dolist (ovl ovl-list)
      (if (overlay-get ovl 'coverlay) (throw 'loop t) nil)
      nil)))

(defun coverlay-clear-cov-overlays ()
  "Clear all coverlay overlays in current buffer."
  (remove-overlays (point-min) (point-max) 'coverlay t))

(defun coverlay-overlay-current-buffer-with-list (tuple-list)
  "Overlay current buffer acording to given TUPLE-LIST."
  (save-excursion
    (goto-char (point-min))
    (dolist (ovl (coverlay-map-overlays tuple-list))
      (progn
        (overlay-put ovl 'face (cons 'background-color coverlay:untested-line-background-color))
        (overlay-put ovl 'coverlay t)
        ))))

(defun coverlay-map-overlays (tuple-list)
  "make-overlay for each of a TUPLE(two line-numbers) LIST."
  (mapcar 'coverlay-make-overlay tuple-list))

(defun coverlay-make-overlay (tuple)
  "Make overlay for values in TUPLE."
  (make-overlay (point-at-bol (car tuple)) (point-at-eol (cadr tuple))))

(defun coverlay-stats-tuples-for (buffer stats-alist)
  "Construct tuple for BUFFER and data in STATS-ALIST."
  (cdr (assoc (expand-file-name (buffer-file-name buffer)) stats-alist)))

(defun coverlay--get-filenames ()
  "Return all filenames from current lcov file."
  (mapcar #'car coverlay-alist))

(defun coverlay-overlay-all-buffers (filename)
  "Overlay all buffers visiting FILENAME."
  (let ((buffers (find-buffer-visiting filename)))
    (when buffers
        (message (format "Marking buffers: %s" buffers))
        (coverlay-overlay-buffer buffers)
        filename)))

(defun coverlay-clear-all-buffers (filename)
  "Clear all buffers visiting FILENAME."
  (let ((buffers (find-buffer-visiting filename)))
    (when buffers
        (message (format "Clearing buffers: %s" buffers))
        (with-current-buffer buffers
          (when (coverlay-overlay-exists-p)
            (coverlay-clear-cov-overlays)))
        filename)))

(defun coverlay--overlay-all-buffers ()
  "Toggle all overlays in open buffers contained in alist."
  (mapcar (lambda (file)
          (coverlay-overlay-all-buffers file))
        (coverlay--get-filenames)))

(defun coverlay--clear-all-buffers ()
  "Clear all overlays in open buffers contained in alist."
  (mapcar (lambda (file)
          (coverlay-clear-all-buffers file))
        (coverlay--get-filenames)))

;;(coverlay--overlay-all-buffers)

;;;###autoload
(define-minor-mode coverlay-mode
  "overlays for uncovered lines"
  :lighter " lcov"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c ll") 'coverlay-toggle-overlays)
            (define-key map (kbd "C-c lf") 'coverlay-load-file)
            map)
  (coverlay--switch-mode coverlay-mode))

(defun coverlay--switch-mode (enabled)
  "Switch global mode to be ENABLED or not."
  ;; missing: restore overlay state
  (if enabled
      (progn
        (add-hook 'coverlay-mode-hook #'coverlay--update-buffers)
        (add-hook 'find-file-hook #'coverlay-file-load-callback))
    ;; cleanup
    (coverlay-end-watch)
    (remove-hook 'find-file-hook #'coverlay-file-load-callback)
    (remove-hook 'coverlay-mode-hook #'coverlay--update-buffers)
    (coverlay--clear-all-buffers)))

;;(setq coverlay-mode-hook nil)

(defun coverlay--update-buffers ()
  "Update all buffers to current mode state."
  (mapcar (lambda (file)
            (if coverlay-mode
                (coverlay-overlay-all-buffers file)
              (coverlay-clear-all-buffers file)))
        (coverlay--get-filenames)))


(provide 'coverlay)
;;; coverlay.el ends here

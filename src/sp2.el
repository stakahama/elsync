

;; Purpose of this script is to sync two directories
;;   written for the purpose of transferring files from SP2 to external HD
;;   this program exists in its present form because but cygwin, rsync, or python
;;   was not installed but emacs was available on a internet-less system
;;   in a trailer in Tijuana
;;
;; Description
;;   conditions:
;;   1. file does not exist
;;   2. file is newer
;;   3. file is not same size (incomplete transfer)
;;
;; Written and used Summer 2010 as 'sp2sync' and 'checkfiles'
;; Modified from original version in 2016 to separate general functions
;;   and sp2-specific functions (untested)

;;;_* ---------- Functions ----------

(load (concat
       (expand-file-name (file-name-directory load-file-name))
       "transfer"))

;;;_ . print times

;;;_  : helpers

(defun select-files (testfn dirname)
  (remove-if-not testfn (directory-files dirname)))

(defun get-file-time (x)
  (nth 5 (file-attributes x)))

(defun print-min (description time)
  (flet ((diff-in-min (a b) (/ (- (time-to-seconds a) (time-to-seconds b))
                              60.0)))
    (print (format "%s: last file written %.1f minutes ago"
                   description (diff-in-min (current-time) time)))))

;;;_  : top level

(defun last-sp2bfile (dirname)
  (let* ((current-date (let ((d (decode-time (current-time))))
			 (format "%04d%02d%02d" (nth 5 d) (nth 4 d) (nth 3 d))))
	 (today (path-join dirname current-date))
	 (sp2bfiles (select-files
		     '(lambda (x) (equal "sp2b" (file-name-extension x)))
		     today))
	 (lastfile (last-atom sp2bfiles)))
    (get-file-time (path-join today lastfile))))

(defun last-flowfile (dirname)
  (let* ((flowfiles (select-files
		     '(lambda (x) (string-match "flows\\.txt" x))		     
		     dirname))
	 (lastfile (last-atom flowfiles)))
    (get-file-time (path-join dirname flowfile))))

(defun check-last-files ()
  (print-min "SP2" (last-sp2bfile "C:/SP2/Data"))
  (print-min "FTIR" (last-flowfile "C:/CalMex/FlowData")))

;;;_ . sync

;;;_  : helpers

(defun dir2sec (dirname)
  "dirname: expects directory name in YYYYmmdd format"
  (flet ((directory-date (dirname)
			(let (year month day)
			  (setq year (substring dirname 0 4))
			  (setq month (substring dirname 4 6))
			  (setq day (substring dirname 6 8))
			  (mapcar 'string-to-number (list day month year))))
	 (fulldate (day-month-year)
		   (apply 'encode-time 
			  (reduce 'append (list '(0 0 0) 
						day-month-year 
						'(nil t nil))))))
	 (time-to-seconds (fulldate (directory-date dirname)))))

(defun sort-by-date-increasing (x y)
    (or (< (dir2sec x) (dir2sec y))
	(string-lessp x y)))

;;;_  : top level

(defun sync-all ()
  ;; sync flowrate data (folders contain text files only)
  (sync "C:/CalMex/FlowData" "D:/CalMex/FlowData")
  ;; sync log files (folders contain text files only)
  (sync "C:/CalMex/notes" "D:/CalMex/notes")
  ;; sp2 files
  (transfer "C:/SP2/Data" "D:/CalMex/SP2/Data" 'sort-by-date-increasing))

;;;_* ---------- Application (example usage) ----------

;;;_ . in emacs

;;(sync "C:/SP2/Data/20100512" "D:/SP2/Data/20100512")
;;(syncall)

;;;_ . batch mode

;; C:\emacs\bin\emacs -batch -l C:\emacs\site-lisp\transfer.el --eval "(sync \"C:/CalMex/notes\" \"D:/CalMex/notes\")"
;; C:\emacs\bin\emacs -batch -l C:\emacs\site-lisp\sp2.el --eval "(syncall)"


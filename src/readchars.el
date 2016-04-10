
;; read flow file

(require 'cl)

(defun extract (elems seq)
  (mapcar (lexical-let ((seq seq))
	    (lambda (x) (nth x seq)))
	  elems))

(defun get-datetime (elems)
  (let (date time)
    (setq date (mapconcat 'identity (extract (number-sequence 0 2) elems) "-"))
    (setq time (concat (mapconcat 'identity
				  (extract (number-sequence 3 4) elems) ":")
		       ":" (format "%02d" (round (string-to-number
						  (nth 5 elems))))
		       " " (nth 6 elems)))
    (concat date " " time)))

(defun process-line (txt)
  (let (elems datetime flow-a flow-b (a 11) (b 17))
    (setq elems (split-string txt))
    (setq datetime (get-datetime elems))
    (setq flow-a (nth a elems))
    (setq flow-b (nth b elems))
    (concat datetime "\t" flow-a "\t" flow-b)))

(defun princ-with-newline (x)
  (princ x)
  (princ "\n"))

(defun parsetxt (inputFile)
  (let (headerfields beginning line datarow readfrom moreLines x)
    (setq headerfields '("POSIXtime" "A_channel_SLPM" "B_channel_SLPM"))
    (princ-with-newline (mapconcat 'identity headerfields "\t"))
    (find-file inputFile)
    (goto-char (point-min))
    (setq readfrom (current-buffer))
    (setq moreLines t)
    (setq x 0)
    (while moreLines
      (setq beginning (point))
      (move-end-of-line 1)
      (setq line (buffer-substring-no-properties beginning (point)))
      (if (not (= 0 (length line)) )
          (princ-with-newline (process-line line)))
      (setq x (1+ x))
      (setq moreLines (= 0 (forward-line 1))))
    (kill-buffer readfrom)))

;; example:
;;
;; (parsetxt "data-example/20100518flows.txt")

;; called from R:
;;
;; ReadParsed <- function(filename) {
;;   app <- "/Applications/Emacs.app/Contents/MacOS/Emacs"
;;   fn <- "./readchars.el"
;;   cmd <- sprintf("%s -batch -l %s --eval '(parsetxt \"%s\")'",
;;                  app, fn, filename)
;;   read.delim(pipe(cmd), colClasses=c("character",rep("numeric",2)))
;; }
;;
;; ReadParsed("data-example/20100518flows.txt")

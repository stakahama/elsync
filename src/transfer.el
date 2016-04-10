
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

;;;_* --- Common Lisp extensions ---

(require 'cl)

;;;_* --- operating sys functions ---

(defun path-join (dirname filename)
  (concat (file-name-as-directory dirname) filename))

(defun last-atom (x)
  (car (reverse x)))

;;;_* --- copying functions ---

(defun docopy (filename)
  "dirname1 and dirname2 are dynamically scoped"
  (message (format "Copying %s ..." filename))
  (copy-file (path-join dirname1 filename) 
	     (path-join dirname2 filename) t t)
  (message "Done.")
  nil)

(defun docopy-p (filename)
  (flet ( (nbytes (f) (nth 7 (file-attributes f))) )
    (let ( (x (path-join dirname1 filename))
	   (y (path-join dirname2 filename)) )
      (or (not (file-exists-p y))
	  (file-newer-than-file-p x y)
	  (> (nbytes x) (nbytes y))))))

(defun is-dot-p (filename) 
  (equal "." (substring filename 0 1)))

;;;_* --- main syncing function, revised ---

(defun sync (dirname1 dirname2)
  "args: string, string"
    (let (filelist copythese)
      (setq filelist (remove-if 'is-dot-p (directory-files dirname1)))
      (setq copythese (remove-if-not 'docopy-p filelist))
      (mapc 'docopy copythese)))

;;;_* --- move/copy subfolders ---

(defun transfer (local remote &optional compfn)
  "sync subfolders"
  (lexical-let ((local local)
		(remote remote))
    (flet ((copydir (dirname)
		    (let (dirname-local dirname-remote)
		      (setq dirname-local (path-join local dirname)
			    dirname-remote (path-join local dirname))
		      (if (not (file-exists-p dirname-remote))
			  (make-directory dirname-remote))
		      (sync dirname-local dirname-remote)))
	   (movedir (dirname)
		    (let (fulldir)
		      (copydir dirname)
		      (setq fulldir (path-join local dirname))
		      (message "Deleting directory %s ..." fulldir)
		      (dired-delete-file fulldir 'always)
		      (message "Done.")
		      nil)))
      (let (subdirs)
	(if (not compfn) ;; comparison function
	    (setq compfn 'string-lessp))
	(setq subdirs (reverse (sort
				(remove-if 'is-dot-p (directory-files local))
				'compfn)))
	(mapc 'movedir (rest subdirs))
	(copydir (first subdirs))
	nil))))

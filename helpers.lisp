(in-package #:pjs-json)

(defun x-form-p (sym form)
  (and (listp form)
       (eq (first form)
	   sym)))

(defun quoted-symbol-p (form)
  (and (x-form-p 'quote form)
       (symbolp (second form))))

(defun json-form-p (form)
  (and (listp form)
       (member (first form)
	       '(json-obj
		 json-arr
		 json-val))))

(defun json-write-form-p (form)
  (and (x-form-p 'write-string form)
       (eq (third form)
	   'json-stream)))

(defun %remap-sym (sym)
  (case sym
    (json-arr
      '%json-arr)
    (json-obj
      '%json-obj)
    (json-val
      '%json-val)
    (json-k-v
      '%json-k-v)
    (t
     (error "bad symbol to % remap ~w" sym))))

(defun %remap-form (form)
  `(,(%remap-sym (first form))
    ,@ (rest form)))

(defun insert-separator (sep list)
  (let (seen)
    (dolist-c (el list)
      (if seen
	  (collect sep)
	  ;; else
	  (setf seen t))
      (collect el))))

(defun insert-separator* (sep list)
  (let (seen)
    (dolist-c (el list)
      (if seen
	  (collect sep)
	  ;; else
	  (setf seen t))
      (dolist (el el)
	(collect el)))))


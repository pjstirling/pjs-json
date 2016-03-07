(in-package #:pjs-json)

(macrolet ((global-macros (&rest syms)
	     `(progn
		,@ (mapcar (lambda (sym)
			     `(defmacro ,sym (&body body)
				(declare (ignore body))
				(error ,(format nil "misuse of global ~w macro" sym))))
			   syms))))
  (global-macros json-arr json-obj json-val))

(defmacro json-k-v (k v)
  (declare (ignore k v))
  (error "misuse of global JSON-K-V macro"))

(defmacro merge-writes (&body body &environment env)
  (flet ((msg (&rest args)
	   (declare (ignore args))))
    (let ((body (with-collector (collect)
		  (labels ((walk (form)
			     (let ((form (macroexpand form env)))
			       (if (x-form-p 'progn form)
				   (mapcar #'walk (rest form))
				   ;; else
				   (collect form)))))
		    (mapcar #'walk body)))))
      (msg "after expansion body is ~%~w~%" body)
      (bind ((constant "")
	     (body (with-collector (collect)
		     (flet ((emit-constant ()
			      (when (< 0 (length constant))
				(msg "emitting constant ~w~%" constant)
				(collect `(write-string ,constant json-stream))
				(setf constant ""))))
		       (dolist (form body)
			 (if (and (json-write-form-p form)
				  (stringp (second form)))
			     (progn
			       (msg "constant is ~w~%" constant)
			       (setf constant
				     (sconc constant (second form))))
			     ;; else
			     (progn
			       (emit-constant)
			       (collect form))))
		       (emit-constant)))))
	`(progn
	   ,@body)))))

(defmacro defmacro* (name (&rest args) &body body)
  `(defmacro ,name ,args
     (flet ((msg (str &rest args)
	      (apply #'format
		     t
		     (sconc ,(sconc "~&" (symbol-name name) ": ")
			    str)
		     args)))
       (msg "body before ~%~W~%" body)
       ,(bind ((:symbols result))
	  `(let ((,result ,@body))
	     (msg "result body ~%~W~%" ,result)
	     ,result)))))

(defmacro with-json-binds (&body body)
  (let ((form (first body)))
    (if (and (null (rest body))
	     (json-form-p form))
	(%remap-form form)
	;; else
	(error "misuse: must have lone JSON form as child ~w" body))))

(defmacro with-json-stream (stream &body body)
  `(with-open-stream (json-stream ,stream)
     (with-json-binds
       ,@body)))

(defmacro with-json-output-to-string (&body body)
  `(with-output-to-string (json-stream)
     (with-json-binds
       ,@body)))

(defmacro slash (ch)
  (let ((str (make-string 2)))
    (setf (char str 0)
	  #\\)
    (setf (char str 1)
	  ch)
    `(write-string ,str json-stream)))

(defun json-str (str json-stream)
  (write-char #\" json-stream)
  (dovector (ch str)
    (case ch
      (#\"
       (slash #\"))
      (#\\
       (slash #\\))
      ;; control codes
      (#\Backspace
       (slash #\b))
      (#\Tab
       (slash #\t))
      (#\Newline
       (slash #\n))
      (#\Vt
       (slash #\f))
      (#\Return
       (slash #\r))
      (t
       ;; any utf char can be output as \uXXXX (where X is a hex digit) but this uses 6 bytes
       ;; which is worse than even utf-8 for the very worst case, so restrict it to chars that
       ;; don't print pleasantly
       ;; 
       ;; most of the codes below 32 that aren't covered above probably don't belong in
       ;; strings, because they aren't printable. but, for completeness
       ;;
       ;; other ranges are probably also worth doing if they don't have good font support
       ;;
       ;; on the other hand, though: should we really cater for humans mentally parsing
       ;; our output?
       (let ((code (char-code ch)))
	 (if (<= code 32)
	     (format json-stream "\\u~4,'0x" code)
	     ;; else
	     (write-char ch json-stream))))))
  (write-char #\" json-stream))

(defmacro %json-val (form)
  (cond
    ((or (numberp form)
	 (stringp form)
	 (eq t form)
	 (null form)
	 (vectorp form)
	 (keywordp form)
	 (hash-table-p form))
     `(write-string ,(with-output-to-string (s)
		       (%%json-val form s))
		    json-stream))
    ((quoted-symbol-p form)
     `(write-string ,(with-output-to-string (s)
		       (%%json-val (second form) s))
		    json-stream))
    (t
     `(%%json-val ,form json-stream))))

(defmacro with-remapped-array-forms (&body body)
  (bind ((:symbols seen))
    (flet ((remap-form (sym)
	     `(,sym (&body body)
		    `(progn
		       (if ,',seen
			   ,+comma-separator+
			   ;; else
			   (setf ,',seen t))
		       (,',(%remap-sym sym) ,@body)))))
      `(let (,seen)
	 (macrolet ,(mapcar #'remap-form '(json-arr json-obj json-val))
	   ,@body)))))

(defmacro %json-arr (&body body)
  `(merge-writes
     (write-string "[" json-stream)
     ,@(if (every #'json-form-p body)
	   (insert-separator +comma-separator+
			     (mapcar #'%remap-form body))
	   ;; else
	   `((with-remapped-array-forms
	       ,@body)))
     (write-string "]" json-stream)))

(defmacro %as-string (form)
  (cond
    ((or (stringp form)
	 (keywordp form)
	 (quoted-symbol-p form))
     `(%json-val ,form))
    ((numberp form)
     `(%json-val ,(princ-to-string form)))
    (t
     (bind ((:symbols place))
       `(let ((,place ,form))
	  (if (or (stringp ,place)
		  (symbolp ,place))
	      (%%json-val ,place json-stream)
	      ;; else
	      (%%json-val (princ-to-string ,place)
			  json-stream)))))))

(defmacro %json-obj (&body body)
  `(merge-writes
     (write-string "{" json-stream)
     ,@(if (every (lambda (form)
		    (x-form-p 'json-k-v form))
		  body)
	   (insert-separator +comma-separator+
			     (mapcar (lambda (form)
				       (destructuring-bind (k v)
					   (rest form)
					 `(progn
					    (%as-string ,k)
					    ,+colon-separator+
					    ,(if (json-form-p v)
						 (%remap-form v)
						 ;; else
						 `(%json-val ,v)))))
				     body))
	   ;; else
	   (bind ((:symbols seen))
	     `((let (,seen)
		 (macrolet ((json-k-v (k v)
			      `(merge-writes
				 (if ,',seen
				     ,+comma-separator+
				     ;; else
				     (setf ,',seen t))
				 (%as-string ,k)
				 ,+colon-separator+
				 ,(if (json-form-p v)
				      (%remap-form v)
				      ;; else
				      `(%json-val ,v)))))
		   ,@body)))))
     (write-string "}" json-stream)))

(defun %%json-val (form json-stream)
  (case form
    ((t)
     (write-string "true" json-stream))
    (t
     (etypecase form
       (null
	(write-string "null" json-stream))
       ;; must come before vector
       (string
	(json-str form json-stream))
       (vector
	(%json-arr
	  (dovector (el form)
	    (json-val el))))
       (hash-table
	(%json-obj
	  (maphash (lambda (k v)
		     (json-k-v k v))
		   form)))
       (number
	(princ form json-stream))
       (symbol
	(write-char #\" json-stream)
	(write-string (ps:symbol-to-js-string form)
		      json-stream)
	(write-char #\" json-stream))))))


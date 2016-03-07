(in-package #:pjs-json)

(define-condition parser-error (error)
  ((format :initarg :format
	   :reader parser-error-format)
   (args :initarg :args
	 :reader parser-error-args))
  (:report (lambda (c stream)
	     (apply #'format stream (parser-error-format c) (parser-error-args c)))))

(defun parser-error (format &rest args)
  (signal (make-condition 'parser-error
			  :format format
			  :args args)))

(defun error-position-string (str at)
  (let* ((len (length str))
	 (start (max 0
		     (- at 10)))
	 (end (min len
		   (+ at 10)))
	 (delta (- at start))
	 (substr (subseq str start end)))
    (with-output-to-string (s)
      (print substr
	     s)
      (terpri s)
      (dotimes (i (1+ delta))
	(when (and (< i (length substr))
		   (member (char substr i)
		       '(#\\ #\")))
	  (write-char #\Space s))
	(write-char #\Space s))
      (write-char #\^ s))))

(defmacro parse-ecase (form &body clauses)
  (flet ((expand-match (match)
	   (if (listp match)
	       (if (eq (first match)
		       'range)
		   (bind ((:db (bottom top)
			       (rest match))
			  (bottom-code (char-code bottom)))
		     (dotimes-c (i (- (1+ (char-code top))
				      bottom-code))
		       (collect (code-char (+ bottom-code i)))))
		   ;; else
		   match)
	       ;; else
	       (list match))))
    (bind ((:symbols form-sym)
	   possibles
	   (clauses (mapcar (lambda (clause)
			      (bind ((:db (match &rest body)
					  clause)
				     (match (expand-match match)))
				(setf possibles
				      (append possibles match))
				`(,(if (rest match)
				       `(member ,form-sym ',match)
				       ;; else
				       `(eql ,form-sym ,(first match)))
				  ,@body)))
			    clauses)))
      `(let ((old-index index)
	     (,form-sym ,form))
	 (cond
	   ,@clauses
	   (t
	    (parser-error ,(format nil "Unexpected '~~w' at index ~~a~~%Expected one of ~a~~%~~a" possibles)
			  ,form-sym
			  old-index
			  (error-position-string str old-index))))))))

(defun digit-to-number (ch)
  (unless (digit-char-p ch)
    (error "out of range ~w" ch))
  (- (char-code ch)
     (char-code #\0)))

(defun hash (&rest args)
  (let ((result (make-hash-table :test 'equal)))
    (while args
      (setf (gethash (pop args)
		     result)
	    (pop args)))
    result))

(defun json-parse (str)
  (let ((len (length str))
	(index 0))
    (labels ((peek ()
	       (if (< index len)
		   (char str index)
		   ;; else
		   'end-of-file))
	     ;;
	     (ch ()
	       (prog1
		   (peek)
		 (unless (= index len)
		   (incf index))))
	     ;;
	     (skip-ws ()
	       (while (and (< index len)
			   (member (peek)
				   '(#\Space #\Return #\Newline #\Tab)))
		 (incf index)))
	     ;;
	     (parse-array ()
	       (ch)
	       (let ((result (make-array 5 :adjustable t :fill-pointer 0)))
		 (if (eql (peek) #\])
		     (incf index)
		     ;; else
		     (progn
		       (vector-push-extend (parse-form)
					   result)
		       (while t
			 (skip-ws)
			 (parse-ecase (ch)
			   (#\,
			    (skip-ws)
			    (vector-push-extend (parse-form)
						result))
			   (#\]
			    (return))))))
		 result))
	     ;;
	     (expect (pattern)
	       (dotimes (i (length pattern))
		 (let* ((expected (char pattern i))
			(old-index index)
			(got (ch)))
		   (unless (eql got expected)
		     (parser-error "Unexpected '~w' at index ~a, expected '~w'~%~a"
				   got
				   old-index
				   expected
				   (error-position-string str old-index))))))
	     ;;
	     (parse-k-v (hash)
	       (let ((key (parse-string)))
		 (skip-ws)
		 (expect ":")
		 (skip-ws)
		 (setf (gethash key hash)
		       (parse-form))))
	     ;;
	     (parse-hash ()
	       (let ((result (hash)))
		 (ch)
		 (skip-ws)
		 (parse-ecase (peek)
		   (#\"
		    (parse-k-v result)
		    (while t
		      (skip-ws)
		      (parse-ecase (ch)
			(#\,
			 (skip-ws)
			 (parse-k-v result))
			(#\}
			 (return)))))
		   (#\}
		    (ch)))
		 result))
	     ;;
	     (digit-char-p* (c)
	       (and (characterp c)
		    (digit-char-p c)))
	     ;;
	     (ch-digit ()
	       (let ((c (peek)))
		 (when (digit-char-p* c)
		   (incf index)
		   (digit-to-number c))))
	     ;;
	     (parse-positive-integer ()
	       (bind ((digit (ch))
		      (int (parse-ecase digit
			     ((range #\0 #\9)
			      (digit-to-number digit)))))
		 (while (setf digit (ch-digit))
		   (setf int
			 (+ digit
			    (* 10 int))))
		 int))
	     (parse-number ()
	       (let ((mantissa 0))
		 (parse-ecase (peek)
		   (#\0
		    (incf index)
		    (when (digit-char-p* (peek))
		      (parser-error "Unexpected '~w' at index ~a. JSON specification forbids leading zeroes in numeric constants ~%~a"
				    (peek)
				    index
				    (error-position-string str index))))
		   ((range #\1 #\9)
		    (setf mantissa
			  (parse-positive-integer))))
		 (when (eql (peek) #\.)
		   (incf index)
		   (bind ((start index)
			  (fraction (parse-positive-integer))
			  (delta (- index start))
			  (scale (expt 10 delta)))
		     (setf mantissa
			   (+ mantissa
			      (/ fraction scale)))))
		 (if (member (peek) '(#\e #\E))
		     (progn
		       (incf index)
		       (bind ((sign (parse-ecase (peek)
				      (#\+
				       (incf index)
				       1)
				      (#\-
				       (incf index)
				       -1)
				      ((range #\0 #\9)
				       1)))
			      (exponent (parse-positive-integer)))
			 (* mantissa
			    (expt 10
				  (* sign exponent)))))
		     ;; else
		     mantissa)))
	     (parse-string ()
	       (let ((result (make-array 32 :element-type 'character :adjustable t :fill-pointer 0))
		     (start index)
		     c)
		 (incf index)
		 (while t
		   (unless (< index len)
		     (parser-error "Unexpected 'END-OF-FILE' inside string, at index ~a.~%~a~%Started at index ~a~%~a"
				   index
				   (error-position-string str index)
				   start
				   (error-position-string str start)))
		   (vector-push-extend (case (setf c (ch))
					 (#\"
					  (return))
					 (#\\
					  (parse-ecase (ch)
					    (#\"
					     #\")
					    (#\\
					     #\\)
					    (#\/
					     #\/)
					    (#\b
					     #\Backslash)
					    (#\t
					     #\Tab)
					    (#\n
					     #\Newline)
					    (#\f
					     #\Vt)
					    (#\r
					     #\Return)
					    (#\u
					     (let ((code 0))
					       (dotimes (i 4)
						 (setf code
						       (+ (parse-ecase (setf c (ch))
							    ((range #\0 #\9)
							     (digit-to-number c))
							    ((range #\a #\f)
							     (+ 10
								(- (char-code c)
								   (char-code #\a))))
							    ((range #\A #\F)
							     (+ 10
								(- (char-code c)
								   (char-code #\A)))))
							  (* 16 code))))
					       (code-char code)))))
					 (t
					  c))
				       result))
		 (coerce result 'string)))
	     (parse-form ()
	       (parse-ecase (peek)
		 (#\-
		  (incf index)
		  (* -1 (parse-number)))
		 ((range #\0 #\9)
		  (parse-number))
		 (#\"
		  (parse-string))
		 (#\{
		  (parse-hash))
		 (#\[
		  (parse-array))
		 (#\t
		  (expect "true")
		  t)
		 (#\f
		  (expect "false")
		  nil)
		 (#\n
		  (expect "null")
		  nil))))
      (values (parse-form)
	      (= index len)
	      index
	      len))))

(defun test-error-position-string ()
  (let ((str "alpha bravo charlie delta echo\\\" foxtrot gamma hotel india joker kilo lima"))
    (dotimes (i (length str))
      (format t "~a~%" (error-position-string str i)))))

(defun json-to-string (form)
  (with-json-output-to-string
    (json-val form)))

(defun hash-table-keys (hash)
  (with-collector (collect)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (collect k))
	     hash)))

(defun json= (a b)
  (if (eq a t)
      (eq b t)
      ;; else
      (etypecase a
	(string
	 (and (stringp b)
	      (string= a b)))
	(vector
	 (and (vectorp b)
	      (= (length a)
		 (length b))
	      (dotimes (i (length a) t)
		(unless (json= (aref a i)
			       (aref b i))
		  (return nil)))))
	(hash-table
	 (and (hash-table-p b)
	      (let ((keys (hash-table-keys a)))
		(when (equal keys
			     (hash-table-keys b))
		  (dolist (key keys t)
		    (unless (json= (gethash key a)
				   (gethash key b))
		      (return nil)))))))
	(number
	 (and (numberp b)
	      (= a b)))
	(null
	 (null b)))))

(defun test-json-parse ()
  (let ((success 0)
	(fail 0))
    (macrolet ((y (str result)
		 `(block nil
		    (handler-bind ((parser-error (lambda (c)
						   (format t "~&UF failure parsing ~w~%" ,str)
						   (pprint-logical-block (*standard-output*
									  nil
									  :per-line-prefix "    ")
						     (format t  "message was ~a"
							     (apply #'format
								    nil
								    (parser-error-format c)
								    (parser-error-args c))))
						   (incf fail)
						   (return))))
		      (let ((parsed (json-parse ,str)))
			(if (json= parsed ,result)
			    (incf success)
			    ;; else
			    (format t "~&M failure parsing ~w~&   got result ~w instead of ~w"
				    ,str
				    (json-to-string parsed)
				    (json-to-string ,result)))))))
	       (n (str)
		 `(block nil
		    (handler-bind ((parser-error (lambda (c)
						   (declare (ignore c))
						   (incf success)
						   (return))))
		      (multiple-value-bind (parsed complete full)
			  (json-parse ,str)
			(format t "~&US failure to signal error when parsing ~w~&   got result ~w~%   parser reached index ~a/~a"
				,str
				parsed
				complete
				full)
			(incf fail))))))
      (y "true" t)
      (y "null" nil)
      (y "false" nil)
      ;(y "t" t)
      ;(y "true" nil)
      (y "{}" (hash))
      (y "{\"foo\":1}" (hash "foo" 1))
      (y "[]" (vector))
      (y "[1,2, 3]" (vector 1 2 3))
      (y "1" 1)
      (y "-1" -1)
      (y "1.5" 1.5)
      (y "1.25e-1" 1.25e-1)
      
      (n "t")
      (n "{")
      (n "[")
      (n "01")
      (n "1.")
      (n "1e")
      (format t "~&ran ~a tests with ~a failures~%" (+ success fail) fail))))

(defpackage #:pjs-json
  (:use #:cl #:pjs-utils)
  (:export #:with-json-stream
	   #:with-json-output-to-string
	   #:json-val
	   #:json-arr
	   #:json-obj
	   #:json-k-v

	   #:json-parse))

(in-package #:pjs-json)

(defparameter +comma-separator+
  '(write-string "," json-stream))

(defparameter +colon-separator+
  '(write-string ":" json-stream))


;;;; package.lisp

(defpackage #:cl-cloudstack-client-integration
  (:use #:cl
	#:cl-cloudstack-client
	#:lisp-unit)
  (:export *cloudstack-uri*))

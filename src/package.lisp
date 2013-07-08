;;;; package.lisp

(defpackage #:cl-cloudstack-client
  (:use #:cl)
  (:export cloudstack-client
	   make-cloudstack-client
	   sign-request

	   ;; API
	   list-service-offerings
	   ))


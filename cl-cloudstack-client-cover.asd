;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-cloudstack-client-cover.asd
;;;; Purpose:       cl-cloudstack-client code coverage.
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************


(asdf:defsystem #:cl-cloudstack-client-cover
  :serial t
  :description "Code coverage for the cl-cloudstack-client."
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :depends-on (#:sb-cover #:cl-cloudstack-client-test)
  :components
  ((:module :test
	    :components ((:file "cover")))))



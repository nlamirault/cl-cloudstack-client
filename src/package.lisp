;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       cl-cloudstack-client package definition
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************


(defpackage #:cl-cloudstack-client
  (:use #:cl)
  (:export cloudstack-client
	   make-cloudstack-client
	   sign-request

	   cloudstack-error
	   cloudstack-request-error

	   ;; API
	   list-service-offerings
	   ))


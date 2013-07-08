;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cover.lisp
;;;; Purpose:       cl-cloudstack-client code coverage.
;;;; Programmer:    Nicolas Lamirault <nlamirault@gmail.com>
;;;;
;;;; *************************************************************************


(require :sb-cover)
(declaim (optimize sb-cover:store-coverage-data))

(ql:quickload "cl-cloudstack-client-test")

(lisp-unit:run-tests :all :cl-cloudstack-client-test)

(let ((output-dir (format nil "/tmp/cl-cloudstack-client/")))
  (ensure-directories-exist output-dir)
  (sb-cover:report output-dir))
     
(declaim (optimize (sb-cover:store-coverage-data 0)))

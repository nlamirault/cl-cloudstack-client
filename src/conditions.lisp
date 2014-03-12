;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          conditions.lisp
;;;; Purpose:       cl-cloudstack-client errors
;;;; Programmer:    Nicolas Lamirault <nicolas.lamirault@gmail.com>
;;;;
;;;; *************************************************************************



(in-package #:cl-cloudstack-client)

(define-condition cloudstack-error (simple-error)
  ()
  (:documentation "Main Cloudstack error."))


(define-condition cloudstack-request-error (cloudstack-error)
  ((code :reader code
         :initarg :code
         :documentation "The error code.")
   (message :reader message
            :initarg :message
            :documentation "Explanation message."))
  (:documentation "Cloudstack request error.")
  (:report (lambda (condition stream)
             (format stream "Cloudstack error ~A : ~A."
                     (code condition) (message condition)))))


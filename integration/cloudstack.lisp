;; Copyright (C) 2014  Nicolas Lamirault

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(in-package :cl-cloudstack-client-integration)

(defparameter *cloudstack-uri* nil)

(defparameter *user-api-key* nil)

(defparameter *user-secret-key* nil)

(defmacro with-cloudstack ((cloudstack) &body body)
  `(let ((,cloudstack
	  (make-cloudstack-client *cloudstack-uri*
				  *user-api-key*
				  *user-secret-key*)))
     ,@body))


(define-test cant-start-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cloudstack-call cloudstack "startVirtualMachine"))))

(define-test cant-stop-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cloudstack-call cloudstack "stopVirtualMachine"))))

(define-test cant-reboot-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cloudstack-call cloudstack "rebootVirtualMachine"))))

(define-test can-retrieve-all-hypervisors
  (with-cloudstack (cloudstack)
    (let* ((json (cloudstack-call cloudstack "listHypervisors"))
	   (response (find :listhypervisorsresponse json :key #'car)))
      (assert-equal 4 (cdr (find :count (cdr response) :key #'car)))
      (let* ((hypervisors (find :hypervisor (cdr response) :key #'car))
	     (values (cdr hypervisors)))
	(loop for name in '("KVM" "XenServer" "VMWare" "Ovm")
	   do (assert-true (find name values :key #'cdar :test 'string-equal)))))))

(define-test can-retrieve-all-hosts
  (with-cloudstack (cloudstack)
    (assert-true (cloudstack-call cloudstack "listHosts"))))

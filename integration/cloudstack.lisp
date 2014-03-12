

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

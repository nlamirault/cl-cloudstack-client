

(in-package :cl-cloudstack-client-test)

(defparameter *cloudstack-test-uri* nil)

(defparameter *user-api-key*
  "sHfqA41hDYeHH0oNpHAEA5tZzA-rT5houFJUfuXCa_jZMoUroJKSOxwBG5qivXCk0Zm9CjQ63-xKGzuqiHYQ_w")

(defparameter *user-secret-key*
  "jBtFUGUIKWRUzowcSg4VJ4VKNGatnaaH4R6fLV0T6DinPJssJJmLr2UWOQRgyCxZIsWmHYw362T64poyF3wMBA")


(defmacro with-cloudstack ((cloudstack) &body body)
  `(let ((,cloudstack 
	  (make-cloudstack-client *cloudstack-test-uri* *user-api-key* *user-secret-key*)))
     ,@body))
			   

(define-test can-sign-simple-request
  (with-cloudstack (cloudstack)
    (assert-equal (concatenate 'string 
			       "apikey="
			       *user-api-key*
			       "&command=listDomains"
			       "&response=json"
			       "&signature=celKqR3O6kN6viQeLHiizyCv97A%3D")
		  (sign-request cloudstack "listDomains"))))
		

(define-test can-sign-complex-request
  (with-cloudstack (cloudstack)
    (assert-equal (concatenate 'string
			       "account=EROS_ITG"
			       "&apikey="
			       *user-api-key*
			       "&command=listVirtualMachines&domainid=2"
			       "&response=json"
			       "&signature=c%2BRFr9bw2iv4OSYZdg1U0ZqkEpA%3D")
		  (sign-request cloudstack
				"listVirtualMachines" 
				'((:account "EROS_ITG") (:domainid "2"))))))

(define-test cant-start-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cl-cloudstack-client::start-virtual-machine cloudstack))))

(define-test cant-stop-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cl-cloudstack-client::stop-virtual-machine cloudstack))))

(define-test cant-reboot-vm-without-id
  (with-cloudstack (cloudstack)
    (assert-error 'cloudstack-request-error
		  (cl-cloudstack-client::reboot-virtual-machine cloudstack))))

(define-test can-retrieve-all-hypervisors
  (with-cloudstack (cloudstack)
    (let* ((json (cl-cloudstack-client::list-hypervisors cloudstack))
	   (response (find :listhypervisorsresponse json :key #'car)))
      (assert-equal 4 (cdr (find :count (cdr response) :key #'car)))
      (let* ((hypervisors (find :hypervisor (cdr response) :key #'car))
	     (values (cdr hypervisors)))
	(loop for name in '("KVM" "XenServer" "VMWare" "Ovm")
	   do (assert-true (find name values :key #'cdar :test 'string-equal)))))))

(define-test can-retrieve-all-hosts
  (with-cloudstack (cloudstack)
    (assert-true (cl-cloudstack-client::list-hosts cloudstack))))

;; (define-test can-retrieve-ssvm-connected
;;   (with-cloudstack (cloudstack)
;;     (let* ((json (cl-cloudstack-client::list-hosts cloudstack
;; 						   '((:type "SecondaryStorageVM")
;; 						     (:resourcestate "Enabled"))))
;; 	   (response (find :listhostsresponse json :key #'car)))
;;       (assert-equal 1 (cdr (find :count (cdr response) :key #'car))))))

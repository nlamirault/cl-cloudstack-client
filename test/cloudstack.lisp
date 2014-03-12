

(in-package :cl-cloudstack-client-test)

(defparameter *cloudstack-test-uri* nil)

(defparameter *user-api-key*
  "sHfqA41hDYeHH0oNpHAEA5tZzA-rT5houFJUfuXCa_jZMoUroJKSOxwBG5qivXCk0Zm9CjQ63-xKGzuqiHYQ_w")

(defparameter *user-secret-key*
  "jBtFUGUIKWRUzowcSg4VJ4VKNGatnaaH4R6fLV0T6DinPJssJJmLr2UWOQRgyCxZIsWmHYw362T64poyF3wMBA")


(defmacro with-cloudstack ((cloudstack) &body body)
  `(let ((,cloudstack
	  (make-cloudstack-client *cloudstack-test-uri*
				  *user-api-key*
				  *user-secret-key*)))
     ,@body))


(define-test can-sign-simple-request
  (with-cloudstack (cloudstack)
    (assert-equal (concatenate 'string
			       "apikey="
			       *user-api-key*
			       "&command=listDomains"
			       "&response=json"
			       "&signature=celKqR3O6kN6viQeLHiizyCv97A%3D")
		  (cl-cloudstack-client::sign-request cloudstack "listDomains"))))


(define-test can-sign-complex-request
  (with-cloudstack (cloudstack)
    (assert-equal (concatenate 'string
			       "account=EROS_ITG"
			       "&apikey="
			       *user-api-key*
			       "&command=listVirtualMachines&domainid=2"
			       "&response=json"
			       "&signature=c%2BRFr9bw2iv4OSYZdg1U0ZqkEpA%3D")
		  (cl-cloudstack-client::sign-request cloudstack
						      "listVirtualMachines"
						      '((:account "EROS_ITG")
							(:domainid "2"))))))

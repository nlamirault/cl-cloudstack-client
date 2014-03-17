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

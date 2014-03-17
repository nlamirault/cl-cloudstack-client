;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
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

(in-package #:cl-cloudstack-client)

(defparameter *debug* t)

(defclass cloudstack-client ()
  ((apikey :initform nil
	   :initarg :api-key
	   :accessor cloudstack-client-apikey
	   :documentation "API Key for the Cloudstack API.")
   (secretkey :initform nil
	      :initarg :secret-key
	      :accessor cloudstack-client-secretkey
	      :documentation "Secret Key for the Cloudstack API.")
   (uri :initform nil
	:initarg :uri
	:accessor cloudstack-client-uri
	:documentation "URI of the Cloudstack API."))
  (:documentation "A Cloudstack client."))

(defun make-cloudstack-client (uri apikey secretkey)
  "Creates a new Cloudstack client."
  (make-instance 'cloudstack-client
		 :uri uri :api-key apikey :secret-key secretkey))

;;
;; Tools
;;


(defun generate-params-uri (params-alist)
  "Generates the post content given an alist of parameters."
  (let ((content
	 (format nil "~{~A~^&~}"
		 (mapcar #'(lambda (arg-pair)
			     (format nil "~A=~A"
				     (string-downcase (car arg-pair))
				     (cadr arg-pair)))
			 params-alist))))
    content))


(defun http-request (url) ;; method parameters)
  "Perform HTTP request."
  (when *debug*
    (format t "~&CloudStack Request : ~A ~%" url)) ;; parameters))
  (multiple-value-bind (body status-code headers uri stream must-close)
      (drakma:http-request url)
    (declare (ignore headers uri stream must-close))
    (when *debug*
      (format t "~&Cloudstack Response ~A~&~A." status-code body))
    (if (and status-code (< status-code 400))
	(json:decode-json-from-string body)
	(error 'cloudstack-request-error :code status-code :message body))))

(defgeneric sign-request (cloudstack-client name &optional parameters)
  (:documentation "Sign request url using api-key and secret-key of the
 CLOUDSTACK-CLIENT."))

(defmethod sign-request ((cloudstack-client cloudstack-client) name &optional parameters)
  (with-slots (apikey secretkey) cloudstack-client
    (let* ((args '())
	   (hmac (ironclad:make-hmac
		  (flexi-streams:string-to-octets secretkey :external-format :utf-8)
		  :sha1)))
      (push (list "apikey" apikey) args)
      (push (list "command" name) args)
      (push (list "response" "json") args)
      (when parameters
	(loop for param in parameters
	   do (push param args)))
      (let ((request (generate-params-uri (sort args #'string-lessp :key #'car))))
	(ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array
				    (string-downcase request)))
	(let ((signature (base64:usb8-array-to-base64-string
			  (ironclad:hmac-digest hmac))))
	  (concatenate 'string
		       request
		       "&signature="
		       (drakma:url-encode signature :utf-8)))))))

(defgeneric cloudstack-call (cloudstack-client name &key parameters) ;; method content)
  (:documentation "Make a query to the Cloudstack API using URL."))

(defmethod cloudstack-call ((cloudstack-client cloudstack-client) name
			    &key parameters) ;; (method :get) content)
    (let ((url (sign-request cloudstack-client name parameters)))
      (http-request (format nil "~A?~A" (cloudstack-client-uri cloudstack-client) url))))



;; (defmacro define-cloudstack-command (name description command cloudstack-client
;; 				     parameters &body body)
;;   `(progn
;;      (defgeneric ,name (cloudstack-client &optional parameters)
;;        (:documentation ,description))
;;      (defmethod ,name ((cloudstack-client cloudstack-client) &optional parameters)
;;        (cloudstack-call cloudstack-client ,command :parameters ,parameters))))


;;
;; Virtual machines
;;

;; (define-cloudstack-command deploy-virtual-machine
;;     "Creates and automatically starts a virtual machine based on a service offering,
;; disk offering, and template."
;;     "deployVirtualMachine" cloudstack-client parameters)

;; (define-cloudstack-command list-virtual-machines
;;     "List virtual machines"
;;     "listVirtualMachines" cloudstack-client parameters)

;; (define-cloudstack-command start-virtual-machine
;;     "Starts a virtual machine."
;;     "startVirtualMachine" cloudstack-client parameters)

;; (define-cloudstack-command stop-virtual-machine
;;     "Stops a virtual machine."
;;     "stopVirtualMachine" cloudstack-client parameters)

;; (define-cloudstack-command reboot-virtual-machine
;;     "Reboots a virtual machine."
;;     "rebootVirtualMachine" cloudstack-client parameters)


;;
;; Domains
;;

;; (define-cloudstack-command list-domains
;;     cloudstack-client "listDomains" "List domains" parameters)


;;
;; Service Offering
;;


;; (define-cloudstack-command list-service-offerings
;;     cloudstack-client "listServiceOfferings" "Lists all available service offerings." parameters)

;;
;; Disk Offering
;;


;; (define-cloudstack-command list-disk-offerings
;;     cloudstack-client "listDiskOfferings" "Lists all available disk offerings." parameters)

;;
;; Host
;;

;; (define-cloudstack-command list-hosts
;;     cloudstack-client "listHosts" "Lists hosts." parameters)


;;
;; Hypervisor
;;

;; (define-cloudstack-command list-hypervisors
;;     cloudstack-client "listHypervisors" "Lists hypervisors." parameters)

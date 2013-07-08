;;;; cl-cloudstack-client.lisp

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
;; API
;; 

(defgeneric list-service-offerings (cloudstack-client)
  (:documentation "Lists all available service offerings."))

(defgeneric list-disk-offerings (cloudstack-client)
  (:documentation "Lists all available disk offerings."))

;;
;; Tools
;;

(defun http-request (url method parameters)
  "Perform HTTP request."
  (progn
    (when *debug*
      (format t "~&CloudStack Call : ~A ~%Params: ~A~%" url parameters))
    (multiple-value-bind (body status-code headers uri stream must-close)
	(if (equal :post method)
	    (drakma:http-request url
				 :method method
				 :content-length t
				 :parameters parameters)
	    (drakma:http-request url
				 :method method
				 :parameters parameters))
      (when *debug*
	(format t "~&Cloudstack Call HTTP ~A~&Response: ~A." status-code body))
      ;;(declare (ignore headers uri stream must-close))
      (if (and status-code (< status-code 400))
	  (json:decode-json-from-string body)
	  (error 'cloudstack-request-error :code status-code :message body)))))

(defgeneric sign-request (cloudstack-client name)
  (:documentation "Sign request url using api-key and secret-key of the CLOUDSTACK-CLIENT."))

(defmethod sign-request ((cloudstack-client cloudstack-client) name)
  (with-slots (apikey secretkey) cloudstack-client
    (let* ((request (format nil "apikey=~A&command=~A&response=json" apikey name))
	   (hmac (ironclad:make-hmac (flexi-streams:string-to-octets secretkey :external-format :utf-8)
				     :sha1)))
      (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array (string-downcase request)))
      (let ((signature (base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))
	(concatenate 'string
		     request
		     "&signature="
		     (drakma:url-encode signature :utf-8))))))
      

(defgeneric api-perform (cloudstack-client url &key parameters method content)
  (:documentation "Make a query to the Cloudstack API using URL."))

(defmethod api-perform ((cloudstack-client cloudstack-client) name
			&key parameters (method :get) content)
    (let ((url (sign-request cloudstack-client name)))
      (http-request url :get parameters)))


;;
;; Implementation
;; 


(defmethod list-service-offerings ((cloudstack-client cloudstack-client))
  (api-perform cloudstack-client "listServiceOfferings"))

;;; "cl-cloudstack-client" goes here. Hacks and glory await!


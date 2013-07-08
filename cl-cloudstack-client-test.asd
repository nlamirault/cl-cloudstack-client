;;;; cl-cloudstack-client.asd

(asdf:defsystem #:cl-cloudstack-client-test
  :serial t
  :description "Unit tests for the Common Lisp client for Cloudstack API."
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :depends-on (#:cl-cloudstack-client #:lisp-unit)
  :components
  ((:module :test
	    :components ((:file "package")
			 (:file "cloudstack" :depends-on ("package"))))))



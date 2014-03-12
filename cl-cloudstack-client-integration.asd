;;;; cl-cloudstack-client.asd

(asdf:defsystem #:cl-cloudstack-client-integration
  :serial t
  :description "Integration tests for the Common Lisp client for Cloudstack API."
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :depends-on (#:cl-cloudstack-client #:lisp-unit)
  :components
  ((:module :integration
	    :components ((:file "package")
			 (:file "cloudstack" :depends-on ("package"))))))

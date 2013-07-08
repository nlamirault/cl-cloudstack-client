;;;; cl-cloudstack-client.asd

(asdf:defsystem #:cl-cloudstack-client
  :serial t
  :description "A Common Lisp client for Cloudstack API."
  :author "Nicolas Lamirault <nicolas.lamirault@gmail.com>"
  :depends-on (#:drakma
               #:cl-json
	       #:ironclad)
  :components ((:module :src
			:components ((:file "package")
				     (:file "conditions" :depends-on ("package"))
				     (:file "cl-cloudstack-client" :depends-on ("conditions"))))))


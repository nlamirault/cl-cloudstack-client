cl-cloudstack-client
====================

A client for Cloudstack API writtent in Common Lisp.

Installation
------------

Add the projet :

    (push #p"/projects/cl-cloudstack-client/" asdf:*central-registry*)
	(ql:quickload "cl-cloudstack-client")

Run unit tests:

	(ql:quickload "cl-cloudstack-client-test")
	(setq lisp-unit:*print-failures* t)
	(setq cl-cloudstack-client-test:*cloudstack-test-uri* "http://.....")
	(lisp-unit:run-tests :all :cl-cloudstack-client-test)

Usage
-----



Contact
-------

Nicolas Lamirault <nicolas.lamirault@gmail.com>



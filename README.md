# cl-cloudstack-client

[![Build Status](https://drone.io/github.com/nlamirault/cl-cloudstack-client/status.png)](https://drone.io/github.com/nlamirault/cl-cloudstack-client/latest)

A client for Cloudstack API writtent in Common Lisp.


## Installation

* Add the projet and load it using [Quicklisp](http://www.quicklisp.org):

        CL-USER> (push #p"/projects/cl-cloudstack-client/" asdf:*central-registry*)
		CL-USER> (ql:quickload "cl-cloudstack-client")

* Run unit tests:

		CL-USER> (ql:quickload "cl-cloudstack-client-test")
		CL-USER> (setq lisp-unit:*print-failures* t)
		CL-USER> (setq cl-cloudstack-client-test:*cloudstack-test-uri* "http://.....")
		CL-USER> (lisp-unit:run-tests :all :cl-cloudstack-client-test)

* Run the integration tests (depends on a running Cloudstack managment server):

        CL-USER> (ql:quickload "cl-cloudstack-client-integration")
		CL-USER> (setq lisp-unit:*print-failures* t)
		CL-USER> (setq cl-cloudstack-client-test:*cloudstack-uri* "http://.....")
		CL-USER> (lisp-unit:run-tests :all :cl-cloudstack-client-integration)


## Usage

Refers to the [Cloudstack API](https://cloudstack.apache.org/docs/api/apidocs-4.1/TOC_Root_Admin.html) and performs calls :

	CL-USER> (setq *cloudstack*
                 (cl-cloudstack-client::make-cloudstack-client
                      "http://localhost:8080/client/api"
                      "MY_API_KEY"
                      "MY_SECRET_KEY"))
	CL-USER> (cl-cloudstack-client:cloudstack-call
                                   *cloudstack*
                                   "listServiceOfferings"
                                   :parameters '(("name" "small")))
	((:LISTSERVICEOFFERINGSRESPONSE (:COUNT . 1)
      (:SERVICEOFFERING
        ((:ID . "10") (:NAME . "small ") (:DISPLAYTEXT . "small ") (:CPUNUMBER . 1)
         (:CPUSPEED . 1000) (:MEMORY . 1024) (:CREATED . "2013-02-15T11:50:10+0100")
         (:STORAGETYPE . "shared") (:OFFERHA) (:LIMITCPUUSE) (:ISSYSTEM)
         (:DEFAULTUSE)))))


## Changelog

A changelog is available [here](ChangeLog.md).


## Copyright and license

Code and documentation (c) Nicolas Lamirault. Code released under [the MIT license](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>

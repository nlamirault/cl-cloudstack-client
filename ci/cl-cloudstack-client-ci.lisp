;;
;; Continuous integration: launch unit tests
;;

(in-package :cl-user)

(load ".clenv/.quicklisp/setup.lisp")

(ql:quickload "cl-cloudstack-client")
(ql:quickload "cl-cloudstack-client-test")

(setq cl-cloudstack-client-test:*cloudstack-test-uri*
      "http://localhost:8096")
(lisp-unit:run-tests :all :cl-cloudstack-client-test)

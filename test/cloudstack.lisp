

(in-package :cl-cloudstack-client-test)

(defparameter *ordered-url*
  (concatenate 'string 
	       "apikey=sHfqA41hDYeHH0oNpHAEA5tZzA-rT5houFJUfuXCa_jZMoUroJKSOxwBG5qivXCk0Zm9CjQ63-xKGzuqiHYQ_w"
	       "&command=listDomains"
	       "&response=json"
	       "&signature=celKqR3O6kN6viQeLHiizyCv97A%3D"))

(defparameter *user-api-key*
  "sHfqA41hDYeHH0oNpHAEA5tZzA-rT5houFJUfuXCa_jZMoUroJKSOxwBG5qivXCk0Zm9CjQ63-xKGzuqiHYQ_w")

(defparameter *user-secret-key*
  "jBtFUGUIKWRUzowcSg4VJ4VKNGatnaaH4R6fLV0T6DinPJssJJmLr2UWOQRgyCxZIsWmHYw362T64poyF3wMBA")

(define-test can-sign-request
  (let ((cloudstack 
	 (make-cloudstack-client "command=listDomains" *user-api-key* *user-secret-key*)))
    (assert-equal *ordered-url* (sign-request cloudstack "listDomains"))))
		


;; (define-test sign-request
;;   (assert-equal 
;; tring url = ICloudStackService.API_LIST_DOMAINS
;;   65                 + ICloudStackService.API_JSON_RESPONSE; //"command=listDomains&response=json";
;;   66         String signature = new SignatureSigner().signRequest(url,
;;   67                 CloudStackTestConfiguration.USER_API_KEY,
;;   68                 CloudStackTestConfiguration.USER_SECRET_KEY);
;;   69         
;;   70         String orderedUrl = "apikey=sHfqA41hDYeHH0oNpHAEA5tZzA-rT5houFJUfuXCa_jZMoUroJKSOxwBG5qivXCk0Zm9CjQ63-xKGzuqiHYQ_w"
;;   71                  + "&command=listDomains"
;;   72                  + "&response=json"
;;   73                  + "&signature=celKqR3O6kN6viQeLHiizyCv97A%3D";
;;   74         assertEquals(orderedUrl, signature);

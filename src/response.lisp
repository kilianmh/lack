(defpackage lack/response
  (:nicknames #:lack.response)
  (:use #:cl)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:local-time
                #:format-timestring
                #:universal-to-timestamp
                #:+gmt-zone+)
  (:import-from #:cl-cookie
		#:make-cookie
		#:cookie
		#:cookie-jar
		#:cookie-jar-cookies
		#:write-set-cookie-header)
  (:import-from #:alexandria
		#:when-let)
  (:export #:response
           #:make-response
           #:finalize-response
           #:response-status
           #:response-headers
           #:response-body
           #:response-set-cookies))
(in-package #:lack/response)

(defstruct (response
            (:constructor make-response (&optional status headers (body nil has-body)
                                         &aux (no-body (not has-body)))))
  (status nil :type (integer 100 599))
  (headers nil :type list)
  (body nil :type (or vector pathname list))
  (no-body nil :type boolean)
  (set-cookies nil :type (or list cookie cookie-jar)))

(defun finalize-response (res)
  (finalize-cookies res)
  (with-slots (status headers body no-body) res
    (list* status headers
           (cond
             ((and no-body (not body)) nil)
             ((or (consp body) (pathnamep body) (and (not (stringp body)) (vectorp body)))
              (list body))
             (t (list (list body)))))))

(defun finalize-cookies (res)
  (when-let (set-cookies (response-set-cookies res))
    (setf (response-headers res)
        (append (response-headers res)
		(typecase set-cookies
		  (cons
		   (loop for (k v) on set-cookies by #'cddr
			 append (list :set-cookie (bake-cookie k v))))
		  (cookie
		   (list :set-cookie (write-set-cookie-header set-cookies)))
		  (cookie-jar
		   (mapcan (lambda (set-cookie)
			     (list :set-sookie (write-set-cookie-header set-cookie)))
			   (cookie-jar-cookies set-cookies))))))))

(defun bake-cookie (key value)
  "Convert the value plist with cl-cookie to set-cookie header"
  (destructuring-bind (&key value domain path expires secure httponly samesite
			 partitioned origin-hist max-age &allow-other-keys)
      value
    (write-set-cookie-header
     (make-cookie :name key :value value :path path :domain domain
		  :origin-host origin-host :expires expires :max-age max-age
		  :same-site samesite :partitioned partitioned
		  :secure-p secure :httponly-p httponly))))

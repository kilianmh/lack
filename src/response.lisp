(defpackage lack/response
  (:nicknames :lack.response)
  (:use :cl)
  (:import-from :quri
                :url-encode)
  (:import-from :local-time
                :format-timestring
                :universal-to-timestamp
                :+gmt-zone+)
  (:export :response
           :make-response
           :finalize-response
           :response-status
           :response-headers
           :response-body
           :response-set-cookies))
(in-package :lack/response)

(defstruct (response
            (:constructor make-response (&optional status headers (body nil has-body)
                                         &aux (no-body (not has-body)))))
  (status nil :type (integer 100 599))
  (headers nil :type list)
  (body nil :type (or vector pathname list))
  (no-body nil :type boolean)
  (set-cookies nil :type list))

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
  (setf (response-headers res)
        (append (response-headers res)
                (loop for (k v) on (response-set-cookies res) by #'cddr
                      append (list :set-cookie (bake-cookie k v))))))

(defun bake-cookie (key value)
  (unless value
    (return-from bake-cookie ""))

  (destructuring-bind (&key domain path expires secure httponly samesite &allow-other-keys)
      value
    (unless (or (member samesite (list :lax :strict))
                secure)
      (warn "Samesite=None cookies require Secure"))
    (with-output-to-string (s)
      (format s "~A=~A"
              (quri:url-encode (string key))
              (quri:url-encode (getf value :value)))
      (when domain
        (format s "; domain=~A" domain))
      (when path
        (format s "; path=~A" path))
      (when expires
        (format s "; expires=")
        (format-timestring
         s (universal-to-timestamp expires)
         :format '(:short-weekday ", " (:day 2) #\Space :short-month #\Space (:year 4) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2) " GMT")
         :timezone +gmt-zone+))
      (when secure
        (write-string "; secure" s))
      (when httponly
        (write-string "; HttpOnly" s))
      (format s "; SameSite=~A" (cond ((eq samesite :lax)
                                       "Lax")
                                      ((eq samesite :strict)
                                       "Strict")
                                      (t "None"))))))

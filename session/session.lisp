;;;; session.lisp
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(in-package #:session)

(defvar *app* (make-instance 'ningle:app))
(defvar *namespace* "session_test")

(defmacro from-session (key)
  `(gethash ,key
	    (getf (lack.request:request-env ningle:*request*)
		  :lack.session)))

(setf (ningle:route *app* "/")
      #'(lambda (params)
	  (declare (ignore params))
	  
	  (unless (from-session :counter)
	      (setf (from-session :counter) 0))
	  (format nil "Hello, you've been here for ~Ath times!"
		  (incf (from-session :counter)))))

(clack:clackup
 (lack.builder:builder
  (:session
   :store (lack.middleware.session.store.redis:make-redis-store
	   :namespace *namespace*))
  *app*))




(ql:quickload '(babel usocket))

(declaim (optimize (debug 3) (speed 0)))

(defparameter *qclasses*
  '((:in . 1)))

(defparameter *qtypes*
  '((:a . 1)
    (:ns . 2)))

(defclass dns-packet ()
  ((id :initarg :id :accessor id)
   (qr :initarg :qr :accessor qr)
   (opcode :initarg :opcode :accessor opcode :initform 0)
   (aa :initarg :aa :accessor aa :initform nil)
   (tc :initarg :tc :accessor tc :initform nil)
   (rd :initarg :rd :accessor rd :initform nil)
   (ra :initarg :ra :accessor ra :initform nil)
   (rcode :initarg :rcode :accessor rcode :initform 0)
   (questions :initarg :questions :accessor questions :initform '())
   (answers :initarg :answers :accessor answers :initform '())
   (nss :initarg :nss :accessor nss :initform '())
   (ars :initarg :ars :accessor ars :initform '())))

(defclass dns-question ()
  ((qname :initarg :qname :accessor qname)
   (qtype :initarg :qtype :accessor qtype)
   (qclass :initarg :qclass :accessor qclass :initform :in)))

(defgeneric extend-vector (pkt buffer))

(defun encode (pkt)
  (let ((buffer
	  (make-array 4096 :fill-pointer 0
			   :adjustable t
			   :element-type '(unsigned-byte 8))))
    (extend-vector pkt buffer)
    buffer))

(defun push-short (val buffer)
  (vector-push-extend (ash val -8) buffer)
  (vector-push-extend (logand val #xff) buffer))

(defmethod extend-vector ((pkt dns-packet) buffer)
  (push-short (id pkt) buffer)
  (vector-push-extend (+ (if (equal (qr pkt) :response) 128 0)
			 (ash (opcode pkt) 3)
			 (if (aa pkt) 4 0)
			 (if (tc pkt) 2 0)
			 (if (rd pkt) 1 0))
		      buffer)
  (vector-push-extend (+ (if (ra pkt) 128 0)
			 (rcode pkt))
		      buffer)
  (push-short (length (questions pkt)) buffer)
  (push-short (length (answers pkt)) buffer)
  (push-short (length (nss pkt)) buffer)
  (push-short (length (ars pkt)) buffer)
  (loop for q in (questions pkt) do (extend-vector q buffer))
  (loop for a in (answers pkt) do (extend-vector a buffer))
  (loop for ns in (nss pkt) do (extend-vector ns buffer))
  (loop for ar in (ars pkt) do (extend-vector ar buffer))
  buffer)

(defmethod extend-vector ((pkt dns-question) buffer)
  (loop for label in (qname pkt) do
	(let ((msg (babel:string-to-octets label)))
	  (vector-push-extend (length msg) buffer)
	  (dotimes (i (length msg))
	    (vector-push-extend (aref msg i) buffer))))
  (vector-push-extend 0 buffer)
  (push-short (cdr (assoc (qtype pkt) *qtypes*)) buffer)
  (push-short (cdr (assoc (qclass pkt) *qclasses*)) buffer)
  buffer)

(let ((buf (encode
	    (make-instance 'dns-packet
			   :id 257
			   :qr :query
			   :questions
			   (list (make-instance 'dns-question
						:qname '("google" "com")
						:qtype :a))))))
  (usocket:socket-send (usocket:socket-connect
			"8.8.8.8" 53
			:protocol :datagram :element-type '(unsigned-byte 8))
		       buf (length buf)))

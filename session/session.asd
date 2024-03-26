;;;; session.asd

(asdf:defsystem #:session
  :description "Describe session here"
  :author "Alberto Lerda"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack #:ningle #:lack-session-store-redis)
  :components ((:file "package")
               (:file "session")))

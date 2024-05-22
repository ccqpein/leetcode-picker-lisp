;;;; -*- Mode: Lisp -*-

(defpackage :leetcode-picker-sys
  (:use :CL :asdf))

(in-package :leetcode-picker-sys)

(defsystem leetcode-picker
  :defsystem-depends-on ("yason" "cl-ppcre" "arrow-macros" "clingon")
  :components ((:file main))
  :build-operation "program-op"
  :build-pathname "leetcode-picker"
  :entry-point "leetcode-picker:main"
  )

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

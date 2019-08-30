(in-package #:common-lisp-user)


(asdf:defsystem htm-cl
  :author "Hai NGUYEN"
  :maintainer "Hai NGUYEN"
  :licence "Please see the attached LICENCE file."
  :homepage "https://gitlab.com/haicnguyen/htm-cl"
  :version "0.0.1"
  :depends-on (#:ieee-floats #:cl-mop #:cl-portaudio)
  :components ((:module "src/"
                :serial t
                :components ((:file "package")
                             (:file "helper")
                             (:file "scalar-encoder"))))
  :description "This package implements Numenta's NUPIC in Common Lisp.")


(asdf:defsystem htm-cl/test
  :author "Hai NGUYEN"
  :maintainer "Hai NGUYEN"
  :licence "Please see the attached LICENSE file."
  :depends-on (#:rove)
  :components ((:module "t/"
                :serial t
                :components ((:file "package")
                             (:file "scalar-encoder")))))

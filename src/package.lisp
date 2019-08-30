(in-package #:common-lisp-user)


(defpackage htm-helper
  (:use #:common-lisp)
  (:export #:next-after-float32))


(defpackage htm-scalar-encoder
  (:use #:common-lisp)
  (:export #:category-input-p
           #:clip-input-p
           #:encode
           #:minimum-input
           #:maximum-input
           #:num-consecutive-1-bits
           #:periodic-input-p
           #:radius
           #:resolution
           #:scalar-encoder
           #:size
           #:sparsity))


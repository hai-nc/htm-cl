;;;; Unit tests on scalar encoder

;;; Based on the community's htm.core/src/htm/tests/unit/encoders/ScalarEncoderTest.cpp viewed 2019-08-28.

(in-package #:htm-scalar-encoder-test)


;; (defstruct test-data
;;   ;; Test data
;;   (input 0.0 :type 'single-float)
;;   (expected-output nil) ; *sparse* indices of active bits.
;;   )


(rove:deftest test-clipping-inputs
  (let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :size       10
                                  :num-consecutive-1-bits 2                    
                                  :minimum-input    10                   
                                  :maximum-input    20)))
    (rove:testing "With no input clipping:"
      (setf (htm-scalar-encoder:clip-input-p encoder) nil)
      (rove:ok (signals (htm-scalar-encoder:encode encoder 9.9) 'simple-error)
               (format nil "Input below minimum-input of ~a" (htm-scalar-encoder:minimum-input encoder)))
      (rove:ok (encode encoder 10.0))
      (rove:ok  (encode encoder 19.9))
      (rove:ok (encode encoder 20.0))
      (rove:ok (signals (encode encoder 20.1) 'simple-error)
               (format nil "Input above maximum-input of ~a" (htm-scalar-encoder:maximum-input encoder))))
    ;; test with input-clipping allowed:
    (rove:testing "With input clipping:"
      (setf (htm-scalar-encoder:clip-input-p encoder) t)
      (rove:ok (encode encoder  9.9))
      (rove:ok (encode encoder 20.1)))))


;;; Helper functions for subsequent test below 
(defun encode-cases (encoder data-array)
  "For each test-data in the DATA-ARRAY, encode its input and compare with its expected-output.
DATA-ARRAY: a simple vector of struct test-data."
  (loop with each-input = nil
        with each-expected-output = nil
        for each-case in data-array do
          (progn
            (setf each-input (car each-case)
                  each-expected-output (cdr each-case))
            (rove:ok (equalp (htm-scalar-encoder:encode encoder each-input)
                             each-expected-output)))))
    
;;; now back to the tests
(rove:deftest non-integer-bucket-width
  (rove:testing "Non-integer-bucket-width:"
    (let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :size       7
                                  :num-consecutive-1-bits 3
                                  :minimum-input    10.0
                                  :maximum-input    20.0
                                  ))
          (data-array '((10.0 . ((0 . 2)))
                        (20.0 . ((4 . 6))))))
    (encode-cases encoder data-array))))


;;; another test
(rove:deftest round-to-nearest-multiple-of-resolution
    (let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :num-consecutive-1-bits 3
                                  :minimum-input     10.0
                                  :maximum-input     20.0
                                  :resolution  1))
          (cases  '((10.00 . ((0 . 2)))
                    (10.49 . ((0 . 2)))
                    (10.50 . ((0 . 2))) ; (((1 . 3))) in Scalar-Encoder.cpp
                    (11.49 . ((1 . 3)))
                    (11.50 . ((2 . 4)))
                    (14.49 . ((4 . 6)))
                    (14.50 . ((4 . 6))) ; (((5 . 7))) in Scalar-Encoder.cpp
                    (15.49 . ((5 . 7)))
                    (15.50 . ((6 . 8)))
                    (19.00 . ((9 . 11)))
                    (19.49 . ((9 . 11)))
                    (19.50 . ((10 . 12)))
                    (20.00 . ((10 . 12))))))
      (rove:testing "Round-to-nearest-multiple-of-resolution:" 
        (rove:ok (equalp (htm-scalar-encoder:size encoder) 13))
        (encode-cases encoder cases))))


(rove:deftest periodic-round-nearest-multiple-of-resolution
  (rove:testing "Periodic-round-nearest-multiple-of-resolution:"
    (let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :num-consecutive-1-bits 3
                                  :minimum-input    10.0
                                  :maximum-input    20.0
                                  :resolution   1
                                  :periodic-input-p t))
          (cases  '((10.00 . ((0 . 2)))
                    (10.49 . ((0 . 2)))
                    (10.50 . ((0 . 2))) ; (((1 . 3)))
                    (11.49 . ((1 . 3)))
                    (11.50 . ((2 . 4)))
                    (14.49 . ((4 . 6)))
                    (14.50 . ((4 . 6))) ; (((5 . 7)))
                    (15.49 . ((5 . 7)))
                    (15.50 . ((6 . 8)))
                    (19.49 . ((9 . 9) (0 . 1)))
                    (19.50 . ((0 . 2)))
                    (20.00 . ((0 . 2))))))
      (rove:ok (equalp (htm-scalar-encoder:size encoder) 10))
      (encode-cases encoder cases))))


(rove:deftest periodic-day-of-week
  (rove:testing "Periodic-day-of-week:"
    (let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :num-consecutive-1-bits 3
                                  :minimum-input    1.0 ; Monday
                                  :maximum-input    8.0 ; Monday
                                  :size   14
                                  :periodic-input-p t)
                   ;; equivalently: radius = 1.5 or resolution = 0.5 (12 hours)
                   )
          (cases  '((1.0 . ((0 . 2)))
                    (2.0 . ((2 . 4)))
                    (3.0 . ((4 . 6)))
                    (4.0 . ((6 . 8)))
                    (5.0 . ((8 . 10)))
                    (6.0 . ((10 . 12)))
                    (7.0 . ((12 . 13) (0 . 0)))
                    (8.0 . ((0 . 2))))))
      (rove:ok (equalp (htm-scalar-encoder:resolution encoder) 0.5))
      (format t "radius: ~a" (htm-scalar-encoder:radius encoder))
      ;; (rove:ok (equalp (htm-scalar-encoder:radius encoder) 1.5))
      (encode-cases encoder cases))))

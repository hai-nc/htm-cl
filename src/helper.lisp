;;;; Utility functions for testing.


(in-package #:htm-helper)


(defun next-after-float32 (a-float)
  "Return the smallest increment of a float (32-bit) number.
This emulates C++ std::nextafter() function.
This requires the Common Lisp 'ieee-floats' library."
  ;; references: moonshadow 2008 https://stackoverflow.com/questions/155378/how-to-alter-a-float-by-its-smallest-increment-or-close-to-it#155397
  (ieee-floats:decode-float32
   (1+ (ieee-floats:encode-float32 a-float)))
  ;; ;; optionally: check that we haven't flipped the sign or generated
  ;; ;; an overflow/underflow result by examining the sign and exponent
  ;; ;; bits!?
  )

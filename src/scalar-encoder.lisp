;;; This is based on the HTM Community Edition of NuPIC <https://github.com/htm-community/htm.core/encoders/ScalarEncoder.cpp> commit d9ee57970f2a9dd96d9149fd74308261a216e525 date 17 Jun 2019 at 20:37:52.
;;;
;;; Compared to Scalar-Encoder.cpp, this library outputs have some differences:
;;; - output format: this library outputs a list of at most 2 dotted pairs of the form (start index . end index), each pair represents the start and end indices of a continuous sequence of the output array where all the bit values each has a value of 1. The remaining array elements not in any of the returned (start index . end-index) range have bit value 0. If the output is periodic and the slice \"wrap around\" to the beginning of the output array, then at most 2 pairs of (start index . end index) is provided (please refer to the `encode' method below).
;;; - due to rounding error, some outputs from this library are off-1-index compared to those expected from Scalar-Encoder.cpp, for instance this library encodes 10.5 into the 1-bit slice of [0,2] instead of [1,3] as in Scalar-Encoder.cpp, or 14.5 into [4,6] instead of [5,7].


(in-package #:htm-scalar-encoder)


(defclass scalar-encoder ()
  (;; slots:
   
   ;; The number of consecutive bits of value 1's in the encoded output SDR.
   (num-consecutive-1-bits
    :initarg :num-consecutive-1-bits
    :initform 0
    :accessor num-consecutive-1-bits
    :type 'integer)

   ;; If true then this encoder will only encode unsigned integers, and all inputs shall have a unique/non-overlapping representations.
   (category-input-p
    :initarg :category-input-p
    :initform nil
    :accessor category-input-p)
  
  ;; Members 'minimum-input' and 'maximum-input' defines the range of the inputs. These endpoints are inclusive.
   (minimum-input
    :initarg :minimum-input
    :initform 0.0
    :accessor minimum-input
    :type 'single-float)
   
   (maximum-input
    :initarg :maximum-input
    :initform  0.0
    :accessor maximum-input
    :type 'single-float)

  ;; If true, inputs outside the range [minimum-input, maximum-input] will be clipped into the range [minimum-input, maximum-input] (which includes minimum-input, maximum-input endpoints).  If false, inputs outside the range will raise an error.
   (clip-input-p
    :initarg :clip-input-p
    :initform nil
    :accessor clip-input-p)

  ;; If true, then the minimum-input & maximum-input inputs are the same and the first and last bits of the output SDR are adjacent.  The contiguous block of 1's wraps around the end back to the beginning. (eg. hours of day - last hour of one day will proceed to beginning hour of day of the next day).
  ;;
  ;; If false, then minimum-input & maximum-input inputs are the endpoints of the input range, are not adjacent, and activity does not wrap around.
   (periodic-input-p
    :initarg :periodic-input-p
    :initform nil
    :accessor periodic-input-p)

  ;; Minimum-Input difference (inclusive) between two inputs to have different representations for them, including when the difference equals this amount.
   (resolution
    :initarg :resolution
    :initform 0.0
    :accessor resolution
    :type 'single-float)
  
  ;; The total number of bits in the encoded output SDR.
   (size
    :initarg :size
    :initform 0
    :accessor size
    :type 'integer)

  ;; An alternative way to specify the member 'num-consecutive-1-bits'.  Sparsity requires that the size also needs specifying.  Specify only one of num-consecutive-1-bits or sparsity.
   (sparsity
    :initarg :sparsity
    :initform 0.0
    :accessor sparsity
    :type 'single-float)

  ;; Two inputs separated by more than the radius have non-overlapping representations. Two inputs separated by less than the radius will in general overlap in at least some of their bits. You can think of this as the radius of the input.
   (radius
    :initarg :radius
    :initform 0.0
    :accessor radius
    :type 'single-float))
  
  (:documentation "Encodes a floating point number into an array of bits. The output is 0's except for a contiguous block of 1's. The location of this contiguous block varies continuously with the input value."))

(defmethod assert-exactly-one-non-zero-slot ((encoder scalar-encoder) slot-symbol-list)
  (loop with attributes = slot-symbol-list
          with non-zero-attributes = nil
          with attribute-value = nil
          for attribute in attributes do
            (progn
              (setf attribute-value (slot-value encoder attribute))
              (when (and attribute-value
                         (numberp attribute-value)
                         (null (zerop attribute-value)))
                (push attribute non-zero-attributes)))
          finally
             (progn
               (unless (equalp (length non-zero-attributes) 1)
                 (error "Expecting *exactly one* of these slot to be non-zero: ~a~%, got ~a such slots: ~a" attributes (length non-zero-attributes) non-zero-attributes))
               non-zero-attributes)))


(defmethod initialize-instance :after ((encoder scalar-encoder) &key)

  ;; These four (4) mutually-exclusive members define the total
  ;; number of bits in the output, and only one of them should be
  ;; non-zero when constructing the encoder:
  (assert-exactly-one-non-zero-slot encoder '(size radius category-input-p resolution))

  (assert-exactly-one-non-zero-slot encoder '(num-consecutive-1-bits sparsity))

  (with-slots (resolution
               minimum-input
               maximum-input
               size
               radius
               sparsity
               category-input-p
               clip-input-p
               periodic-input-p
               num-consecutive-1-bits)
      encoder
    ;; validate parameters
    (assert (< minimum-input maximum-input))

    (when category-input-p
      (when clip-input-p
        (error "Incompatible arguments: category-input-p & clip-input-p!"))
      (when periodic-input-p
        (error "Incompatible arguments: category-input-p & periodic-input-p!"))
      (unless (>= minimum-input 0.0)
        (error "Minimum-Input input value of category-input-p encoder must be non-negative!"))
      (unless (> maximum-input 0.0)
        (warn "Maximum-Input input value of category-input-p encoder must be a positive!")))

    (when category-input-p (setf radius 1.0))

    (when (> sparsity 0.0)
      (assert (<= 0.0 sparsity 1.0))
      (unless (> size 0)
        (error "Argument 'size' (value: ~a) should be > 0!" size))
      (round (* size sparsity)))
          
    ;; Determine resolution & size:
    (let ((extent-width (- (if periodic-input-p
                               maximum-input
                               ;; else increase the max by the smallest possible amount:
                               (htm-helper:next-after-float32 (coerce maximum-input 'single-float)))
                           minimum-input))
          needed-bands)
      
      (if (> size 0)
          ;; Distribute the active bits along the domain [minimum-input, maximum-input], including the endpoints. The resolution is the width of each band between the points.
          (setf resolution (/ extent-width
                              (if periodic-input-p
                                  size
                                  (1- (- size (1- num-consecutive-1-bits)) ; number of buckets
                                   ))))
          (progn
            (when (> radius 0.0)
              (setf resolution (/ radius num-consecutive-1-bits)))
            (setf needed-bands (ceiling (/ extent-width resolution))
                  size (if periodic-input-p
                           needed-bands
                           (+ needed-bands (1- num-consecutive-1-bits)))))))

    ;; Determine radius. Always calculate this even if it was given,
    ;; to correct for rounding error:
    (setf radius (* num-consecutive-1-bits resolution))

    ;; Determine sparsity. Always calculate this even if it was
    ;; given, to correct for rounding error.
    (setf sparsity (/ num-consecutive-1-bits size))

    (assert (> size 0))
    (assert (> num-consecutive-1-bits 0))
    (assert (< num-consecutive-1-bits size))

    ;; (call-next-method :dimensions size)
    ))
 

(defmethod encode ((encoder scalar-encoder) input)
  "Returns a list of at most 2 dotted pairs of the form (start index . end index), each pair represents the start and end indices of a continuous sequence of the output array where all the bit values each has a value of 1. The remaining array elements not in any of the returned (start index . end-index) range have bit value 0. If the output is periodic and the slice \"wrap around\" to the beginning of the output array, then at most 2 pairs of (start index . end index) is provided.
INPUT: a floating-point number."
  (with-slots (size
               clip-input-p
               periodic-input-p
               category-input-p
               minimum-input
               maximum-input
               resolution
               num-consecutive-1-bits)
      encoder
    (assert (and input (numberp input)))
    
    (unless (<= minimum-input input maximum-input)
      (if clip-input-p
          (if periodic-input-p
              (setf input (+ minimum-input (mod input (- maximum-input minimum-input))))
            (setf input (max input minimum-input)
                  input (min input maximum-input)))
          (error "Input (~a) is out of bound [~a, ~a]" input minimum-input maximum-input)))

    (when category-input-p (assert (= input (ffloor input))))
    
    (let* ((max-index (1- size))
           ;; `mod' is used below for correction for edge case of rounding at 0.5 will become 1.0, which makes start-index 1 index higher that max-index (eg. case of minimum-input 10.0, maximum-input 20.0, resolution 1, periodic-input-p t, and input is 19.5)
           (start-index (mod (round (/ (- input minimum-input) resolution))
                             size))
           (end-index (+ start-index (1- num-consecutive-1-bits))))
      (assert (<= 0 start-index max-index))
      (if (<= end-index max-index)
          (list (cons start-index end-index))
          (if periodic-input-p
              ;; split the slice into 2: (start-index . max-index) and (0 . length-of-max-index-to-end-index-minus-1):
              (list (cons start-index max-index) (cons 0 (- end-index size)))  
              ;; no wrap-around: shift (start-index . end-index) slice backwards until end-index matches with max-index:
              (list (cons (- size num-consecutive-1-bits) max-index)))))))

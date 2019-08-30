A Common Lisp implementation of A.I. based on Numenta's HTM algorithms.


# Requirements

1. SBCL (Steel-Bank Common Lisp) (tested with SBCL version 1.5.4 on GNU-Linux)
1. quicklisp.
1. connection to the Internet, which might be taken for granted, for quicklisp to auto-download dependencies during execution.


# Usage

These steps below are for the GNU-Linux operating system:


First, download this repository and link/copy it as "~/quicklisp/local-projects/htm-cl"


Then load the system:


```lisp
(ql:quickload "htm-cl")
```

## Scalar encoder

This is a port of htm.core's ScalarEncoder.cpp. This library converts a floating-point number into a list of dotted pairs, each of which of the form (start-index . end-index) representing the start and end indices of the continuous sequence of 1-bits of the output bit-array. 


Other information of the output bit-array (eg. size, resolution, radius, etc.) can be accessed via accessors of the scalar-encoder object.


For example, to encode days of week:


```lisp

(let ((encoder (make-instance 'htm-scalar-encoder:scalar-encoder
                                  :num-consecutive-1-bits 3
                                  :minimum-input    1.0 ; Monday
                                  :maximum-input    8.0 ; Monday
                                  :size   14
                                  :periodic-input-p t)
                   ;; equivalently: radius = 1.5 or resolution = 0.5 (12 hours)
                   ))
      (htm-scalar-encoder:encode encoder 1.0)) ; => ((0 . 2))
```


When the input is periodic and the output "overflow" its maximum index, the consecutive 1-bits of the output can be wrapped around and thus the need for two dotted pairs to represent the wrapped-around sequences of 1-bits. Continuing with the example above:


```lisp
(htm-scalar-encoder:encode encoder 7.0) ; => ((12 . 13) (0 . 0))
```

Since the output bit-array has 14 bits and the sequence of 1-bits is the 12th-13th-14th bits (starting with index zero), the 14th bit is wrapped-around and becoming the 0th bit.


## Testing

To run the tests:


```lisp
(ql:quickload "rove")
(ql:quickload "htm-cl/tests")
(rove:run :htm-cl/tests)
```

# License


Copyright (c) 2019 Hai NGUYEN.


Please also refer the attached LICENSE file.

;;; File with functions to generate portable random numbers.
;;; Taken from random-state [add-reference]

(in-package :vrp)


;;;{{{ utilities
(declaim (inline truncate-bits))
(declaim (ftype (function ((integer 0) (integer 0)) (integer 0))))
(defun truncate-bits (x bits)
  (logand x (1- (ash 1 bits))))

(declaim (inline truncate32))
(declaim (ftype (function (integer) (unsigned-byte 32)) truncate32))
(defun truncate32 (x)
  (logand x #xFFFFFFFF))

(declaim (inline truncate64))
(declaim (ftype (function (integer) (unsigned-byte 64)) truncate64))
(defun truncate64 (x)
  (logand x #xFFFFFFFFFFFFFFFF))

(defun byte-array-to-int (array)
  (loop with int = 0
        for i from 0 below (length array)
        do (setf (ldb (byte 8 (* i 8)) int) (aref array i))
        finally (return int)))

(defun hopefully-sufficiently-random-seed ()
  #+unix
  (let ((seq (make-array 8 :element-type '(unsigned-byte 8))))
    (with-open-file (stream #P"/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence seq stream))
    (byte-array-to-int seq))
  #+(and win32 sb-dynamic-core)
  (byte-array-to-int (sb-win32:crypt-gen-random 8))
  #-(or unix (and win32 sb-dynamic-core))
  (logxor #+sbcl (sb-ext:get-bytes-consed)
          (get-internal-real-time)
          (get-universal-time)))

(defun 32bit-seed-array (size seed)
  (declare (optimize speed))
  (let ((array (make-array size :element-type '(unsigned-byte 32))))
    (setf (aref array 0) (truncate32 seed))
    ;; Using generator from:
    ;; Line 25 of Table 1 in "The Art of Computer Programming Vol. 2" (2nd Ed.), pp 102
    (loop for i from 1 below size
          do (setf (aref array i)
                   (truncate32 (* 69069 (aref array (1- i))))))
    array))

(defun 64bit-seed-array (size seed)
  (declare (optimize speed))
  (let ((array (make-array size :element-type '(unsigned-byte 64))))
    (setf (aref array 0) (truncate64 seed))
    (loop for i from 1 below size
          do (setf (aref array i)
                   (truncate64 (+ (* 6364136223846793005
                                     (logxor (aref array (1- i))
                                             (ash (aref array (1- i)) -62)))
                                  i))))
    array))

(defun barr (bytes &rest contents)
  (make-array (length contents) :element-type `(unsigned-byte ,bytes) :initial-contents contents))

(defmacro incfmod (place mod &optional (delta 1))
  `(setf ,place (mod (+ ,place ,delta) ,mod)))

;;;}}}


;;;;;{{{ random number generation functions

;;;{{{ basic generator class 
(defclass basic-generator ()
  ((seed :initarg :seed :reader seed :writer set-seed)
   (bytes :initform (error "Missing BYTES initform from generator definition.")
          :reader bytes))
  (:default-initargs
   :seed (error "SEED required.")))

(defmethod print-object ((generator basic-generator) stream)
  (print-unreadable-object (generator stream :type T)
    (format stream "~s" (seed generator))))
;;;}}}

(defvar *current-generator* nil
  "The current generator.")

;;;{{{ macro define-generator-generic
(defmacro define-generator-generic (name (generator &rest args) &rest options)
  (let* ((inner-name (symb name "%")))
    `(progn
       (defgeneric ,inner-name (,generator ,@args) ,@options)

       ;; (defmethod ,inner-name (,generator ,@args)
       ;;   (,name (global-generator ,generator)
       ;;          ,@(remove-if (lambda (a) (find a lambda-list-keywords)) args)))
       (defun ,name ,args
         (,inner-name *current-generator* ,@(remove-if (lambda (a) (find a lambda-list-keywords)) args))))))
;;;}}}

;;;{{{ random-byte
(define-generator-generic random-byte (generator))
;;;}}}

;;;{{{ random-bytes
(define-generator-generic random-bytes (generator bytes))

(defmethod random-bytes% ((generator basic-generator) (bytes integer))
  ;; (declare (optimize speed))
  (let ((chunk (bytes generator)))
    (cond ((= bytes chunk)
           (random-byte% generator))
          ((< bytes chunk)
           (truncate-bits (random-byte% generator) bytes))
          (T
           (let ((random 0))
             ;; Fill upper bits
             (loop for i downfrom (- bytes chunk) above 0 by chunk
                   for byte = (random-byte% generator)
                   do (setf (ldb (byte chunk i) random) byte))
             ;; Fill lowermost bits.
             ;; This will cause spilling on misaligned boundaries, but we don't care.
             (setf (ldb (byte chunk 0) random) (random-byte% generator))
             random)))))
;;;}}}

;;;{{{ random unit
(define-generator-generic random-unit (generator))

(defmethod random-unit% ((generator basic-generator))
  ;; (declare (optimize speed))
  (let* ((bits #.(integer-length most-positive-fixnum))
         (random (random-bytes% generator bits)))
    (/ (float (the (integer 0) random) 0.0d0) most-positive-fixnum)))
;;;}}}

;;;{{{ random-float
(define-generator-generic random-float (generator from to))

(defmethod random-float% :around ((generator basic-generator) (from real) (to real))
  ;; (declare (optimize speed))
  (if (< from to)
      (call-next-method)
      (call-next-method generator to from)))

(defmethod random-float% ((generator basic-generator) (from real) (to real))
  ;; (declare (optimize speed))
  (+ from (* (- to from) (random-unit% generator))))
;;;}}}

;;;{{{ random-int
(define-generator-generic random-int (generator from to))

(defmethod random-int% :around ((generator basic-generator) (from integer) (to integer))
  ;; (declare (optimize speed))
  (if (< from to)
      (call-next-method)
      (call-next-method generator to from)))

(defmethod random-int% ((generator basic-generator) (from integer) (to integer))
  ;; (declare (optimize speed))
  (cond ((and (typep from 'fixnum) (typep to 'fixnum))
         (let* ((range (- to from))
                (bits (integer-length range))
                (random (random-bytes% generator bits)))
           (declare (type (integer 0) random range))
           (+ from
              (if (= 0 (logand range (1+ range)))
                  random
                  (round (* range (/ random (ash 1 bits))))))))))
;;;}}}

;;;{{{ reseed
(define-generator-generic reseed (generator &optional new-seed))

(defmethod reseed% :around ((generator basic-generator) &optional new-seed)
  (let ((seed (or new-seed (hopefully-sufficiently-random-seed))))
    (set-seed seed generator)
    (call-next-method generator seed))
  generator)
;;;}}}


;;;;;}}}


;;;;;{{{ random number generators

(defmacro new-instance-of (class)
  `(let* ((result (make-instance ',class :seed 1)))
     (reseed% result)
     ;; return result
     result))

;;;{{{ linear congruence
(defclass linear-congruence-class (basic-generator)
  ((state :accessor state)
   (multiplier :initarg :multiplier :reader multiplier :writer set-multiplier)
   (increment :initarg :increment :reader increment :writer set-increment)
   (bytes :initarg :bytes :writer set-bytes))
  (:default-initargs
   :bytes 64
   :multiplier 6364136223846793005
   :increment 1442695040888963407))

(defmethod reseed% ((generator linear-congruence-class) &optional new-seed)
  (setf (state generator) (mod new-seed (1- (ash 1 (bytes generator))))))

(defmethod random-byte% ((generator linear-congruence-class))
  (let ((c (increment generator))
        (a (multiplier generator))
        (x (state generator))
        (b (bytes generator)))
    ;(declare (optimize speed) (type integer b c a x))
    (let ((new (mod (+ c (* x a)) (1- (ash 1 b)))))
      (setf (state generator) new)
      new)))

(defparameter linear-congruence (new-instance-of linear-congruence-class))
;;;}}}

;;;{{{ mersenne-twister
(defclass mersenne-twister (basic-generator)
  ((n :initarg :n :reader n)
   (m :initarg :m :reader m)
   (upper :initarg :upper :reader upper)
   (lower :initarg :lower :reader lower)
   (matrix :initarg :matrix :reader matrix :writer set-matrix)
   (index :initarg :index :accessor index)
   (magic :initarg :magic :reader magic)
   (shiftops :initarg :shiftops :reader shiftops)))

(defmethod reseed% :after ((generator mersenne-twister) &optional new-seed)
  (declare (ignore new-seed))
  (setf (index generator) (n generator)))

(defmacro %inner-mersenne-twister (bytes)
  `(let ((i 0)
         (n (n generator))
         (m (m generator))
         (upper (upper generator))
         (lower (lower generator))
         (matrix (matrix generator))
         (magic (magic generator))
         (shiftops (shiftops generator)))
     ;; (declare (optimize speed)
     ;;          (ftype (function (mersenne-twister) (unsigned-byte 16)) index)
     ;;          (type (simple-array (unsigned-byte ,bytes)) matrix magic)
     ;;          (type (unsigned-byte 16) n m i))
     (flet ((magic (i) (aref magic i))
            (matrix (i) (aref matrix i)))
       ;; (declare (inline magic matrix))
       (when (= (the integer (index generator)) n)
         (loop while (< i (- n m))
               for x = (logior (logand (matrix i) upper)
                               (logand (matrix (1+ i)) lower))
               do (setf (aref matrix i)
                        (logxor (matrix (+ i m))
                                (ash x -1)
                                (magic (mod x 2))))
                  (incf i))
         (loop while (< i (1- n))
               for x = (logior (logand (matrix i) upper)
                               (logand (matrix (1+ i)) lower))
               do (setf (aref matrix i)
                        (logxor (matrix (+ i (- m n)))
                                (ash x -1)
                                (magic (mod x 2))))
                  (incf i))
         (setf (index generator) 0))
       (let ((result (matrix (index generator))))
         ;; (declare (type (unsigned-byte ,bytes) result))
         (setf (index generator) (the fixnum (1+ (index generator))))
         (loop for (shift mask) across shiftops
               do (setf result (logxor result
                                       (logand (ash (the (unsigned-byte ,bytes) result)
                                                    (the (signed-byte 16) shift))
                                               (the (unsigned-byte ,bytes) mask)))))
         result))))

(defclass mersenne-twister-32-class (mersenne-twister)
  ((bytes :initform 32))
  (:default-initargs
   :n 624
   :m 397
   :upper #x80000000
   :lower #x7fffffff
   :magic (barr 32 0 #x9908b0df)
   :shiftops #((-11 #xFFFFFFFF)
               (  7 #x9D2C5680)
               ( 15 #xEFC60000)
               (-18 #xFFFFFFFF))))

(defmethod reseed% ((generator mersenne-twister-32-class) &optional new-seed)
  (set-matrix (32bit-seed-array (n generator) new-seed) generator))

(defmethod random-byte% ((generator mersenne-twister-32-class))
  (%inner-mersenne-twister 32))

(defclass mersenne-twister-64-class (mersenne-twister)
  ((bytes :initform 64))
  (:default-initargs
   :n 312
   :m 156
   :upper #xFFFFFFFF80000000
   :lower #x000000007FFFFFFF
   :magic (barr 64 0 #xB5026F5AA96619E9)
   :shiftops #1A((-29 #x5555555555555555)
                 ( 17 #x71D67FFFEDA60000)
                 ( 37 #xFFF7EEE000000000)
                 (-43 #xFFFFFFFFFFFFFFFF))))

(defmethod reseed% ((generator mersenne-twister-64-class) &optional new-seed)
  (set-matrix (64bit-seed-array (n generator) new-seed) generator))

(defmethod random-byte% ((generator mersenne-twister-64-class))
  (%inner-mersenne-twister 64))

(defparameter mersenne-twister-32 (new-instance-of mersenne-twister-32-class))
(defparameter mersenne-twister-64 (new-instance-of mersenne-twister-64-class))
(defparameter mt32 (new-instance-of mersenne-twister-32-class))
(defparameter mt64 (new-instance-of mersenne-twister-64-class))

;;;}}}

;;;{{{ middle-square
(defclass middle-square-class (basic-generator)
  ((bytes :writer set-bytes :initform 32)
   (state :accessor state)))

(defmethod reseed% ((generator middle-square-class) &optional new-seed)
  (setf (state generator) new-seed)
  (set-bytes (integer-length new-seed) generator))

(defmethod random-byte% ((generator middle-square-class))
  (let* ((digits (bytes generator))
         (square (expt (state generator) 2))
         (offset (floor (/ (max 0 (- (integer-length square) digits)) 2)))
         (new (ldb (byte digits offset) square)))
    (setf (state generator) new)
    (values new (bytes generator))))

(defparameter middle-square (new-instance-of middle-square-class))

;;;}}}

;;;{{{ pcg
(defclass pcg-class (basic-generator)
  ((state :initarg :state :accessor state)
   (inc :initarg :inc :accessor inc)
   (bytes :initform 32))
  (:default-initargs
   :state #x853c49e6748fea9b
   :inc #xda3e39cb94b95bdb))

(defmethod reseed% ((generator pcg-class) &optional new-seed)
  (setf (state generator) 0)
  (setf (inc generator) (truncate64 (logior 1 (ash new-seed 1))))
  (random-byte% generator)
  (setf (state generator) (truncate64 (+ (state generator) new-seed)))
  (random-byte% generator))

(defmethod random-byte% ((generator pcg-class))
  ;; (declare (optimize speed))
  (let ((oldstate (the (unsigned-byte 64) (state generator))))
    (setf (state generator) (truncate64
                             (+ (truncate64 (* oldstate #x6364136223846793005))
                                (the (unsigned-byte 64) (inc generator)))))
    (let* ((xorshifted (ash (logxor (ash oldstate -18) oldstate) -27))
           (rot (ash oldstate -59)))
      (truncate32
       (logior (ash xorshifted (- rot))
               (ash xorshifted (logand (- rot) 31)))))))

(defparameter pcg (new-instance-of pcg-class))
;;;}}}

;;;{{{ tt800
(defclass tt800-class (basic-generator)
  ((magic :initform (barr 32 0 #x8ebfd028) :reader magic)
   (shiftops :initform #((  7 #x2b5b2500)
                         ( 15 #xdb8b0000)
                         (-16 #xffffffff)) :reader shiftops)
   (n :initform 25 :reader n)
   (m :initform 7 :reader m)
   (index :initform 0 :accessor index)
   (matrix :initform NIL :reader matrix :writer set-matrix)
   (bytes :initform 32)))

(defmethod reseed% ((generator tt800-class) &optional new-seed)
  (set-matrix (32bit-seed-array (n generator) new-seed) generator)
  (setf (index generator) (n generator)))

(defmethod random-byte% ((generator tt800-class))
  (let ((i 0)
        (n (n generator))
        (m (m generator))
        (matrix (matrix generator))
        (magic (magic generator))
        (shiftops (shiftops generator)))
    ;; (declare (optimize speed)
    ;;          (ftype (function (tt800) (unsigned-byte 8)) index)
    ;;          (type (simple-array (unsigned-byte 32)) matrix magic)
    ;;          (type (simple-array list 1) shiftops)
    ;;          (type (unsigned-byte 8) n m i))
    (flet ((matrix (n) (aref matrix n))
           (magic (n) (aref magic n)))
      (declare (inline matrix magic))
      (when (= (the integer (index generator)) n)
        (loop while (< i (- n m))
              do (setf (aref matrix i)
                       (logxor (matrix (+ i m))
                               (ash (matrix i) -1)
                               (magic (mod (matrix i) 2))))
                 (incf i))
        (loop while (< i n)
              do (setf (aref matrix i)
                       (logxor (matrix (+ i (- m n)))
                               (ash (matrix i) -1)
                               (magic (mod (matrix i) 2))))
                 (incf i))
        (setf (index generator) 0))
      (let ((result (matrix (index generator))))
        (declare (type (unsigned-byte 32) result))
        (incf (index generator))
        (loop for (shift mask) across shiftops
              do (setf result (logxor result (logand (ash result (the (signed-byte 6) shift))
                                                     (the (unsigned-byte 32) mask)))))
        result))))

(defparameter tt800 (new-instance-of tt800-class))
;;;}}}

;;;;;}}}


;;;{{{ initialize *current-generator*

;; (setf *current-generator* (new-instance-of linear-congruence-class))
(setf *current-generator* (new-instance-of mersenne-twister-64-class))
;;;}}}

;;;{{{ random "things"
(defun random-int-matrix (rows columns from to)
  "Returns a random matrix with the given rows and columns. Each element in the range from to."
  (let* ((result (make-array (list rows columns) :initial-element 0)))
    (dotimes (i rows)
      (dotimes (j columns)
        (setf (aref result i j) (random-int from to))))
    ;; return result
    result))
;;;}}}

;;;{{{ shadowing cl::random

(defgeneric random-any (min max)
  (:documentation "A function to hide the random-int/float functions, and to make them similar to the original cl::random in the sense that the max value is never reached."))

(defmethod random-any ((min integer) (max integer))
  (random-int min (1- max)))

(defmethod random-any ((min float) (max t))
  (random-float min max))

(defmethod random-any ((min t) (max float))
  (random-float min max))

(defun random (max &optional (min 0))
  (random-any (min min max) (max min max)))
;;;}}}

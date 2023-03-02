(in-package :vrp)

(defparameter *vrp-unit-testing-indent* 3
  "The indentation that should be used when printing the results of the tests.")

(defparameter *vrp-unit-testing-display-results* t
  "Whether or not to print the result of the tests.")

(defparameter *vrp-unit-testing-display-output* t
  "Whether or not to print the output information of the tests.")

(defmacro check-t (form)
  `(let* ((result ,form)
          (display-form ',form))
     (progn
       (if *vrp-unit-testing-display-results*
           (format t "~vt~:[FAIL~;pass~]"
                   *vrp-unit-testing-indent*
                   result))
       (if *vrp-unit-testing-display-output*
           (format t " ... Check ~a:  ~30t~a~%"
                   T display-form))
       (if (and *vrp-unit-testing-display-results*
                (not *vrp-unit-testing-display-output*))
           (format t "~%"))
       result)))

(defmacro check-non-nil (form)
  `(let* ((result ,form))
     (progn
       (if *vrp-unit-testing-display-output*
           (format t "~vt~:[FAIL~;pass~] ... Check non NIL:   ~30t~a~%"
                *vrp-unit-testing-indent*
                result ',form))
       result)))

(defmacro check-nil (form)
  `(let* ((result (not ,form))
          (display-form ',form))
     (progn
       (if *vrp-unit-testing-display-results*
           (format t "~vt~:[FAIL~;pass~]"
                   *vrp-unit-testing-indent*
                   result))
       (if *vrp-unit-testing-display-output*
           (format t " ... Check ~a:  ~30t~a~%"
                   NIL display-form))
       (if (and *vrp-unit-testing-display-results*
                (not *vrp-unit-testing-display-output*))
           (format t "~%"))
       result)))

(defmacro make-check (name operator)
  (let* ((macro-name (symb 'check- name)))
    `(defmacro ,macro-name (value form)
         `(let* ((result (,',operator ,value ,form))
                 (display-form ',form))
           (progn
             (if *vrp-unit-testing-display-results*
                 (format t "~vt~:[FAIL~;pass~]"
                         *vrp-unit-testing-indent*
                         result))
             (if *vrp-unit-testing-display-output*
                 (format t " ... Check ~a:  ~30t~a~%"
                         ,value display-form))
             (if (and *vrp-unit-testing-display-results*
                      (not *vrp-unit-testing-display-output*))
                 (format t "~%"))
             result)))))

(make-check = =)

(make-check eq eq)

(make-check str= string=)

(make-check list= obj=)

(make-check obj= obj=)

(defmacro deftests (name &body tests)
  (let* ((test-code (loop for test in tests
                          collect `(setf result (and ,test result)))))
    `(let* ((result t))
       (if *vrp-unit-testing-display-output*
           (format t "~%~a:~%" ,name))
       (progn
         ,@test-code)
       result)))
